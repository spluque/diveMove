".labDive" <- function(act, string, interval)
{
    ## Value: Label dives along vector of same length as input.  Return a
    ## matrix labelling each dive and postdive reading
    ## --------------------------------------------------------------------
    ## Arguments: act=factor with values to label, string=character string
    ## to search in act to be labelled sequentially, interval=sampling
    ## interval in the input
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    dive <- vector(mode="numeric", length=length(act))
    dive[act == string] <- 1
    runs <- rle(dive)
    rawid <- rep(seq(along=runs$lengths), runs$lengths)

    diveid <- rawid
    diveid[dive == 0] <- 0               # non-dive are 0, and dives conseq:
    diveid[dive != 0] <- rep(seq(along=table(diveid)[-1]), table(diveid)[-1])
    pdid <- numeric(length(rawid))        # dives are 0 and postdives conseq:
    pdinds <- rawid %in% (unique(rawid[dive == 1]) + 1)
    pdid[pdinds] <- rep(seq(along=table(rawid[pdinds])), table(rawid[pdinds]))
    cbind(dive.id=diveid, postdive.id=pdid)
}


".detDive" <- function(zdepth, act, dive.thr=4, ...)
{
    ## Value: A data frame; detecting dives, using a depth threshold
    ## --------------------------------------------------------------------
    ## Arguments: zdepth=depth vector of zoc'ed data, act=factor with
    ## land/sea activity IDs (2nd element returned by detPhase), with
    ## values "W" for at-sea, dive.thr=dive threshold in m/s ...=sampling
    ## interval in (s), to pass to labDive
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## Get the indices of below surface activity and label as "U"
    underw <- which(act == "W" & zdepth > 0)
    act[underw] <- "U"

    labuw <- diveMove:::.labDive(act, "U", ...) # label underwater excursions
    ## Max depth of each "U" phase
    uwmax <- tapply(zdepth[underw], labuw[underw, 1], max, na.rm=TRUE)
    ## Change each "U" phase to "D" if its max depth > dive threshold
    act[labuw[, 1] %in% as.numeric(names(uwmax[uwmax > dive.thr]))] <- "D"

    inddive <- diveMove:::.labDive(act, "D", ...)
    ndives <- length(unique(inddive[act == "D", 1]))
    message(ndives, " dives detected")

    ## Return data frame with vectors of dive indices, adjusted activity,
    ## and postdive indices
    data.frame(dive.id=inddive[, 1], dive.activity=act,
               postdive.id=inddive[, 2])
}

##_+ Dive Detection with quantiles of rates of descent/ascent -------------
".cutDive" <- function(x, descent.crit.q, ascent.crit.q, wiggle.tol)
{
    ## Value: Create a factor that breaks a dive into descent,
    ## descent/bottom, bottom, bottom/ascent, ascent, and/or
    ## descent/ascent given a proportion of maximum depth for bottom time.
    ## Return a character matrix with orig ID and corresponding label.
    ## --------------------------------------------------------------------
    ## Arguments: x=a 3-col matrix with index in original TDR object and
    ## non NA depths.  A single dive's data.
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    depths <- x[, 2]
    times <- x[, 3]
    "det.fun" <- function(x, y, index, ascent=FALSE) {
        bottom.depth <- y[index] * wiggle.tol
        if (ascent) {
            asc <- y[index:length(y)]
            diffx <- diff(x[index:length(x)])
            rate <- -diff(asc) / diffx
            crit.rate <- quantile(rate[rate > 0], ascent.crit.q)
            beyond <- which(rate > crit.rate)
            bott.wiggle <- rate <= 0 & asc[-1] > bottom.depth
            if (any(bott.wiggle)) {
                crit.id <- index + max(which(bott.wiggle))
            } else {
                crit.id <- ifelse(length(beyond) < 1, length(x),
                                  index + beyond[1] - 1)
            }
            crit.id:length(x)
        } else {
            desc <- y[1:index]
            diffx <- diff(x[1:index])
            rate <- diff(desc) / diffx
            crit.rate <- quantile(rate[rate > 0], descent.crit.q)
            low.rates <- which(rate < crit.rate)
            desc.wiggle <- rate <= 0 & desc[-length(desc)] < bottom.depth
            if (length(low.rates) > 0 & any(desc.wiggle)) {
                low.below <- setdiff(low.rates, which(desc.wiggle))
                crit.id <- ifelse(length(low.below) < 1, low.rates[1],
                                  low.below[1])
            } else if (length(low.rates) < 1) {
                crit.id <- length(rate)
            } else crit.id <- low.rates[1]
            1:crit.id
        }
    }

    ## Descent detection
    desc.maxdd.id <- which.max(depths)
    descind <- det.fun(times, depths, desc.maxdd.id, ascent=FALSE)

    ## Ascent detection
    depths.rev <- rev(depths)
    asc.maxdd.id <- (nrow(x) - which.max(depths.rev)) + 1
    ascind <- det.fun(times, depths, asc.maxdd.id, ascent=TRUE)

    ## Bottom detection
    bottind <- c(descind[length(descind)],
                 setdiff(seq(nrow(x)), union(descind, ascind)),
                 ascind[1])

    ## descent is everything in descind that's not in union of bottind and ascind
    d <- setdiff(descind, union(bottind, ascind))
    ## descent/bottom is what's common to descind and bottind
    db <- intersect(descind, bottind)
    ## bottom is everything in bottind that's not in union of descind and ascind
    b <- setdiff(bottind, union(descind, ascind))
    ## bottom/ascent is what's common to ascind and bottind
    ba <- intersect(ascind, bottind)
    ## ascent is everything in ascind that's not in union of descind and bottind
    a <- setdiff(ascind, union(descind, bottind))
    ## descent/ascent is what's common to descind and ascind
    da <- intersect(descind, ascind)

    labs <- character(nrow(x))
    labs[d] <- "D"
    labs[db] <- "DB"
    labs[b] <- "B"
    labs[ba] <- "BA"
    labs[a] <- "A"
    labs[da] <- "DA"
    ## If there are repetitions, keep the last one to avoid missing ascent labels
    rowids <- unique(c(x[d, 1], x[db, 1], x[b, 1], x[ba, 1], x[a, 1], x[da, 1]))
    cbind(rowids, labs)
}

".labDivePhase" <- function(x, diveID, descent.crit.q, ascent.crit.q, wiggle.tol)
{
    ## Value: A factor labelling portions of dives
    ## --------------------------------------------------------------------
    ## Arguments: x=class TDR object, diveID=numeric vector indexing each
    ## dive (non-dives should be 0)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDR")) stop("x must be a TDR object")
    ok <- which(diveID > 0 & !is.na(getDepth(x))) # required diving indices
    if (length(ok) > 0) {
        ddepths <- getDepth(x)[ok]               # diving depths
        dtimes <- getTime(x)[ok]                 # diving times
        dids <- diveID[ok]                       # dive IDs
        ## We send a matrix of indices, and non-NA depths and times
        td <- matrix(data=c(ok, ddepths, as.numeric(dtimes)), ncol=3)
        perdivetd <- by(td, dids, diveMove:::.cutDive, descent.crit.q,
                        ascent.crit.q, wiggle.tol)
        labdF <- do.call(rbind, perdivetd)
        ff <- factor(rep("X", length(diveID)),
                     levels=c(unique(labdF[, 2]), "X"))
        ff[as.numeric(labdF[, 1])] <- labdF[, 2]
        ff
    } else {
        warning("no dives were found in x")
        factor(rep("X", length(diveID)))
    }
}

".diveIndices" <- function(diveID, diveNo)
{
    ## Value: A numeric vector with the indices of dives (and their
    ## beginning/end indices) in diveID
    ## --------------------------------------------------------------------
    ## Arguments: diveID=numeric vector numbering all dives and non-dives,
    ## diveNo=numeric vector of unique dive indices to extract fromdiveID.
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    ok <- which(diveID %in% diveNo)
    okl <- setdiff(ok - 1, ok)
    okr <- setdiff(ok + 1, ok)
    sort(c(okl, ok, okr))               # add the surface points
}
