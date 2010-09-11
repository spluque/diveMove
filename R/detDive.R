## $Id$

".labDive" <- function(act, string)
{
    ## Value: Label dives along vector of same length as input.  Return a
    ## matrix labelling each dive and postdive reading
    ## --------------------------------------------------------------------
    ## Arguments: act=factor with values to label, string=character string
    ## to search in act to be labelled sequentially
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    dive <- vector(mode="numeric", length=length(act))
    dive[act == string] <- 1
    runs <- rle(dive)
    rawid <- rep(seq(along=runs$lengths), runs$lengths)

    diveid <- rawid
    diveid[dive == 0] <- 0             # non-dives are 0, and dives conseq:
    diveid[dive != 0] <- rep(seq(along=table(diveid)[-1]), table(diveid)[-1])
    pdid <- numeric(length(rawid))      # dives are 0 and postdives conseq:
    pdinds <- rawid %in% (unique(rawid[dive == 1]) + 1)
    pdid[pdinds] <- rep(seq(along=table(rawid[pdinds])), table(rawid[pdinds]))
    cbind(dive.id=diveid, postdive.id=pdid)
}


".detDive" <- function(zdepth, act, dive.thr)
{
    ## Value: A data frame; detecting dives, using a depth threshold
    ## --------------------------------------------------------------------
    ## Arguments: zdepth=depth vector of zoc'ed data, act=factor with
    ## land/sea activity IDs (2nd element returned by detPhase), with
    ## values "W" for at-sea, dive.thr=dive threshold in m ...=sampling
    ## interval in (s), to pass to labDive
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## Get the indices of below surface activity and label as "U"
    underw <- which(act == "W" & zdepth > 0)
    act[underw] <- "U"

    ## Label underwater excursions
    labuw <- diveMove:::.labDive(act, "U")
    ## Max depth of each "U" phase
    uwmax <- tapply(zdepth[underw], labuw[underw, 1], max, na.rm=TRUE)
    ## Change each "U" (underwater) phase to "D" (diving) if its max depth
    ## > dive threshold; i.e. we assume a dive started when depth dipped
    ## below zero and ended when it returned to zero *if the maximum depth
    ## of such a phase exceeded the dive threshold*.  This is a problem if
    ## we have noise above the dive threshold, so we need to switch the
    ## observations <= surface back to "U" and relabel the activity vector
    ## accordingly (this works great!)
    act[labuw[, 1] %in% as.numeric(names(uwmax[uwmax > dive.thr]))] <- "D"

    dives.maybe <- diveMove:::.labDive(act, "D")
    surface.idx <- which(dives.maybe[, 1] > 0 & zdepth <= dive.thr)
    act[surface.idx] <- "U"
    inddive <- diveMove:::.labDive(act, "D")
    ndives <- length(unique(inddive[act == "D", 1]))
    message(ndives, " dives detected")

    ## Return data frame with vectors of dive indices, adjusted activity,
    ## and postdive indices
    data.frame(dive.id=inddive[, 1], dive.activity=act,
               postdive.id=inddive[, 2])
}

##_+ Dive Detection with smoothing spline and derivative
".cutDive" <- function(x, smooth.par, knot.factor)
{
    ## Value: Factor that breaks a dive into descent, descent/bottom,
    ## bottom, bottom/ascent, ascent, and/or descent/ascent given a
    ## proportion of maximum depth for bottom time.  Return a character
    ## matrix with orig ID and corresponding label.
    ## --------------------------------------------------------------------
    ## Arguments: x=a 3-col matrix with index in original TDR object and
    ## non NA depths.  A single dive's data; smooth.par=spar parameter for
    ## smooth.spline(); knot.factor=numeric scalar that multiplies the
    ## duration of the dive (used to construct the time predictor for the
    ## derivative)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## If this is a single observation, return as "DA" and NULLs
    if (nrow(x) < 2) {
        return(list(label.matrix=cbind(rowids=x[, 1], labs="DA"),
                    dive.spline=NULL, spline.deriv1=NULL, descent.crit=NULL,
                    ascent.crit=NULL))
    }
    depths <- x[, 2]
    times <- x[, 3]
    if (nrow(x) >= 4) { # guard against smooth.spline() limitations
        times.scaled <- as.numeric(times) - as.numeric(times[1])
    } else {
        times.scaledOrig <- as.numeric(times) - as.numeric(times[1])
        times4 <- seq(times[1], times[length(times)], length.out=4)
        times4.scaled <- as.numeric(times4) - as.numeric(times4[1])
        depths.itpfun <- approxfun(times.scaledOrig, depths)
        depths <- depths.itpfun(times4.scaled)
        times.scaled <- seq(times.scaledOrig[1],
                            times.scaledOrig[length(times.scaledOrig)],
                            length.out=4)
    }
    times.pred <- seq(times.scaled[1], times.scaled[length(times.scaled)],
                      length.out=length(times.scaled) * knot.factor)
    depths.smooth <- stats::smooth.spline(times.scaled, depths,
                                          spar=smooth.par, all.knots=TRUE)
    depths.deriv <- predict(depths.smooth, times.pred, deriv=1)
    depths.s <- depths.deriv$y

    ## Descent ------------------------------------------------------------
    Dd1pos.l <- rle(sign(depths.s))$lengths[1]
    Dd1pos.min <- which.min(depths.s[seq(Dd1pos.l)])
    Dd1pos.crit <- which.min(abs(times.pred[Dd1pos.min] - times.scaled))

    ## Ascent -------------------------------------------------------------
    ## Length of the sequence of negative derivative at the end of the
    ## predicted smooth depths (from end)
    Ad1neg.l <- rle(sign(rev(depths.s)))$lengths[1]
    ## Position of the *first* maximum negative derivative in the sequence
    ## at the end of the predicted smooth depths (from end)
    Ad1neg.max <- which.max(rev(depths.s)[seq(Ad1neg.l)])
    ## Absolute differences between the time predictor corresponding to the
    ## position above and scaled time (it's important it's in reverse, so
    ## that we can find the first minimum below)
    Ad1neg.ad <- abs(rev(times.pred)[Ad1neg.max] - rev(times.scaled))
    ## Position of the *first* minimum absolute difference above
    Ad1neg.crit <- length(times.scaled) - which.min(Ad1neg.ad) + 1

    ## Correct both descent and ascent critical indices if this is a brief
    ## dive
    if (nrow(x) < 4) {
        Dd1pos.crit <- which.min(abs(times.scaled[Dd1pos.crit] -
                                     times.scaledOrig))
        Ad1neg.crit <- which.min(abs(times.scaled[Ad1neg.crit] -
                                     times.scaledOrig))
    }

    descind <- seq(Dd1pos.crit)
    ascind <- seq(Ad1neg.crit, nrow(x))

    ## Bottom -------------------------------------------------------------
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
    label.mat <- cbind(rowids, labs)
    list(label.matrix=label.mat, dive.spline=depths.smooth,
         spline.deriv1=depths.deriv, descent.crit=Dd1pos.crit,
         ascent.crit=Ad1neg.crit)
}

".labDivePhase" <- function(x, diveID, ...)
{
    ## Value: A factor labelling portions of dives
    ## --------------------------------------------------------------------
    ## Arguments: x=class TDR object, diveID=numeric vector indexing each
    ## dive (non-dives should be 0). As it is called by calibrateDepth,
    ## these indices include underwater phases, not necessarily below dive
    ## threshold. ...=arguments passed to .cutDive(), usually from
    ## calibrateDepth() and include 'smooth.par' and 'knot.factor'.
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
        perdivetd <- lapply(by(td, dids, diveMove:::.cutDive, ...), "[")
        labdF <- do.call(rbind, lapply(perdivetd, function(p) {
            p["label.matrix"][[1]]
        }))
        ff <- factor(rep("X", length(diveID)),
                     levels=c(unique(labdF[, 2]), "X"))
        ff[as.numeric(labdF[, 1])] <- labdF[, 2]
        list(phase.labels=ff, phase.models=perdivetd)
    } else {
        warning("no dives were found in x")
        list(phase.labels=factor(rep("X", length(diveID))),
             phase.models=NULL)
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
    okl <- pmax(1, setdiff(ok - 1, ok))
    okr <- pmin(length(diveID), setdiff(ok + 1, ok))
    sort(c(okl, ok, okr))               # add the surface points
}


## TEST ZONE --------------------------------------------------------------
