"oneDiveStats" <- function(x, interval, speed=FALSE)
{
    ## Value: A matrix with time/depth stats for each dive segment
    ## --------------------------------------------------------------------
    ## Arguments: x=a matrix with data for a single dive interval=sampling
    ## interval speed=logical; should we calculate speed stats?
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## define descent, bottom, and ascent submatrices
    desc <- x[grep("D", as.character(x[, 1])), 2:ncol(x)]
    bott <- x[grep("B", as.character(x[, 1])), 2:ncol(x)]
    asc <- x[grep("A", as.character(x[, 1])), 2:ncol(x)]

    ## DESCENT
    begdesc <- desc[1, 1]
    enddesc <- desc[nrow(desc), 1]
    desctim <- difftime(enddesc, begdesc, units="secs") + interval / 2
    descdist <- max(desc[, 2], na.rm=TRUE)
    ## BOTTOM
    if (nrow(bott) > 0) {
        botttim <- difftime(bott[nrow(bott), 1], bott[1, 1], units="secs")
        bottdist <- sum(abs(diff(bott[!is.na(bott[, 2]), 2])))
    }
    ## ASCENT
    begasc <- asc[1, 1]
    asctim <- difftime(asc[nrow(asc), 1], begasc, units="secs") + interval / 2
    ascdist <- max(asc[, 2], na.rm=TRUE)
    ## DIVE DURATION
    divetim <- ifelse(exists("botttim"),
                      desctim + botttim + asctim,
                      desctim + asctim)
    ## MAXIMUM DIVE DEPTH
    maxdep <- max(x[, 3], na.rm=TRUE)

    if (!speed) {
        cbind(begdesc=begdesc, enddesc=enddesc, begasc=begasc,
              desctim=desctim,
              botttim=ifelse(exists("botttim"), botttim, NA),
              asctim=asctim, descdist=descdist,
              bottdist=ifelse(exists("bottdist"), bottdist, NA),
              ascdist=ascdist, divetim=divetim, maxdep=maxdep)
    } else {
        descv <- diveMove:::.speedStats(desc[, -2], vdist=descdist)
        bottv <- diveMove:::.speedStats(bott[, -2])
        ascv <- diveMove:::.speedStats(asc[, -2], vdist=ascdist)
        cbind(begdesc=begdesc, enddesc=enddesc, begasc=begasc,
              desctim=desctim,
              botttim=if (exists("botttim")) botttim else NA,
              asctim=asctim, descdist=descdist,
              bottdist=if (exists("bottdist")) bottdist else NA,
              ascdist=ascdist, desc.tdist=descv[, 1],
              desc.mean.speed=descv[, 2], desc.angle=descv[, 3],
              bott.tdist=bottv[, 1], bott.mean.speed=bottv[, 2],
              asc.tdist=ascv[, 1], asc.mean.speed=ascv[, 2],
              asc.angle=ascv[, 3], divetim=divetim,
              maxdep=maxdep)
    }
}

## TEST ZONE ---------------------------------------------------------
