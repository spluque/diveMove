"diveStats" <- function(x)
{
    ## Value: A data frame with per-dive statistics
    ## --------------------------------------------------------------------
    ## Arguments: x=object of class TDRcalibrate
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDRcalibrate")) stop("x must be a TDRcalibrate object")
    zvtdr <- getTDR(x)                     # fully calibrated object
    interval <- getDtime(zvtdr)            # sampling interval
    diveid <- getDAct(x, "dive.id")        # dive IDs
    postdiveid <- getDAct(x, "postdive.id")          # postdive IDs
    ok <- which(diveid > 0 & diveid %in% postdiveid) # diving subscripts
    dtimes <- getTime(zvtdr)[ok]                     # diving times
    ddepths <- getDepth(zvtdr)[ok]                   # diving depths
    dids <- diveid[ok]                               # dive IDs
    dphases <- getDPhaseLab(x)[ok]                   # dive phase labels
    okpd <- which(postdiveid %in% unique(dids)) # postdive subscripts
    pdtimes <- getTime(zvtdr)[okpd]          # required postdive times
    pddepths <- getDepth(zvtdr)[okpd]        # required postdive depths
    pdids <- postdiveid[okpd]                # required postdive IDs

    postdive.dur <- tapply(pdtimes, pdids, function(k) {
        difftime(k[length(k)], k[1], units="secs")
    })

    dtimestz <- attr(dtimes, "tzone")
    if (!is(zvtdr, "TDRspeed")) {
        td <- data.frame(dphases, dtimes, ddepths)
        perdive <- do.call(rbind, by(td, dids, oneDiveStats, interval=interval))
        res <- data.frame(perdive, postdive.dur)
        for (i in 1:3) res[, i] <- structure(res[, i],
                                             class=c("POSIXt", "POSIXct"),
                                             tzone=dtimestz)
    } else {
        dspeeds <- getSpeed(zvtdr)[ok]  # diving speeds
        td <- data.frame(dphases, dtimes, ddepths, dspeeds)
        perdive <- do.call(rbind, by(td, dids, oneDiveStats, interval=interval,
                                     speed=TRUE))
        ## for postdive total distance and mean speed
        ptd <- matrix(c(pdtimes, getSpeed(zvtdr)[okpd]), ncol=2)
        pdv <- do.call(rbind, by(ptd, pdids, diveMove:::.speedStats))
        res <- data.frame(perdive, postdive.dur, postdive.tdist=pdv[, 1],
                          postdive.mean.speed=pdv[, 2])
        for (i in 1:3) res[, i] <- structure(res[, i],
                                             class=c("POSIXt", "POSIXct"),
                                             tzone=dtimestz)
    }

    res
}
