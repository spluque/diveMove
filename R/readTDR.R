## $Id: readTDR.R,v 1.1.1.1.2.1 2007-02-13 21:51:53 sluque Exp $

".getInterval" <- function(time)
{
    ## Value: numeric; the mode of intervals between time readings
    ## --------------------------------------------------------------------
    ## Arguments: time=POSIXct
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    stopifnot(all(!is.na(time)))
    if (length(time) < 2) {
        interval <- 0
    } else {
        tab <- table(difftime(time[-1], time[-length(time)],
                              units="secs", tz="GMT"))
        interval <- as.numeric(names(tab[which.max(tab)]))
    }
    interval
}

"readTDR" <- function(file, dateCol=1, timeCol=2, depthCol=3, speed=FALSE,
                      subsamp=5, concurrentCols=4:6,
                      dtformat="%d/%m/%Y %H:%M:%S", tz="GMT")
{
    ## Value: TDR or TDRspeed object from *.csv file
    ## --------------------------------------------------------------------
    ## Arguments: file=path to file to read; dateCol=col no. with date,
    ## timeCol=col no. with time, depthCol=col no. with depth,
    ## speedCol=col no. with speed, subsamp=subsample at this interval,
    ## dtformat=format to interpret the pasted date and time columns,
    ## tz=time zone to assume
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    srcfile <- basename(file)
    rawdat <- read.csv(file, header=TRUE, na.strings="", as.is=TRUE)
    names(rawdat) <- tolower(names(rawdat))
    rawdat.ncol <- seq(ncol(rawdat))
    dtpasted <- paste(rawdat[, dateCol], rawdat[, timeCol])
    datetime <- as.POSIXct(strptime(dtpasted, format=dtformat), tz=tz)
    origint <- diveMove:::.getInterval(datetime)
    if(!identical(all.equal(origint, subsamp), TRUE)) {
        steptim <- as.numeric((subsamp) / origint)
        stepind <- seq(from=1, to=length(datetime), by=round(steptim))
        datetime <- datetime[stepind]
        rawdat <- rawdat[stepind, ]
    }
    goodcc <- concurrentCols[is.finite(concurrentCols)]
    okconcurCols <- goodcc %in% rawdat.ncol
    allbadcc <- all(!okconcurCols)
    somebadcc <- any(!okconcurCols) && !allbadcc
    if (somebadcc) { # warn of no concurrent data later
        warning(paste("Columns", concurrentCols[!okconcurCols],
                      "given as concurrentCols could not be found\n"))
    }
    if (allbadcc && !is.null(concurrentCols)) {
        warning("None of the columns given as concurrentCols exist\n")
        tdr <- new("TDR", file=srcfile, time=datetime,
                   depth=rawdat[, depthCol],
                   dtime=diveMove:::.getInterval(datetime))
    } else {
        concurrentCols <- concurrentCols[okconcurCols]
        ccData <- as.data.frame(rawdat[, concurrentCols[okconcurCols]])
        names(ccData) <- names(rawdat)[concurrentCols[okconcurCols]]
        tdr <- new("TDR", file=srcfile, time=datetime,
                   depth=rawdat[, depthCol],
                   dtime=diveMove:::.getInterval(datetime),
                   concurrentData=ccData)
    }
    if (speed) as.TDRspeed(tdr) else tdr
}
