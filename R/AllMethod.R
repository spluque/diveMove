## $Id$

###_ + Show and PlotTDR

###_  . TDR and TDRspeed
setMethod("show", signature=signature(object="TDR"),
          definition=function(object) {
              trange <- range(object@time)
              cat("Time-Depth Recorder data -- Class",
                  class(object), "object\n")
              cat("  Source File          : ", object@file, "\n",
                  sep="")
              cat("  Sampling Interval (s): ", object@dtime, "\n",
                  sep="")
              cat("  Number of Samples    : ", length(object@time), "\n",
                  sep="")
              cat("  Sampling Begins      : ",
                  paste(object@time[1]), "\n", sep="")
              cat("  Sampling Ends        : ",
                  paste(object@time[length(object@time)]), "\n", sep="")
              cat("  Total Duration (d)   : ",
                  difftime(trange[2], trange[1], units="days"), "\n", sep="")
              drange <- range(object@depth, na.rm=TRUE)
              cat("  Measured depth range : [",
                  drange[1], ", ", drange[2], "]\n", sep="")
              if (length(names(object@concurrentData)) > 0) {
                  cat("  Other variables      : ",
                      names(object@concurrentData), "\n")
              }
          })

setMethod("plotTDR", signature(x="TDR"),
          function(x, ...) {
              plotTD(getTime(x), getDepth(x), ...)
          })

setMethod("plotTDR", signature(x="TDRspeed"),
          function(x, concurVars, concurVarTitles, ...) {
              if (missing(concurVarTitles) && !missing(concurVars)) {
                  concurVarTitles <- colnames(concurVars)
              } else if (missing(concurVarTitles) && missing(concurVars)) {
                  concurVars <- NULL
                  concurVarTitles <- "speed (m/s)"
              }
              plotTD(getTime(x), getDepth(x),
                     concurVars=cbind(getSpeed(x), concurVars),
                     concurVarTitles=concurVarTitles, ...)
          })

###_  . TDRcalibrate
setMethod("show", signature=signature(object="TDRcalibrate"),
          definition=function(object) {
              mCall <- gsub(" = ", "=", gsub("^ +", "", deparse(object@call)))
              dry <- object@gross.activity$activity == "L"
              dd <- length(unique(object@gross.activity$ phase.id[dry]))
              wet <- object@gross.activity$activity == "W"
              wetz <- object@gross.activity$activity == "Z"
              ww <- length(unique(object@gross.activity$ phase.id[wet | wetz]))
              cat("Depth calibration -- Class", class(object), "object\n",
                  sep="")
              cat("  Call                          : ", mCall, "\n", sep="")
              cat("  Source file                   : ", object@tdr@file, "\n",
                  sep="")
              cat("  Containing TDR of class       : ", class(object@tdr),
                  "\n", sep="")
              cat("  Number of dry phases          : ", dd, "\n", sep="")
              cat("  Number of aquatic phases      : ", ww, "\n", sep="")
              cat("  Number of dives detected      : ",
                  max(object@dive.activity$dive.id, na.rm=TRUE), "\n", sep="")
              cat("  Dry threshold used (s)        : ", object@dry.thr, "\n",
                  sep="")
              cat("  Aquatic theshold used (s)     : ", object@wet.thr, "\n",
                  sep="")
              cat("  Dive threshold used (s)       : ", object@dive.thr,
                  sep="")
              if (is(object@tdr, "TDRspeed")) {
                  cat("\n  Speed calibration coefficients: a=",
                      format(object@speed.calib.coefs[1], digits=2), "; b=",
                      format(object@speed.calib.coefs[2], digits=2), "\n",
                      sep="")
              } else cat("\n", sep="")
          })

setMethod("plotTDR", signature(x="TDRcalibrate"),
          function(x, what=c("TDR", "phase.models"),
                   diveNo=seq(max(getDAct(x, "dive.id"))),
                   labels="phase.id", concurVars, surface=FALSE, ...) {
              what <- match.arg(what)
              diveids <- getDAct(x, "dive.id")
              tdr <- getTDR(x)
              switch(what,
                     TDR = {
                         if (max(unique(diveids)) < 1) {
                             ok <- seq(along=slot(tdr, "depth"))
                         } else if (surface) {
                             dives <- diveids %in% diveNo
                             postdiveids <- getDAct(x, "postdive.id")
                             postdives <- postdiveids %in% diveNo
                             ok <- which(dives | postdives)
                         } else ok <- diveMove:::.diveIndices(diveids, diveNo)
                         newtdr <- tdr[ok]
                         switch(labels,
                                phase.id = {
                                    labs <- as.factor(getGAct(x, "phase.id")[ok])
                                },
                                dive.phase = {labs <- getDPhaseLab(x)[ok]})
                         labs <- factor(as.character(labs))
                         if (!missing(concurVars)) {
                             if (!is.character(concurVars))
                                 stop("concurVars must be of class character")
                             ccd <- getCCData(tdr, concurVars)[ok, , drop=FALSE]
                             plotTDR(newtdr, concurVars=ccd, phase.factor=labs, ...)
                         } else plotTDR(newtdr, phase.factor=labs, ...)
                     },
                     phase.models = {
                         if (length(diveNo) != 1)
                             stop("Only one dive's phase model can be plotted")
                         phaseM <- getDPhaseModel(x, diveNo)[[1]]
                         dive <- extractDive(x, diveNo)
                         times <- phaseM$dive.spline$data$x
                         depths <- -getDepth(dive)
                         stopifnot(!is.null(phaseM$dive.spline))
                         depths.s <- -phaseM$dive.spline$y
                         times.deriv1 <- phaseM$spline.deriv1$x
                         depths.deriv1 <- phaseM$spline.deriv1$y
                         d.crit <- phaseM$descent.crit
                         a.crit <- phaseM$ascent.crit
                         d.critq <- phaseM$descent.crit.q
                         a.critq <- phaseM$ascent.crit.q
                         descent.c1 <- times.deriv1 < times[d.crit]
                         descent.c2 <- depths.deriv1 > d.critq
                         descent <- descent.c1 & descent.c2
                         ascent.c1 <- times.deriv1 > times[a.crit]
                         ascent.c2 <- depths.deriv1 < a.critq
                         ascent <- ascent.c1 & ascent.c2
                         layout(matrix(1:2, ncol=1))
                         par(mar=c(3, 4, 0, 1) + 0.1, las=1)
                         plot(times, depths, type="o", axes=FALSE, pch=19,
                              cex=0.5, frame.plot=TRUE, ylab="Depth",
                              ylim=range(depths, depths.s))
                         axis(side=1)
                         axis(side=2, at=pretty(c(depths, depths.s)),
                              labels=rev(pretty(-c(depths, depths.s))), las=1)
                         lines(times, depths.s, lty=2, col="green")
                         lines(times[seq(d.crit)], depths[seq(d.crit)],
                               col="blue")
                         lines(times[a.crit:length(times)],
                               depths[a.crit:length(times)], col="lightblue")
                         legend("top", ncol=2, title=paste("Dive:", diveNo),
                                legend=c("original", "smoothed",
                                  "descent", "ascent"), lty=c(1, 2, 1, 1),
                                col=c("black", "green",
                                  "blue", "lightblue"), cex=0.7)
                         plot(times.deriv1, depths.deriv1, xlab="Time index",
                              ylab="First derivative", type="l", cex=0.3)
                         points(times.deriv1[descent], depths.deriv1[descent],
                                col="blue", cex=0.3)
                         points(times.deriv1[ascent], depths.deriv1[ascent],
                                col="lightblue", cex=0.3)
                         abline(h=c(d.critq, a.critq),
                                v=c(times[d.crit], times[a.crit]), lty=2)
                         text(0.8, c(d.critq, a.critq),
                              labels=c(expression(paste("descent ", hat(q))),
                                expression(paste("ascent ", hat(q)))),
                              pos=c(3, 1), cex=0.7)
                         text(c(times[d.crit], times[a.crit]), 0,
                              labels=c("descent", "ascent"), pos=1, cex=0.7)
                     })})


###_ + Accessors

###_  . TDR and TDRspeed
setMethod("getFileName", signature(x="TDR"), function(x) x@file)

if (getRversion() < "2.11.0") {
    .POSIXct <- function(xx, tz=NULL) {
        structure(xx, class=c("POSIXt", "POSIXct"), tzone=tz)
    }
}
setMethod("getTime", signature(x="TDR"),
          function(x) {
              xx <- x@time
              if (getRversion() >= "2.12.0") {
                  .POSIXct(unclass(xx), attr(xx, "tzone"))
              } else xx
          })

setMethod("getDepth", signature(x="TDR"), function(x) x@depth)

".speedCol" <- function(x)
{
    ## Value: column number where speed is located in x
    ## --------------------------------------------------------------------
    ## Arguments: x=data frame
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    dataNames <- names(x)
    colN <- dataNames %in% diveMove:::.speedNames
    if (length(which(colN)) != 1)
        stop("the column number for speed could not be determined")
    which(colN)
}
setMethod("getSpeed", signature(x="TDRspeed"), function(x) {
    ccData <- x@concurrentData
    speedCol <- diveMove:::.speedCol(ccData)
    ccData[, speedCol]
})

setMethod("getDtime", signature(x="TDR"), function(x) x@dtime)

## Get entire data frame
setMethod("getCCData", signature(x="TDR", y="missing"), function(x) {
    if (nrow(x@concurrentData) > 0) {
        x@concurrentData
    } else stop("No concurrent data are available")
})
## Get named component(s) of the data frame
setMethod("getCCData", signature(x="TDR", y="character"), function(x, y) {
    if (nrow(x@concurrentData) < 1) stop("No concurrent data are available")
    ccd <- getCCData(x)
    ccdnames <- names(ccd)
    ok <- ccdnames %in% y
    bady <- !y %in% ccdnames
    if (length(which(ok)) < 1) {
        stop(paste("y must contain at least one of the names of",
                   "the concurrent data frame"))
    } else if (any(bady)) {
        warning("components: ", y[bady], " could not be found and were ignored")
    }
    ccdf <- as.data.frame(ccd[, ok])
    names(ccdf) <- ccdnames[ok]
    ccdf
})

###_  . TDRcalibrate
setMethod("getTDR", signature(x="TDRcalibrate"), function(x) x@tdr)

## access the entire list
setMethod("getGAct", signature(x="TDRcalibrate", y="missing"),
          function(x) x@gross.activity)
## access only a named element
setMethod("getGAct", signature(x="TDRcalibrate", y="character"),
          function(x, y) x@gross.activity[[y]])

## access entire data frame
setMethod("getDAct", signature(x="TDRcalibrate", y="missing"),
          function(x) x@dive.activity)
## access only a certain column
setMethod("getDAct", signature(x="TDRcalibrate", y="character"),
          function(x, y) x@dive.activity[, y])

## access the entire factor
setMethod("getDPhaseLab", signature(x="TDRcalibrate", diveNo="missing"),
          function(x) x@dive.phases)
## access only those from certain dives
setMethod("getDPhaseLab", signature(x="TDRcalibrate", diveNo="numeric"),
          function(x, diveNo) {
              ctdr <- getTDR(x)
              phases <- x@dive.phases
              okpts <- diveMove:::.diveIndices(getDAct(x, "dive.id"), diveNo)
              phases[okpts]
          })

## access the entire factor
setMethod("getDPhaseModel", signature(x="TDRcalibrate", diveNo="missing"),
          function(x) x@phase.models)
## access only those from certain dives
setMethod("getDPhaseModel", signature(x="TDRcalibrate", diveNo="numeric"),
          function(x, diveNo) x@phase.models[diveNo])

setMethod("getSpeedCoef", signature(x="TDRcalibrate"),
          function(x) x@speed.calib.coefs)


###_ + Coercions and Replacements
setAs("TDR", "data.frame", function(from) {
    file.src <- from@file
    dtime <- from@dtime
    val <- data.frame(time=from@time, depth=from@depth, getCCData(from))
    attr(val, "file") <- file.src
    attr(val, "dtime") <- dtime
    val
})
setMethod("as.data.frame", signature("TDR"),
          function(x, row.names=NULL, optional=FALSE) {
              as(x, "data.frame")
          })

setAs("TDR", "TDRspeed", function(from) {
    new("TDRspeed", file=from@file, time=from@time, depth=from@depth,
        dtime=from@dtime, concurrentData=from@concurrentData)
})
setMethod("as.TDRspeed", signature("TDR"), function(x) as(x, "TDRspeed"))

setReplaceMethod("depth", signature(x="TDR", value="numeric"),
                 function(x, value) {
                     orig <- getDepth(x)
                     if (length(orig) != length(value))
                         stop(paste("replacement must have length:",
                                    length(orig),
                                    "(i.e. same length as original)"))
                     x@depth <- value
                     x
                 })

setReplaceMethod("speed", signature(x="TDRspeed", value="numeric"),
                 function(x, value) {
                     ccData <- x@concurrentData
                     speedCol <- diveMove:::.speedCol(ccData)
                     if (length(ccData[, speedCol]) != length(value))
                         stop(paste("replacement must have length:",
                                    length(ccData[, speedCol]),
                                    "(i.e. same length as original)"))
                     x@concurrentData[, speedCol] <- value
                     x
                 })

setReplaceMethod("ccData", signature(x="TDR", value="data.frame"),
                 function(x, value) {
                     ccDataN <- nrow(getCCData(x))
                     valueN <- nrow(value)
                     if (ccDataN != valueN)
                         stop(paste("replacement must have:", ccDataN,
                                    "rows (i.e. same as original)"))
                     x@concurrentData <- value
                     x
                 })

###_ + Subsetting
setMethod("[", signature("TDR"), function(x, i, j, ..., drop) {
    if (!missing(j) || !missing(...) || !missing(drop))
        stop("subsetting TDR objects can only be done on a single index")
    new(class(x), file=getFileName(x), dtime=getDtime(x), time=getTime(x)[i],
        depth=getDepth(x)[i],
        concurrentData=tryCatch(getCCData(x)[i, , drop=FALSE],
          error=function(k) data.frame()))
})


###_ + Generators and Summaries
"createTDR" <- function(time, depth, concurrentData=data.frame(),
                        speed=FALSE, dtime, file)
{
    ## Value: An object of TDR or TDRspeed class.  Useful to recreate
    ## objects once depth has been zoc'ed and speed calibrated for further
    ## analyses.
    ## --------------------------------------------------------------------
    ## Arguments: see class definitions
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (missing(dtime)) dtime <- diveMove:::.getInterval(time)
    if(speed) {
        new("TDRspeed", time=time, depth=depth, concurrentData=concurrentData,
            dtime=dtime, file=file)
    } else {
        new("TDR", time=time, depth=depth, concurrentData=concurrentData,
            dtime=dtime, file=file)
    }
}

setMethod("extractDive", signature(obj="TDR", diveNo="numeric",
                                   id="numeric"), # for TDR object
          function(obj, diveNo, id) {
              if (length(id) != length(getTime(obj))) {
                  stop ("id and obj must have equal number of rows")
              }
              okpts <- diveMove:::.diveIndices(id, diveNo)
              if (is(obj, "TDRspeed")) {
                  new("TDRspeed", time=getTime(obj)[okpts],
                      depth=getDepth(obj)[okpts],
                      concurrentData=getCCData(obj)[okpts, , drop=FALSE],
                      dtime=getDtime(obj), file=obj@file)
              } else {
                  new("TDR", time=getTime(obj)[okpts],
                      depth=getDepth(obj)[okpts],
                      concurrentData=getCCData(obj)[okpts, , drop=FALSE],
                      dtime=getDtime(obj), file=obj@file)
              }
          })

setMethod("extractDive",                # for TDRcalibrate
          signature(obj="TDRcalibrate", diveNo="numeric", id="missing"),
          function(obj, diveNo) {
              ctdr <- getTDR(obj)
              okpts <- diveMove:::.diveIndices(getDAct(obj, "dive.id"), diveNo)
              if (is(ctdr, "TDRspeed")) {
                  new("TDRspeed", time=getTime(ctdr)[okpts],
                      depth=getDepth(ctdr)[okpts],
                      concurrentData=getCCData(ctdr)[okpts, , drop=FALSE],
                      dtime=getDtime(ctdr), file=ctdr@file)
              } else {
                  new("TDR", time=getTime(ctdr)[okpts],
                      depth=getDepth(ctdr)[okpts],
                      concurrentData=getCCData(ctdr)[okpts, , drop=FALSE],
                      dtime=getDtime(ctdr), file=ctdr@file)
              }
          })

setMethod("timeBudget",            # a table of general attendance pattern
          signature(obj="TDRcalibrate", ignoreZ="logical"),
          function(obj, ignoreZ) {
              act <- getGAct(obj, "activity")
              tt <- getTime(getTDR(obj))
              interval <- getDtime(getTDR(obj))
              if (ignoreZ) {            # ignore the short baths
                  act[act == "Z"] <- "L"
                  attlist <- diveMove:::.rleActivity(tt, act, interval)
                  actlabel <- rle(as.vector(act))$values
                  tripno <- seq(along=actlabel)
              } else {                  # count the short baths
                  attlist <- getGAct(obj)
                  actlabel <- rle(as.vector(act))$values
                  tripno <- seq(along=actlabel)
              }
              data.frame(phaseno=tripno, activity=actlabel,
                         beg=attlist[[3]], end=attlist[[4]],
                         row.names=NULL)
          })

###_ + plotBouts
setMethod("plotBouts", signature(fit="nls"),
          function(fit, ...) {
              if (length(coef(fit)) != 4)
                  stop("fitted model must have exactly 4 coefficients")
              plotBouts2.nls(fit=fit, lnfreq=eval.parent(fit$data), ...)
          })
setMethod("plotBouts", signature(fit="mle"),
          function(fit, x, ...) {
              if (length(coef(fit)) != 3)
                  stop("fitted model must have exactly 3 coefficients")
              plotBouts2.mle(fit=fit, x=x, ...)
          })

###_ + Methods for bec2 are in bouts.R to avoid Collate issues in DESCRIPTION


###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
