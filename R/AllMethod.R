
###_ + Show and plot

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

##' Methods for plotting objects of class "TDR" and "TDRcalibrate"
##'
##' Main plotting method for objects of these classes.  Plot and optionally
##' set zero-offset correction windows in \acronym{TDR} records, with the
##' aid of a graphical user interface (GUI), allowing for dynamic selection
##' of offset and multiple time windows to perform the adjustment.
##'
##' @aliases plotTDR
##' @param x \code{POSIXct} object with date and time, \code{\link{TDR}},
##'     or \code{\link{TDRcalibrate}} object.
##' @param y numeric vector with depth in m.
##' @param concurVars matrix with additional variables in each column to
##'     plot concurrently with depth.  For the (\code{TDR},\code{missing})
##'     and (\code{TDRcalibrate},\code{missing}) methods, a
##'     \code{\link{character}} vector naming additional variables from the
##'     \code{concurrentData} slot to plot, if any.
##' @param xlim \code{POSIXct} or numeric vector of length 2, with lower
##'     and upper limits of time to be plotted.
##' @param depth.lim numeric vector of length 2, with the lower and upper
##'     limits of depth to be plotted.
##' @param ylab.depth character string to label the corresponding y-axes.
##' @param concurVarTitles character vector of titles to label each new
##'     variable given in \var{concurVars}.
##' @param sunrise.time,sunset.time character string with time of sunrise
##'     and sunset, respectively, in 24 hr format.  This is used for
##'     shading night time.
##' @param night.col color for shading night time.
##' @param dry.time subset of time corresponding to observations considered
##'     to be dry.
##' @param phase.factor factor dividing rows into sections.
##' @param ... Arguments for the \code{(POSIXt,numeric)} method.  For
##'     \code{(TDRcalibrate,missing)}, these are arguments for the
##'     appropriate methods.
##' @param diveNo numeric vector or scalar with dive numbers to plot.
##' @param what character: what aspect of the \code{\link{TDRcalibrate}} to
##'     plot, which selects the method to use for plotting.
##' @return If called with the \code{interact} argument set to \code{TRUE},
##'     returns a list (invisibly) with as many components as sections of
##'     the record that were zero-offset corrected, each consisting of two
##'     further lists with the same components as those returned by
##'     \code{\link{locator}}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}, with many ideas
##'     from CRAN package sfsmisc.
##' @seealso \code{\link{calibrateDepth}}, \code{\link{.zoc}}
##' @keywords methods iplot
##' @describeIn plotTDR Base method plotting numeric vector against POSIXt
##'     object
##' @examples
##' \donttest{## Too long for checks
##'
##' ## Continuing the Example from '?calibrateDepth':
##' utils::example("calibrateDepth", package="diveMove",
##'                ask=FALSE, echo=FALSE, run.donttest=TRUE)
##' ## Use interact=TRUE (default) to set an offset interactively
##' ## Plot the 'TDR' object
##' plotTDR(getTime(divesTDR), getDepth(divesTDR))
##' plotTDR(divesTDR)
##'
##' ## Plot different aspects of the 'TDRcalibrate' object
##' plotTDR(dcalib)
##' plotTDR(dcalib, diveNo=19:25)
##' plotTDR(dcalib, what="dive.model", diveNo=25)
##' if (dev.interactive(orNone=TRUE)) {
##'     ## Add surface observations and interact
##'     plotTDR(dcalib, surface=TRUE)
##'     ## Plot one dive
##'     plotTDR(dcalib, diveNo=200)
##' }
##'
##' }
setMethod("plotTDR", signature(x="POSIXt", y="numeric"),
          function(x, y, concurVars=NULL, xlim=NULL, depth.lim=NULL,
                   ylab.depth="depth (m)",
                   concurVarTitles=deparse(substitute(concurVars)),
                   sunrise.time="06:00:00", sunset.time="18:00:00",
                   night.col="gray60", dry.time=NULL,
                   phase.factor=NULL) {
              stopifnot(identical(length(x), length(y)), is.vector(y))
              .plotlyTDR(time=x, depth=y, concurVars=concurVars,
                         xlim=xlim, depth.lim=depth.lim,
                         ylab.depth=ylab.depth,
                         concurVarTitles=concurVarTitles,
                         sunrise.time=sunrise.time,
                         sunset.time=sunset.time,
                         night.col=night.col, dry.time=dry.time,
                         phase.factor=phase.factor)
          })

##' @describeIn plotTDR Interactive graphical display of time-depth data,
##'     with zooming and panning capabilities.
setMethod("plotTDR", signature(x="TDR", y="missing"),
          function(x, y, concurVars, concurVarTitles, ...) {
              if (!missing(concurVars)) {
                  ccd <- getCCData(x, concurVars)
                  if (!missing(concurVarTitles)) {
                      lcvt <- length(concurVarTitles)
                      lcv <- length(concurVars)
                      stopifnot(identical(lcvt, lcv))
                  } else concurVarTitles <- colnames(ccd)
              } else if (missing(concurVars) && missing(concurVarTitles)) {
                  ccd <- concurVarTitles <- NULL
              }
              .plotlyTDR(time=getTime(x), depth=getDepth(x),
                         concurVars=ccd,
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
              cat("Depth calibration -- Class", class(object), "object\n")
              cat("  Call                              : ", mCall, "\n", sep="")
              cat("  Source file                       : ", object@tdr@file, "\n",
                  sep="")
              cat("  Containing TDR of class           : ", class(object@tdr),
                  "\n", sep="")
              cat("  Number of dry phases              : ", dd, "\n", sep="")
              cat("  Number of aquatic phases          : ", ww, "\n", sep="")
              cat("  Number of dives detected          : ",
                  max(object@dive.activity$dive.id, na.rm=TRUE), "\n", sep="")
              cat("  Dry threshold used (s)            : ", object@dry.thr, "\n",
                  sep="")
              cat("  Aquatic theshold used (s)         : ", object@wet.thr, "\n",
                  sep="")
              cat("  Dive threshold used (depth units) : ", object@dive.thr,
                  sep="")
              if (is(object@tdr, "TDRspeed")) {
                  cat("\n  Speed calibration coefficients    : a=",
                      format(object@speed.calib.coefs[1], digits=2), "; b=",
                      format(object@speed.calib.coefs[2], digits=2), "\n",
                      sep="")
              } else cat("\n", sep="")
          })

".plotTDRcalibratePhases" <- function(x, diveNo=seq(max(getDAct(x, "dive.id"))),
                                      concurVars, surface=FALSE, ...)
{
    if (!is(x, "TDRcalibrate")) stop("x must be a TDRcalibrate object")
    ell <- list(...)
    diveNo <- sort(diveNo)
    diveids <- getDAct(x, "dive.id")
    tdr <- getTDR(x)
    if (max(unique(diveids)) < 1) {
        ok <- seq(along=slot(tdr, "depth"))
    } else if (surface) {
        dives <- diveids %in% diveNo
        postdiveids <- getDAct(x, "postdive.id")
        postdives <- postdiveids %in% diveNo
        ok <- which(dives | postdives)
    } else ok <- .diveIndices(diveids, diveNo)
    newtdr <- tdr[ok]
    alltimes <- getTime(tdr)
    newtimes <- getTime(newtdr)
    times.ok <- alltimes >= newtimes[1] & alltimes <= newtimes[length(newtimes)]
    fulltimes <- alltimes[times.ok]
    labs <- getDPhaseLab(x)[ok]
    drys <- timeBudget(x, ignoreZ=TRUE)
    dry.time <- drys[drys[, 2] == "L", c(-1, -2)]
    ell$x <- newtdr
    ell$phase.factor <- labs
    if (length(dry.time) > 0L) ell$dry.time <- dry.time
    if (!missing(concurVars)) {
        if (!is.character(concurVars))
            stop("concurVars must be of class character")
        ell$concurVars <- concurVars
    }
    do.call(plotTDR, args=ell)
}

##' @describeIn plotTDR plot selected aspects of \code{\link{TDRcalibrate}}
##'     object.  Currently, two aspects have plotting methods:
##'
##' * \code{phases} (Optional arguments: \code{concurVars}, \code{surface})
##'   Plots all dives, labelled by the activity phase they belong to.  It
##'   produces a plot consisting of one or more panels; the first panel
##'   shows depth against time, and additional panels show other concurrent
##'   data in the object.  Optional argument \code{concurVars} is a
##'   character vector indicating which additional components from the
##'   \code{concurrentData} slot to plot, if any.  Optional argument
##'   \code{surface} is a logical: whether to plot surface readings.
##'
##' * \code{dive.model} Plots the dive model for the selected dive number
##'   (\code{diveNo} argument).
setMethod("plotTDR", signature(x="TDRcalibrate", y="missing"),
          function(x, y, what=c("phases", "dive.model"),
                   diveNo=seq(max(getDAct(x, "dive.id"))), ...) {
              what <- match.arg(what)
              switch(what,
                     phases = {
                         .plotTDRcalibratePhases(x, diveNo=diveNo, ...)
                     },
                     dive.model = { plotDiveModel(x, diveNo=diveNo) })
          })

###_  . diveModel
setMethod("show", signature=signature(object="diveModel"),
          definition=function(object) {
              ## Lots stolen from print.smooth.spline()
              digits <- getOption("digits")
              cat("Dive model -- Class",
                  class(object), "object\n")
              cat("Model --",
                  object@model, "\n")
              if (!is.null(cl <- object@dive.spline$call)) {
                  cat("Call:\n")
                  dput(cl, control=NULL)
              }
              if (object@model == "smooth.spline") {
                  ip <- object@dive.spline$iparms
                  cv <- cl$cv
                  if(is.null(cv)) cv <- FALSE else if(is.name(cv)) cv <- eval(cv)
                  cat("\nSmoothing Parameter  spar=",
                      format(object@dive.spline$spar, digits=digits),
                      " lambda=", format(object@dive.spline$lambda, digits=digits),
                      if (ip["ispar"] != 1L) {
                          paste("(", ip["iter"], " iterations)", sep="")
                      }, "\n", sep="")
                  cat("Equivalent Degrees of Freedom :",
                      format(object@dive.spline$df, digits=digits), "\n")
                  cat("Penalized Criterion           :",
                      format(object@dive.spline$pen.crit, digits=digits), "\n")
                  cat(ifelse(cv,
                             "PRESS                         : ",
                             "GCV                           : "),
                      format(object@dive.spline$cv.crit, digits=digits),
                      "\n", sep="")
              } else {                  # it's unimodal
                  cat("Optimal tuning parameter      : ",
                      format(object@dive.spline$lambda.opt, digits=digits),
                      "\n", sep="")
                  cat("Estimated residual variance   : ",
                      format(object@dive.spline$sigma, digits=digits),
                      "\n", sep="")
                  cat("Degree of spline              : ",
                      object@dive.spline$degree, "\n", sep="")
                  cat("Number of inner knots         : ",
                      object@dive.spline$g, "\n", sep="")
                  cat("Left boundary of domain       : ",
                      object@dive.spline$a, "\n", sep="")
                  cat("Right boundary of domain      : ",
                      object@dive.spline$b, "\n", sep="")
                  cat("Number of iterations          : ",
                      object@dive.spline$variter, "\n", sep="")
              }
              cat("Observed N                    : ",
                  nrow(object@label.matrix), "\n", sep="")
              cat("Modelled N                    : ",
                  length(object@dive.spline$data$x), "\n", sep="")
              cat("Modelled N (distinct)         : ",
                  length(object@dive.spline$x), "\n", sep="")
              cat("Derivative evaluated at       : ",
                  length(object@spline.deriv$x), " points", "\n", sep="")
              cat("Descent ends after            : ",
                  object@descent.crit, " steps in model", "\n", sep="")
              cat("Ascent begins after           : ",
                  object@ascent.crit, " steps in model", "\n", sep="")
              cat("Descent critical rate         : ",
                  object@descent.crit.rate, "\n", sep="")
              cat("Ascent critical rate          : ",
                  object@ascent.crit.rate, "\n", sep="")
          })

##' Methods for plotting models of dive phases
##'
##' All methods produce a double panel plot.  The top panel shows the depth
##' against time, the cubic spline smoother, the identified descent and
##' ascent phases (which form the basis for identifying the rest of the
##' dive phases), while the bottom panel shows the first derivative of the
##' smooth trace.
##'
##' @aliases plotDiveModel
##' @param x A \code{\link{diveModel}} (diveModel,missing method),
##'     \code{\link{numeric}} vector of time step observations
##'     (numeric,numeric method), or \code{\link{TDRcalibrate}} object
##'     (TDRcalibrate,numeric method).
##' @param diveNo integer representing the dive number selected for
##'     plotting.
##' @param y numeric vector with depth observations at each time step.
##' @param times.s numeric vector with time steps used to generate the
##'     smoothing spline (i.e. the knots, see \code{\link{diveModel}}).
##' @param depths.s numeric vector with smoothed depth (see
##'     \code{\link{diveModel}}).
##' @param d.crit integer denoting the index where descent ends in the
##'     observed time series (see \code{\link{diveModel}}).
##' @param a.crit integer denoting the index where ascent begins in the
##'     observed time series (see \code{\link{diveModel}}).
##' @param times.deriv numeric vector representing the time steps where the
##'     derivative of the smoothing spline was evaluated (see
##'     \code{\link{diveModel}}).
##' @param depths.deriv numeric vector representing the derivative of the
##'     smoothing spline evaluated at \code{times.deriv} (see
##'     \code{\link{diveModel}}).
##' @param d.crit.rate numeric scalar: vertical rate of descent
##'     corresponding to the quantile used (see \code{\link{diveModel}}).
##' @param a.crit.rate numeric scalar: vertical rate of ascent
##'     corresponding to the quantile used (see \code{\link{diveModel}}).
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{diveModel}}
##' @keywords methods
##' @describeIn plotDiveModel Given a \code{\link{diveModel}} object and
##'     (possibly) the dive number that it corresponds to, the plot shows
##'     the model data.
##' @examples
##' \donttest{## Too long for checks
##'
##' ## Continuing the Example from '?calibrateDepth':
##' utils::example("calibrateDepth", package="diveMove",
##'                ask=FALSE, echo=FALSE, run.donttest=TRUE)
##'
##' ## 'diveModel' method
##' dm <- getDiveModel(dcalib, 100)
##' plotDiveModel(dm, diveNo=100)
##'
##' ## 'TDRcalibrate' method
##' plotDiveModel(dcalib, diveNo=100)
##'
##' }
setMethod("plotDiveModel", signature(x="diveModel", y="missing"),
          function(x, diveNo) {
              if (missing(diveNo)) diveNo <- "Unknown"
              diveM <- x
              times <- diveM@dive.spline$data$x
              depths <- diveM@dive.spline$data$y
              times.s <- diveM@dive.spline$x
              depths.s <- diveM@dive.spline$y
              times.deriv <- diveM@spline.deriv$x
              depths.deriv <- diveM@spline.deriv$y
              d.crit <- diveM@descent.crit
              a.crit <- diveM@ascent.crit
              d.crit.rate <- diveM@descent.crit.rate
              a.crit.rate <- diveM@ascent.crit.rate
              plotDiveModel(x=times, y=depths, times.s=times.s,
                            depths.s=depths.s, d.crit=d.crit, a.crit=a.crit,
                            times.deriv=times.deriv,
                            depths.deriv=depths.deriv, diveNo=diveNo,
                            d.crit.rate=d.crit.rate, a.crit.rate=a.crit.rate)
          })

##' @describeIn plotDiveModel Given a \code{\link{TDRcalibrate}} object and
##'     a dive number to extract from it, this method plots the observed
##'     data and the model.  The intended use of this method is through
##'     \code{\link{plotTDR}} when \code{what="dive.model"}.
setMethod("plotDiveModel",
          signature(x="TDRcalibrate", y="missing"),
          function(x, diveNo) {
              if (length(diveNo) != 1L)
                  stop("Only one dive's phase model can be plotted")
              diveM <- getDiveModel(x, diveNo)
              dive <- extractDive(x, diveNo)
              times <- getTime(dive)
              times <- as.numeric(times - times[1])
              depths <- getDepth(dive)
              times.s <- diveM@dive.spline$x
              depths.s <- diveM@dive.spline$y
              times.deriv <- diveM@spline.deriv$x
              if (length(times) < 4L) {
                  ff <- times[length(times)] / times.s[length(times.s)]
                  times.s <- times.s * ff
                  times.deriv <- times.deriv * ff
              }
              depths.deriv <- diveM@spline.deriv$y
              d.crit <- diveM@descent.crit
              a.crit <- diveM@ascent.crit
              d.crit.rate <- diveM@descent.crit.rate
              a.crit.rate <- diveM@ascent.crit.rate
              plotDiveModel(x=times, y=depths, times.s=times.s,
                            depths.s=depths.s, d.crit=d.crit, a.crit=a.crit,
                            diveNo=diveNo, times.deriv=times.deriv,
                            depths.deriv=depths.deriv,
                            d.crit.rate=d.crit.rate, a.crit.rate=a.crit.rate)
          })

##' @describeIn plotDiveModel Base method, requiring all aspects of the
##'     model to be provided.
setMethod("plotDiveModel",
          signature(x="numeric", y="numeric"),
          function(x, y, times.s, depths.s, d.crit, a.crit, diveNo=1,
                   times.deriv, depths.deriv, d.crit.rate, a.crit.rate) {
              times <- x
              depths <- -abs(y)
              depths.s <- -abs(depths.s)
              descent.c1 <- times.deriv < times[d.crit]
              descent.c2 <- depths.deriv > d.crit.rate
              descent <- descent.c1 & descent.c2
              ascent.c1 <- times.deriv > times[a.crit]
              ascent.c2 <- depths.deriv < a.crit.rate
              ascent <- ascent.c1 & ascent.c2
              graphics::layout(matrix(1:2, ncol=1))
              old.par <- par(no.readonly=TRUE)
              on.exit(par(old.par))
              par(mar=c(3, 4, 0, 1) + 0.1, las=1)
              plot(times, depths, type="o", axes=FALSE, pch=19, cex=0.5,
                   frame.plot=TRUE, ylab="Depth",
                   ylim=range(depths, depths.s, na.rm=TRUE))
              axis(side=1)
              axis(side=2, at=pretty(c(depths, depths.s)),
                   labels=rev(pretty(-c(depths, depths.s))), las=1)
              lines(times.s, depths.s, lty=2, col="green")
              lines(times[seq(d.crit)], depths[seq(d.crit)], col="blue")
              lines(times[a.crit:length(x)],
                    depths[a.crit:length(x)], col="lightblue")
              legend("top", ncol=2, title=paste("Dive:", diveNo),
                     legend=c("original", "smoothed",
                       "descent", "ascent"), lty=c(1, 2, 1, 1),
                     col=c("black", "green",
                       "blue", "lightblue"), cex=0.7)
              plot(times.deriv, depths.deriv, xlab="Time index",
                   ylab="First derivative", type="l", cex=0.3)
              points(times.deriv[descent], depths.deriv[descent],
                     col="blue", cex=0.3)
              points(times.deriv[ascent], depths.deriv[ascent],
                     col="lightblue", cex=0.3)
              abline(h=c(d.crit.rate, a.crit.rate),
                     v=c(times[d.crit], times[a.crit]), lty=2)
              text(2, c(d.crit.rate, a.crit.rate),
                   labels=c(expression(paste("descent ", hat(q))),
                     expression(paste("ascent ", hat(q)))),
                   pos=c(3, 1), cex=0.7)
              text(c(times[d.crit], times[a.crit]), 0,
                   labels=c("descent", "ascent"), pos=1, cex=0.7)
          })

###_  . Bouts

##' Generalized log likelihood function taking any number of Poisson
##'     processes in a "broken-stick" model
##'
##' @aliases boutsNLSll
##' @param obj Object of class \code{\link{Bouts}} or numeric vector of
##'     independent data to be described by the function.
##' @param coefs matrix of coefficients (\code{a} and \code{lambda}) in
##'     rows for each process of the model in columns.
##' @return numeric vector as \code{x} with the evaluated function.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @describeIn boutsNLSll Log likelihood \code{Bouts} method
setMethod("boutsNLSll", signature(obj="Bouts"),
          function(obj, coefs) {
              x <- obj@x
              boutsNLSll(x, coefs=coefs)
          })

##' @describeIn boutsNLSll Log likelihood function \code{numeric} method
setMethod("boutsNLSll", signature(obj="numeric"),
          function(obj, coefs) {
              calc_term <- function(params) {
                  params[1] * params[2] * exp(-params[2] * obj)
              }
              terms <- apply(coefs, 2, calc_term)
              if (is.vector(terms)) {
                  log(sum(terms))
              } else{ log(apply(terms, 1, sum)) }
          })

##' Fit "broken stick" model to log frequency data for identification of
##' bouts of behaviour
##'
##' Fits "broken stick" model to the log frequencies modelled as a function
##' of \var{x} (well, the midpoints of the binned data), using chosen
##' value(s) to separate the two or three processes.
##'
##' @aliases boutinit
##' @param obj Object of class \code{\link{Bouts}} or
##'     \code{\link{data.frame}}.
##' @param x.break Numeric vector of length 1 or 2 with \code{x} value(s)
##'     defining the break(s) point(s) for broken stick model, such that
##'     \code{x} < \code{x.break}[1] is 1st process, and \code{x} >=
##'     \code{x.break}[1] & \code{x} < \code{x.break}[2] is 2nd one, and
##'     \code{x} >= \code{x.break}[2] is 3rd one.
##' @param plot logical, whether to plot results or not.
##' @param ... arguments passed to \code{\link{plot}} (must exclude
##'     \code{type}).
##' @return (2,N) matrix with as many columns as the number of processes
##'     implied by \code{x.break} (i.e. \code{length(x.break) + 1}). Rows
##'     are named \code{a} and \code{lambda}, corresponding to starting
##'     values derived from broken stick model.  A plot is produced as a
##'     side effect if argument \code{plot} is \code{TRUE}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @describeIn boutinit Fit "broken-stick" model on \code{data.frame}
##'     object
##' @examples
##' ## 2-process
##' utils::example("rmixexp", package="diveMove", ask=FALSE)
##' ## 'rndproc2' is a random sample vector from the example
##' xbouts2 <- boutfreqs(rndprocs2, 5)  # Bouts class result
##' (startval2 <- boutinit(xbouts2, 80))
##'
##' ## 3-process
##' ## 'rndproc3' is a random sample vector from the example
##' xbouts3 <- boutfreqs(rndprocs3, 5)
##' (startval3 <- boutinit(xbouts3, c(75, 220)))
setMethod("boutinit", signature(obj="data.frame"),
          function(obj, x.break, plot=TRUE, ...) {
              nproc <- length(x.break)
              if (nproc > 2) stop ("x.break must be of length 1 or 2")
              procf <- cut(obj$x, breaks=c(min(obj$x), x.break,
                                           max(obj$x)),
                           include.lowest=TRUE, right=TRUE)
              coefs <- by(obj, procf, function(k) {
                              coef(lm(lnfreq ~ x, k))})
              pars <- lapply(coefs, function(p) {
                                 lambda <- as.vector(-p[2])
                                 a <- as.vector(exp(p[1]) / lambda)
                                 c(a=a, lambda=lambda)
                             })
              parsmat <- matrix(unlist(pars), nrow=2, ncol=length(pars))
              dimnames(parsmat) <- list(c("a", "lambda"), names(pars))
              if (plot) {
                  requireNamespace("lattice", quietly=TRUE) ||
                      stop("lattice package is not available")
                  panelFUN <- function(x, y, ..., pars=parsmat, ab=coefs) {
                      lattice::panel.xyplot(x, y, ...)
                      "procFun" <- function(x) { boutsNLSll(x, pars) }
                      lattice::panel.curve(procFun, min(x),
                                           max(x), add=TRUE)
                      for (l in seq_len(length(ab))) {
                          lattice::panel.abline(ab[[l]], lty=l)
                      }
                  }
                  pp <- lattice::xyplot(lnfreq ~ x, obj, groups=procf,
                                        pars=parsmat, panel=panelFUN, ...)
                  print(pp)
              }
              parsmat
          })

##' @describeIn boutinit Fit "broken-stick" model on \code{Bouts} object
setMethod("boutinit", signature(obj="Bouts"),
          function(obj, x.break, plot=TRUE, ...) {
              lnfreq <- obj@lnfreq
              boutinit(lnfreq, x.break=x.break, plot=plot, ...)
          })

##' Fit mixture of Poisson Processes to Log Frequency data via Non-linear
##' Least Squares regression
##'
##' Methods for modelling a mixture of 2 or 3 random Poisson processes to
##' histogram-like data of log frequency vs interval mid points.  This
##' follows Sibly et al. (1990) method.
##'
##' @aliases fitNLSbouts
##' @param obj Object of class \code{\link{Bouts}}, or
##'     \code{\link{data.frame}} with named components \var{lnfreq} (log
##'     frequencies) and corresponding \var{x} (mid points of histogram
##'     bins).
##' @param start,maxiter Arguments passed to \code{\link{nls}}.
##' @param ... Optional arguments passed to \code{\link{nls}}.
##' @return \code{nls} object resulting from fitting this model to data.
##' @references
##' Sibly, R.; Nott, H. and Fletcher, D. (1990) Splitting behaviour into
##' bouts Animal Behaviour \bold{39}, 63-69.
##' @seealso \code{fitMLEbouts} for a better approach;
##'     \code{\link{boutfreqs}}; \code{\link{boutinit}}
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @keywords models manip
##' @describeIn fitNLSbouts Fit NLS model on \code{data.frame}
##' @examples
##' ## Run example to retrieve random samples for two- and three-process
##' ## Poisson mixtures with known parameters as 'Bouts' objects
##' ## ('xbouts2', and 'xbouts3'), as well as starting values from
##' ## broken-stick model ('startval2' and 'startval3')
##' utils::example("boutinit", package="diveMove", ask=FALSE)
##'
##' ## 2-process
##' bout2.fit <- fitNLSbouts(xbouts2, start=startval2, maxiter=500)
##' summary(bout2.fit)
##' bec(bout2.fit)
##'
##' ## 3-process
##' ## The problem requires using bound constraints, which is available
##' ## via the 'port' algorithm
##' l_bnds <- c(100, 1e-3, 100, 1e-3, 100, 1e-6)
##' u_bnds <- c(5e4, 1, 5e4, 1, 5e4, 1)
##' bout3.fit <- fitNLSbouts(xbouts3, start=startval3, maxiter=500,
##'                          lower=l_bnds, upper=u_bnds, algorithm="port")
##' plotBouts(bout3.fit, xbouts3)
setMethod("fitNLSbouts", signature(obj="data.frame"),
          function(obj, start, maxiter, ...) {
              dim0 <- dim(start)

              .nlsFUN <- function(x, vcoefs) {
                  coefs <- matrix(vcoefs, nrow=dim0[1], ncol=dim0[2])
                  boutsNLSll(x, coefs)
              }

              start0.names <- apply(expand.grid(dimnames(start)), 1,
                                    function(x) paste(x[1], x[2], sep="_"))
              start0 <- as.vector(start)
              names(start0) <-start0.names
              start0
              fit.nls <- nls(lnfreq ~ .nlsFUN(x, coefs), data=obj,
                             start=list(coefs=start0),
                             control=nls.control(maxiter=maxiter), ...)
              fit.nls
          })

##' @describeIn fitNLSbouts Fit NLS model on \code{Bouts} object
setMethod("fitNLSbouts", signature(obj="Bouts"),
          function(obj, start, maxiter, ...) {
              lnfreq <- obj@lnfreq
              fitNLSbouts(lnfreq, start=start, maxiter=maxiter)
          })

##' Calculate bout ending criteria from model coefficients
##'
##' @aliases bec
##' @param fit Object of class \code{nls} or \code{mle}.
##' @return \code{numeric} vector with the bout ending criterion or
##'     criteria derived from the model.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @keywords models manip
##' @describeIn boutsBEC Calculate BEC on \code{nls} object
setMethod("bec", signature(fit="nls"),
          function(fit) {
              coefs <- coef(fit)
              coefs2D <- matrix(coefs, 2, length(coefs) / 2)
              becs <- rep(NA_real_, ncol(coefs2D) - 1)
              for (coln in seq_len(ncol(coefs2D) - 1)) {
                  procn1 <- coefs2D[, coln]
                  procn2 <- coefs2D[, coln + 1]
                  a1.hat <- procn1[1]
                  lambda1.hat <- procn1[2]
                  a2.hat <- procn2[1]
                  lambda2.hat <- procn2[2]
                  becs[coln] <- (log((a1.hat * lambda1.hat) /
                                     (a2.hat * lambda2.hat)) /
                                 (lambda1.hat - lambda2.hat))
              }
              becs
          })

##' @describeIn boutsBEC Calculate BEC on \code{mle} object
setMethod("bec", signature(fit="mle"),
          function(fit) {
              coefs <- coef(fit)
              ncoefs <- length(coefs)

              switch(as.character(ncoefs),
                     "3" = {
                         p_hat <- as.vector(coefs[1])
                         lambda0_hat <- as.vector(coefs[2])
                         lambda1_hat <- as.vector(coefs[3])
                         log((p_hat * lambda0_hat) /
                             ((1 - p_hat) * lambda1_hat)) /
                             (lambda0_hat - lambda1_hat)
                     },
                     "5" = {
                         p_hat <- as.vector(coefs[1:2])
                         p0_hat <- p_hat[1]
                         p1_hat <- p_hat[2]
                         lambdas_hat <- as.vector(coefs[3:ncoefs])
                         lambda0_hat <- lambdas_hat[1]
                         lambda1_hat <- lambdas_hat[2]
                         lambda2_hat <- lambdas_hat[3]
                         bec0 = (log((p0_hat * lambda0_hat) /
                                     ((1 - p0_hat) * lambda1_hat)) /
                                 (lambda0_hat - lambda1_hat))
                         bec1 = (log((p1_hat * lambda1_hat) /
                                     ((1 - p1_hat) * lambda2_hat)) /
                                 (lambda1_hat - lambda2_hat))
                         c(bec0, bec1)
                     },
                     stop("Not implemented"))

          })

##' Plot fitted Poisson mixture model and data
##'
##' @aliases plotBouts
##' @param fit Object of class \code{nls} or \code{mle}.
##' @param obj Object of class \code{\link{Bouts}},
##'     \code{\link{data.frame}} with columns named \code{lnfreq} and
##'     \code{x} (when \code{fit -> nls object}, or numeric vector (valid
##'     when \code{fit -> mle object}.
##' @param bec.lty Line type specification for drawing the BEC reference
##'     line.
##' @param ... Arguments passed to \code{\link{plot.default}}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{boutfreqs}}, \code{\link{fitNLSbouts}},
##'     \code{\link{fitMLEbouts}}
##' @keywords methods models plot
##' @describeIn plotBouts Plot fitted \code{nls} model on \code{data.frame}
##'     object
setMethod("plotBouts", signature(fit="nls", obj="data.frame"),
          function(fit, obj, bec.lty=2, ...) {
              lnfreq <- obj # copy for sanity
              coefs <- coef(fit)
              coefs2D <- matrix(coefs, 2, length(coefs) / 2)
              becx <- bec(fit)
              plot(lnfreq ~ x, lnfreq, type="n", ...)
              curve(boutsNLSll(x, coefs2D), min(lnfreq$x), max(lnfreq$x),
                    add=TRUE)
              rgbgray <- 190 / 255
              points(lnfreq ~ x, lnfreq, pch=20,
                     col=rgb(rgbgray, rgbgray, rgbgray, alpha=0.4 * 255,
                             maxColorValue=255))
              becy <- predict(fit, list(x=becx))
              points(becx, becy, pch=25, col=rgb(1, 0, 0), bg=rgb(1, 0, 0))
              usr <- par("usr")
              arrows(becx, becy, becx, usr[3], code=0, lty=bec.lty)
              for (bec.i in seq_len(length(becx))) {
                  text(becx[bec.i], becy[bec.i], pos=4, offset=1,
                       paste(paste("bec_", bec.i - 1, "=", sep=""),
                             round(becx[bec.i], 2), sep=""),
                       bty="n", cex=0.8)
              }
          })

##' @describeIn plotBouts Plot fitted \code{nls} model on \code{Bouts}
##'     object
setMethod("plotBouts", signature(fit="nls", obj="Bouts"),
          function(fit, obj, bec.lty=2, ...) {
              lnfreq <- obj@lnfreq
              plotBouts(fit, obj=lnfreq, bec.lty=bec.lty, ...)
          })

##' @describeIn plotBouts Plot fitted \code{mle} model on \code{numeric}
##'     object
##' @param xlab,ylab Label for x and y axis, respectively.
setMethod("plotBouts", signature(fit="mle", obj="numeric"),
          function(fit, obj, xlab="x", ylab="Log Frequency",
                   bec.lty=2, ...) {
              x <- obj # copy for sanity
              coefs <- coef(fit)
              ncoefs <- length(coefs)
              becx <- bec(fit)
              range.x <- range(x, na.rm=TRUE)
              llfun <- ifelse(ncoefs == 3, .bouts2MLEll, .bouts3MLEll)

              if (ncoefs == 3) {
                  p_hat <- as.vector(coefs[1])
                  lambda0_hat <- as.vector(coefs[2])
                  lambda1_hat <- as.vector(coefs[3])
                  curve(llfun(x, p=p_hat, lambda0=lambda0_hat,
                              lambda1=lambda1_hat),
                        from=range.x[1], to=range.x[2], xlab=xlab,
                        ylab=ylab, xaxs="i", yaxs="i", ...)
                  becy <- llfun(becx, p=p_hat, lambda0=lambda0_hat,
                                lambda1=lambda1_hat)
              } else {
                  p_hat <- as.vector(coefs[1:2])
                  p0_hat <- p_hat[1]
                  p1_hat <- p_hat[2]
                  lambdas_hat <- as.vector(coefs[3:ncoefs])
                  lambda0_hat <- lambdas_hat[1]
                  lambda1_hat <- lambdas_hat[2]
                  lambda2_hat <- lambdas_hat[3]
                  curve(llfun(x, p0=p0_hat, p1=p1_hat, lambda0=lambda0_hat,
                              lambda1=lambda1_hat, lambda2=lambda2_hat),
                        from=range.x[1], to=range.x[2], xlab=xlab,
                        ylab=ylab, xaxs="i", yaxs="i", ...)
                  becy <- llfun(becx, p0=p0_hat, p1=p1_hat,
                                lambda0=lambda0_hat,
                                lambda1=lambda1_hat,
                                lambda2=lambda2_hat)
              }
              rug(jitter(x), side=3, ticksize=0.015, quiet=TRUE)
              usr <- par("usr")
              points(becx, becy, pch=25, col=rgb(1, 0, 0), bg=rgb(1, 0, 0))
              arrows(becx, becy, becx, usr[3], code=0, lty=bec.lty)
              for (bec.i in seq_len(length(becx))) {
                  text(becx[bec.i], becy[bec.i], pos=4, offset=1,
                       paste(paste("bec_", bec.i - 1, "=", sep=""),
                             round(becx[bec.i], 2), sep=""),
                       bty="n", cex=0.8)
              }
          })

##' @describeIn plotBouts Plot fitted \code{mle} model on \code{Bouts}
##'     object
setMethod("plotBouts", signature(fit="mle", obj="Bouts"),
          function(fit, obj, xlab="x", ylab="Log Frequency",
                   bec.lty=2, ...) {
              x <- obj@x
              plotBouts(fit, obj=x, xlab=xlab, ylab=ylab,
                        bec.lty=bec.lty, ...)
          })


".plotECDF" <- function(xpred.exp, ypred, xlim, ...) {
    ## Helper function for plotting the ECDF
    if (missing(xlim)) {
        rx <- range(xpred.exp)
        dr <- max(0.07 * diff(rx), median(diff(xpred.exp)))
        xlim <- rx + dr * c(1e-3, 1)
    }
    plot(stepfun(xpred.exp, c(ypred[1], ypred)), do.points=FALSE,
         verticals=TRUE, xlim=xlim, las=1, xaxs="i", log="x", ...)
}


".plotCDF" <- function(fit, xpred.exp, pars.l, draw.bec, bec.lty) {
    ## Helper function for plotting the deterministic CDF
    plot(function(x) {
        boutsCDF(x, pars.l[["p"]], pars.l[["lambdas"]])
    }, min(xpred.exp), max(xpred.exp), add=TRUE)
    if (draw.bec) {
        becx <- bec(fit)
        becy <- boutsCDF(becx, pars.l[["p"]], pars.l[["lambdas"]])
        bec.col <- rgb(1, 0, 0)
        points(becx, becy, pch=25, col=bec.col, bg=bec.col)
        arrows(becx, becy, becx, 0, code=0, lty=bec.lty)
        for (bec.i in seq_len(length(becx))) {
            text(becx[bec.i], becy[bec.i], pos=4, offset=1,
                 paste(paste("bec_", bec.i - 1, "=", sep=""),
                   round(becx[bec.i], 2), sep=""),
                 bty="n", cex=0.8)
        }
    }
}

##' Plot empirical and deterministic cumulative frequency distribution
##' Poisson mixture data and model
##'
##' @aliases plotBoutsCDF
##' @param fit Object of class \code{nls} or \code{mle}.
##' @param obj Object of class \code{\link{Bouts}}.
##' @param xlim 2-length vector with limits for the x axis.  If omitted, a
##'     sensible default is calculated.
##' @param draw.bec logical; whether to draw the BECs
##' @param bec.lty Line type specification for drawing the BEC reference
##'     line.
##' @param ... Arguments passed to \code{\link{plot.default}}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @keywords methods models plot
##' @describeIn plotBoutsCDF Plot (E)CDF on \code{\link{nls}} fit object
##'     and numeric vector
setMethod("plotBoutsCDF", signature(fit="nls", obj="numeric"),
          function(fit, obj, xlim, draw.bec=FALSE, bec.lty=2, ...) {
              x <- log1p(obj)
              x.ecdf <- ecdf(x)
              xpred <- seq(min(x), max(x), length.out=101)
              ypred <- x.ecdf(xpred)
              xpred.exp <- expm1(xpred)
              .plotECDF(xpred.exp, ypred, xlim, ...)
              ## Prepare p to plot estimated CDF
              coefs <- coef(fit)
              coefs2D <- matrix(coefs, 2, length(coefs) / 2)
              p_hat <- calc.p(coefs2D)
              ## parse p and lambda into list
              pars.l <- build.p.lambda(c(p_hat, coefs2D[2, ]))
              .plotCDF(fit, xpred.exp=xpred.exp, pars.l=pars.l,
                       draw.bec=draw.bec, bec.lty=bec.lty)
          })

##' @describeIn plotBoutsCDF Plot (E)CDF on \code{\link{nls}} fit object
##'     and \code{\link{Bouts}} object
setMethod("plotBoutsCDF", signature(fit="nls", obj="Bouts"),
          function(fit, obj, xlim, draw.bec=FALSE, bec.lty=2, ...) {
              x <- obj@x
              plotBoutsCDF(fit, obj=x, xlim=xlim, draw.bec=draw.bec,
                           bec.lty=bec.lty, ...)
          })

##' @describeIn plotBoutsCDF Plot (E)CDF on numeric vector
setMethod("plotBoutsCDF", signature(fit="mle", obj="numeric"),
          function(fit, obj, xlim, draw.bec=FALSE, bec.lty=2, ...) {
              x <- log1p(obj)
              x.ecdf <- ecdf(x)
              xpred <- seq(min(x), max(x), length.out=101)
              ypred <- x.ecdf(xpred)
              xpred.exp <- expm1(xpred)
              .plotECDF(xpred.exp, ypred, xlim, ...)
              ## Prepare parameters for plotting CDF
              coefs <- coef(fit)
              ncoefs <- length(coefs)
              pars.l <- build.p.lambda(coefs) # parse p and lambda into list
              .plotCDF(fit, xpred.exp=xpred.exp, pars.l=pars.l,
                       draw.bec=draw.bec, bec.lty=bec.lty)
          })

##' @describeIn plotBoutsCDF Plot (E)CDF on \code{\link{mle}} fit object
setMethod("plotBoutsCDF", signature(fit="mle", obj="Bouts"),
          function(fit, obj, xlim, draw.bec=FALSE, bec.lty=2, ...) {
              x <- obj@x
              plotBoutsCDF(fit, obj=x, xlim=xlim, draw.bec=draw.bec,
                           bec.lty=bec.lty, ...)
          })

##' Maximum Likelihood Model of mixtures of 2 or 3 Poisson Processes
##'
##' Functions to model a mixture of 2 random Poisson processes to identify
##' bouts of behaviour.  This follows Langton et al. (1995).
##'
##' Mixtures of 2 or 3 Poisson processes are supported. Even in this
##' relatively simple case, it is very important to provide good starting
##' values for the parameters.
##'
##' One useful strategy to get good starting parameter values is to proceed
##' in 4 steps.  First, fit a broken stick model to the log frequencies of
##' binned data (see \code{\link{boutinit}}), to obtain estimates of 4
##' parameters in a 2-process model (Sibly et al. 1990), or 6 in a
##' 3-process model.  Second, calculate parameter(s) \var{p} from the alpha
##' parameters obtained from the broken stick model, to get tentative
##' initial values as in Langton et al. (1995). Third, obtain MLE estimates
##' for these parameters, but using a reparameterized version of the -log
##' L2 function.  Lastly, obtain the final MLE estimates for the 3
##' parameters by using the estimates from step 3, un-transformed back to
##' their original scales, maximizing the original parameterization of the
##' -log L2 function.
##'
##' \code{\link{boutinit}} can be used to perform step 1.  Calculation of
##' the mixing parameters \var{p} in step 2 is trivial from these
##' estimates.  Function \code{\link{boutsMLEll.chooser}} defines a
##' reparameterized version of the -log L2 function given by Langton et
##' al. (1995), so can be used for step 3.  This uses a logit (see
##' \code{\link{logit}}) transformation of the mixing parameter \var{p},
##' and log transformations for both density parameters \var{lambda1} and
##' \var{lambda2}.  Function \code{\link{boutsMLEll.chooser}} can be used
##' again to define the -log L2 function corresponding to the
##' un-transformed model for step 4.
##'
##' \code{fitMLEbouts} is the function performing the main job of
##' maximizing the -log L2 functions, and is essentially a wrapper around
##' \code{\link[stats4]{mle}}.  It only takes the -log L2 function, a list
##' of starting values, and the variable to be modelled, all of which are
##' passed to \code{\link[stats4]{mle}} for optimization.  Additionally,
##' any other arguments are also passed to \code{\link[stats4]{mle}}, hence
##' great control is provided for fitting any of the -log L2 functions.
##'
##' In practice, step 3 does not pose major problems using the
##' reparameterized -log L2 function, but it might be useful to use method
##' \dQuote{L-BFGS-B} with appropriate lower and upper bounds.  Step 4 can
##' be a bit more problematic, because the parameters are usually on very
##' different scales and there can be multiple minima.  Therefore, it is
##' almost always the rule to use method \dQuote{L-BFGS-B}, again bounding
##' the parameter search, as well as passing a \code{control} list with
##' proper \code{parscale} for controlling the optimization.  See
##' \code{Note} below for useful constraints which can be tried.
##' @aliases fitMLEbouts
##' @param obj Object of class \code{\link{Bouts}}.
##' @param start passed to \code{\link[stats4]{mle}}.  A row- and
##'     column-named (2,N) matrix, as returned by \code{\link{boutinit}}.
##' @param optim_opts0 named list of optional arguments passed to
##'     \code{\link[stats4]{mle}} for fitting the first model with
##'     transformed parameters.
##' @param optim_opts1 named list of optional arguments passed to
##'     \code{\link[stats4]{mle}} for fitting the second model with
##'     parameters retrieved from the first model, untransformed to
##'     original scale.
##' @return An object of class \code{\link[stats4]{mle}}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @note
##'
##' In the case of a mixture of 2 Poisson processes, useful values for
##' lower bounds for the transformed negative log likelihood
##' reparameterization are \code{c(-2, -5, -10)}.  For the un-transformed
##' parameterization, useful lower bounds are \code{rep(1e-08, 3)}.  A
##' useful parscale argument for the latter is \code{c(1, 0.1, 0.01)}.
##' However, I have only tested this for cases of diving behaviour in
##' pinnipeds, so these suggested values may not be useful in other cases.
##'
##' The lambdas can be very small for some data, particularly
##' \code{lambda2}, so the default \code{ndeps} in \code{\link{optim}} can
##' be so large as to push the search outside the bounds given.  To avoid
##' this problem, provide a smaller \code{ndeps} value.
##' @references
##' Langton, S.; Collett, D. and Sibly, R. (1995) Splitting behaviour into
##' bouts; a maximum likelihood approach.  Behaviour \bold{132}, 9-10.
##'
##' Luque, S.P. and Guinet, C. (2007) A maximum likelihood approach for
##' identifying dive bouts improves accuracy, precision, and
##' objectivity. Behaviour, \bold{144}, 1315-1332.
##'
##' Sibly, R.; Nott, H. and Fletcher, D. (1990) Splitting behaviour into
##' bouts.  Animal Behaviour \bold{39}, 63-69.
##' @keywords methods models manip
##' @describeIn fitMLEbouts Fit model via MLE on numeric vector.
##' @examples
##' ## Run example to retrieve random samples for two- and three-process
##' ## Poisson mixtures with known parameters as 'Bouts' objects
##' ## ('xbouts2', and 'xbouts3'), as well as starting values from
##' ## broken-stick model ('startval2' and 'startval3')
##' utils::example("boutinit", package="diveMove", ask=FALSE)
##'
##' ## 2-process
##' opts0 <- list(method="L-BFGS-B", lower=c(-2, -5, -10))
##' opts1 <- list(method="L-BFGS-B", lower=c(1e-1, 1e-3, 1e-6))
##' bouts2.fit <- fitMLEbouts(xbouts2, start=startval2, optim_opts0=opts0,
##'                           optim_opts1=opts1)
##' plotBouts(bouts2.fit, xbouts2)
##'
##' ## 3-process
##' opts0 <- list(method="L-BFGS-B", lower=c(-5, -5, -6, -8, -12))
##' ## We know 0 < p < 1, and can provide bounds for lambdas within an
##' ## order of magnitude for a rough box constraint.
##' lo <- c(9e-2, 9e-2, 2e-3, 1e-3, 1e-5)
##' hi <- c(9e-1, 9.9e-1, 2e-1, 9e-2, 5e-3)
##' ## Important to set the step size to avoid running below zero for
##' ## the last lambda.
##' ndeps <- c(1e-3, 1e-3, 1e-3, 1e-3, 1e-5)
##' opts1 <- list(method="L-BFGS-B", lower=lo, upper=hi,
##'               control=list(ndeps=ndeps))
##' bout3.fit <- fitMLEbouts(xbouts3, start=startval3, optim_opts0=opts0,
##'                          optim_opts1=opts1)
##' bec(bout3.fit)
##' plotBoutsCDF(bout3.fit, xbouts3)
setMethod("fitMLEbouts", signature(obj="numeric"),
          function(obj, start, optim_opts0=NULL, optim_opts1=NULL) {
              nproc <- ncol(start)
              if (nproc > 3) {
                  stop("Only mixtures of <= 3 processes are implemented")
              }
              p0 <- calc.p(start)
              ## Transform parameters for first fit
              x0 <- c(logit(p0), log(start["lambda", ]))
              ## First fit
              ll0 <- boutsMLEll.chooser(obj, x0, transformed=TRUE)
              fit0.args <- list(ll0, start=x0)
              fit0 <- do.call("mle", args=c(fit0.args, optim_opts0))
              ## Second fit
              start2.l <- build.p.lambda(coef(fit0))
              start2.l[["p"]] <- unLogit(start2.l[["p"]])
              start2.l[["lambdas"]] <- exp(start2.l[["lambdas"]])
              ll1 <- boutsMLEll.chooser(obj, x0, transformed=FALSE)
              fit1.args <- list(ll1, start=unlist(start2.l))
              do.call("mle", args=c(fit1.args, optim_opts1))
          })

##' @describeIn fitMLEbouts Fit model via MLE on \code{\link{Bouts}}
##'     object.
setMethod("fitMLEbouts", signature(obj="Bouts"),
          function(obj, start, optim_opts0=NULL, optim_opts1=NULL) {
              x <- obj@x
              fitMLEbouts(x, start=start,
                          optim_opts0=optim_opts0,
                          optim_opts1=optim_opts1)
          })

##' Label each vector element or matrix row with bout membership number
##'
##' Identify which bout an observation belongs to.
##'
##' @aliases labelBouts
##' @param obj Object of class \code{\link{Bouts}} object, or numeric
##'     vector or matrix with independent data modelled as a Poisson
##'     process mixture.
##' @param becs numeric vector or matrix with values for the bout ending
##'     criterion which should be compared against the values in x for
##'     identifying the bouts.  It needs to have the same dimensions as
##'     \code{x} to allow for situations where \code{bec} is within
##'     \code{x}.
##' @param bec.method character: method used for calculating the
##'     frequencies: \dQuote{standard} simply uses x, while
##'     \dQuote{seq.diff} uses the sequential differences method.
##' @return \code{labelBouts} returns a numeric vector sequentially
##'     labelling each row or element of \var{x}, which associates it with
##'     a particular bout. \code{unLogit} and \code{logit} return a numeric
##'     vector with the (un)transformed arguments.
##' @keywords methods models manip
##' @describeIn labelBouts Label data on vector or matrix objects.
##' @examples
##' ## Run example to retrieve random samples for two- and three-process
##' ## Poisson mixtures with known parameters as 'Bouts' objects
##' ## ('xbouts2', and 'xbouts3'), as well as starting values from
##' ## broken-stick model ('startval2' and 'startval3')
##' utils::example("boutinit", package="diveMove", ask=FALSE)
##'
##' ## 2-process
##' opts0 <- list(method="L-BFGS-B", lower=c(-2, -5, -10))
##' opts1 <- list(method="L-BFGS-B", lower=c(1e-1, 1e-3, 1e-6))
##' bouts2.fit <- fitMLEbouts(xbouts2, start=startval2, optim_opts0=opts0,
##'                           optim_opts1=opts1)
##' bec2 <- bec(bouts2.fit)
##' ## labelBouts() expects its second argument to have the same
##' ## dimensions as the data
##' labelBouts(xbouts2, becs=rep(bec2, length(xbouts2@x)))
setMethod("labelBouts", signature(obj="numeric"),
          function(obj, becs, bec.method=c("standard", "seq.diff")) {
              if (!is(obj, "matrix")) obj <- as.matrix(obj)
              if (!is(becs, "matrix")) becs <- as.matrix(becs)
              if (!identical(dim(obj), dim(becs)))
                  stop("obj and becs must have the same dimensions")
              bec.method <- match.arg(bec.method)
              switch(bec.method,
                     standard = {xx <- obj[-1, ]},
                     seq.diff = {
                         xx <- apply(obj, 2, function(k) abs(diff(k)))
                     })
              testfun <- function(xi, beci) ifelse(xi > beci, 1, 2)
              bectest <- mapply(testfun, xx, becs[-1, ])
              dim(bectest) <- dim(xx)
              bectest.full <- rbind(1, bectest)
              bectest.any <- apply(bectest.full, 1, function(k) any(k < 2))
              chgbout <- which(bectest.any)
              boutno <- seq(along=chgbout)
              reps <- diff(c(chgbout, nrow(obj) + 1))
              rep(boutno, reps)
          })

##' @describeIn labelBouts Label data on \code{\link{Bouts}} object
setMethod("labelBouts", signature(obj="Bouts"),
          function(obj, becs, bec.method=c("standard", "seq.diff")) {
              x <- obj@x
              labelBouts(x, becs=becs, bec.method=bec.method)
          })

###_  . plotZOC

##' Methods for visually assessing results of ZOC procedure
##'
##' Plots for comparing the zero-offset corrected depth from a
##' \code{\link{TDRcalibrate}} object with the uncorrected data in a
##' \code{\link{TDR}} object, or the progress in each of the filters during
##' recursive filtering for ZOC (\code{\link{calibrateDepth}}).
##'
##' The \code{TDR},\code{matrix} method produces a plot like those shown in
##' Luque and Fried (2011).
##'
##' The \code{TDR},\code{TDRcalibrate} method overlays the corrected depth
##' from the second argument over that from the first.
##'
##' @aliases plotZOC
##' @param x \code{TDR} object.
##' @param y matrix with the same number of rows as there are observations
##'     in \code{x}, or a \code{TDRcalibrate} object.
##' @param xlim \code{POSIXct} or numeric vector of length 2, with lower
##'     and upper limits of time to be plotted.  Defaults to time range of
##'     input.
##' @param ylim numeric vector of length 2 (upper, lower) with axis limits.
##'     Defaults to range of input.
##' @param ylab character strings to label the corresponding y-axis.
##' @param ... Arguments passed to \code{\link{legend}}.
##' @return Nothing; a plot as side effect.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{calibrateDepth}}, \code{\link{.zoc}}
##' @references
##'
##' Luque, S.P. and Fried, R. (2011) Recursive filtering for zero offset
##' correction of diving depth time series. PLoS ONE 6:e15850
##' @keywords methods iplot
##' @describeIn plotZOC This plot helps in finding appropriate parameters
##'     for \code{diveMove:::.depthFilter}, and consists of three panels.
##'     The upper panel shows the original data, the middle panel shows the
##'     filters, and the last panel shows the corrected
##'     data. method=\dQuote{visual} in \code{\link{calibrateDepth}}.
##' @examples
##' ## Using the Example from '?diveStats':
##' \donttest{## Too long for checks
##'
##' utils::example("diveStats", package="diveMove",
##'                ask=FALSE, echo=FALSE, run.donttest=TRUE)
##'
##' ## Plot filters for ZOC
##' ## Work on first phase (trip) subset, to save processing time, since
##' ## there's no drift nor shifts between trips
##' tdr <- divesTDR[1:15000]
##' ## Try window widths (K), quantiles (P) and bound the search (db)
##' K <- c(3, 360); P <- c(0.5, 0.02); db <- c(0, 5)
##' d.filter <- diveMove:::.depthFilter(depth=getDepth(tdr),
##'                                     k=K, probs=P, depth.bounds=db,
##'                                     na.rm=TRUE)
##' old.par <- par(no.readonly=TRUE)
##' plotZOC(tdr, d.filter, ylim=c(0, 6))
##' par(old.par)
##'
##' ## Plot corrected and uncorrected depth, regardless of method
##' ## Look at three different scales
##' xlim1 <- c(getTime(divesTDR)[7100], getTime(divesTDR)[11700])
##' xlim2 <- c(getTime(divesTDR)[7100], getTime(divesTDR)[7400])
##' xlim3 <- c(getTime(divesTDR)[7100], getTime(divesTDR)[7200])
##' par(mar=c(3, 4, 0, 1) + 0.1, cex=1.1, las=1)
##' layout(seq(3))
##' plotZOC(divesTDR, dcalib, xlim=xlim1, ylim=c(0, 6))
##' plotZOC(divesTDR, dcalib, xlim=xlim2, ylim=c(0, 70))
##' plotZOC(divesTDR, dcalib, xlim=xlim3, ylim=c(0, 70))
##' par(old.par)
##'
##' }
setMethod("plotZOC", signature(x="TDR", y="matrix"),
          function(x, y, xlim, ylim, ylab="Depth (m)", ...) {
              .plotZOCfilters(x=x, zoc.filter=y, xlim=xlim, ylim=ylim,
                              ylab=ylab, ...)
          })

##' @describeIn plotZOC This plots depth from the \code{TDRcalibrate}
##'     object over the one from the \code{TDR} object.
setMethod("plotZOC", signature(x="TDR", y="TDRcalibrate"),
          function(x, y, xlim, ylim, ylab="Depth (m)", ...) {
              .plotZOCtdrs(x=x, y=y, xlim=xlim, ylim=ylim, ylab=ylab, ...)
          })


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
    colN <- dataNames %in% .speedNames
    if (length(which(colN)) != 1)
        stop("the column number for speed could not be determined")
    which(colN)
}
setMethod("getSpeed", signature(x="TDRspeed"), function(x) {
    ccData <- x@concurrentData
    speedCol <- .speedCol(ccData)
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
              okpts <- .diveIndices(getDAct(x, "dive.id"), diveNo)
              phases[okpts]
          })

## access the entire object
setMethod("getDiveModel", signature(x="TDRcalibrate", diveNo="missing"),
          function(x) x@dive.models)
## access only those from certain dives -- simplify if only one
setMethod("getDiveModel", signature(x="TDRcalibrate", diveNo="numeric"),
          function(x, diveNo) {
              dml <- x@dive.models
              tryCatch({
                  ok <- .diveMatches(names(dml), diveNo)
                  diveNo.ok <- diveNo[ok]
                  dm <- x@dive.models[diveNo.ok]
                  if (length(diveNo.ok) == 1L) dm[[1]] else dm
              })
          })

## Basic diveModel
setMethod("getDiveDeriv", signature(x="diveModel"),
          function(x, phase=c("all", "descent", "bottom", "ascent")) {
              phase <- match.arg(phase)
              d.crit <- x@descent.crit
              a.crit <- x@ascent.crit
              switch(phase,
                     all = {x@spline.deriv},
                     descent = {
                         spd <- x@spline.deriv
                         t.crit <- x@dive.spline$data$x[d.crit]
                         descent <- which(spd$x < t.crit)
                         spd$x <- spd$x[descent]
                         spd$y <- spd$y[descent]
                         spd
                     },
                     bottom = {
                         spd <- x@spline.deriv
                         t.desc.crit <- x@dive.spline$data$x[d.crit]
                         t.asc.crit <- x@dive.spline$data$x[a.crit]
                         bottom <- which(spd$x >= t.desc.crit &
                                         spd$x <= t.asc.crit)
                         spd$x <- spd$x[bottom]
                         spd$y <- spd$y[bottom]
                         spd
                     },
                     ascent = {
                         spd <- x@spline.deriv
                         t.crit <- x@dive.spline$data$x[a.crit]
                         ascent <- which(spd$x > t.crit)
                         spd$x <- spd$x[ascent]
                         spd$y <- spd$y[ascent]
                         spd
                     })
          })
## TDRcalibrate -- do all dives or selection.  Simplify if only one
setMethod("getDiveDeriv", signature(x="TDRcalibrate"),
          function(x, diveNo, phase=c("all", "descent", "bottom", "ascent")) {
              if (missing(diveNo)) diveNo <- seq(max(getDAct(x, "dive.id")))
              phase <- match.arg(phase)
              dl <- lapply(diveNo, function(k) {
                  dm <- getDiveModel(x, diveNo=k)
                  getDiveDeriv(dm, phase=phase)
              })
              names(dl) <- diveNo
              if (length(diveNo) == 1L) dl[[1]] else dl
          })

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
                     speedCol <- .speedCol(ccData)
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
setMethod("[", signature(x="TDR", i="numeric", j="missing", drop="missing"),
          function(x, i, j, ..., drop) {
    new(class(x), file=getFileName(x), dtime=getDtime(x), time=getTime(x)[i],
        depth=getDepth(x)[i],
        concurrentData=tryCatch(getCCData(x)[i, , drop=FALSE],
          error=function(k) data.frame()))
})


###_ + Generators and Summaries

##' Read comma-delimited file with "TDR" data
##'
##' Read a delimited (*.csv) file containing time-depth recorder
##' (\dfn{TDR}) data from various \acronym{TDR} models.  Return a
##' \code{TDR} or \code{TDRspeed} object. \code{createTDR} creates an
##' object of one of these classes from other objects.
##'
##' The input file is assumed to have a header row identifying each field,
##' and all rows must be complete (i.e. have the same number of fields).
##' Field names need not follow any convention.  However, depth and speed
##' are assumed to be in m, and \eqn{m \cdot s^{-1}}{m/s}, respectively,
##' for further analyses.
##'
##' If \var{speed} is TRUE and concurrentCols contains a column named speed
##' or velocity, then an object of class \code{\link{TDRspeed}} is created,
##' where speed is considered to be the column matching this name.
##'
##' @aliases createTDR
##' @param time A \code{POSIXct} object with date and time readings for
##'     each reading.
##' @param depth numeric vector with depth readings.
##' @param concurrentData \code{\link{data.frame}} with additional,
##'     concurrent data collected.
##' @param speed logical: whether speed is included in one of the columns
##'     of concurrentCols.
##' @param dtime numeric scalar: sampling interval used in seconds.  If
##'     missing, it is calculated from the \code{time} argument.
##' @param file character: a string indicating the path to the file to
##'     read.  This can also be a text-mode connection, as allowed in
##'     \code{\link{read.csv}}.
##' @param dateCol integer: column number containing dates, and optionally,
##'     times.
##' @param timeCol integer: column number with times.
##' @param depthCol integer: column number containing depth readings.
##' @param subsamp numeric scalar: subsample rows in \code{file} with
##'     \code{subsamp} interval, in s.
##' @param concurrentCols integer vector of column numbers to include as
##'     concurrent data collected.
##' @param dtformat character: a string specifying the format in which the
##'     date and time columns, when pasted together, should be interpreted
##'     (see \code{\link{strptime}}).
##' @param tz character: a string indicating the time zone assumed for the
##'     date and time readings.
##' @param ... Passed to \code{\link{read.csv}}
##' @return An object of class \code{\link{TDR}} or \code{\link{TDRspeed}}.
##' @note Although \code{\link{TDR}} and \code{\link{TDRspeed}} classes
##'     check that time stamps are in increasing order, the integrity of
##'     the input must be thoroughly verified for common errors present in
##'     text output from \acronym{TDR} devices such as duplicate records,
##'     missing time stamps and non-numeric characters in numeric fields.
##'     These errors are much more efficiently dealt with outside of
##'     \acronym{GNU} using tools like \code{GNU awk} or \code{GNU sed}, so
##'     \code{\link{diveMove}} does not currently attempt to fix these
##'     errors.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @keywords manip
##' @examples
##' ## Do example to define object zz with location of dataset
##' utils::example("dives", package="diveMove",
##'                ask=FALSE, echo=FALSE)
##' srcfn <- basename(zz)
##' readTDR(zz, speed=TRUE, sep=";", na.strings="", as.is=TRUE)
##'
##' ## Or more pedestrian
##' tdrX <- read.csv(zz, sep=";", na.strings="", as.is=TRUE)
##' date.time <- paste(tdrX$date, tdrX$time)
##' tdr.time <- as.POSIXct(strptime(date.time, format="%d/%m/%Y %H:%M:%S"),
##'                        tz="GMT")
##' createTDR(tdr.time, tdrX$depth, concurrentData=data.frame(speed=tdrX$speed),
##'           file=srcfn, speed=TRUE)
"createTDR" <- function(time, depth,
                        concurrentData=data.frame(matrix(ncol=0,
                          nrow=length(time))),
                        speed=FALSE, dtime, file)
{
    if (missing(dtime)) dtime <- .getInterval(time)
    if(speed) {
        new("TDRspeed", time=time, depth=depth, concurrentData=concurrentData,
            dtime=dtime, file=file)
    } else {
        new("TDR", time=time, depth=depth, concurrentData=concurrentData,
            dtime=dtime, file=file)
    }
}

##' Extract Dives from "TDR" or "TDRcalibrate" Objects
##'
##' Extract data corresponding to a particular dive(s), referred to by
##' number.
##'
##' @aliases extractDive
##' @param obj \code{\link{TDR}} object.
##' @param diveNo numeric vector or scalar with dive numbers to
##'     extract. Duplicates are ignored.
##' @param id numeric vector or scalar of dive numbers from where
##'     \code{diveNo} should be chosen.
##' @return An object of class \code{\link{TDR}} or \code{\link{TDRspeed}}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @keywords methods
##' @describeIn extractDive Extract data on TDR object
##' @examples
##' \donttest{## Too long for checks
##' ## Continuing the Example from '?calibrateDepth':
##' utils::example("calibrateDepth", package="diveMove",
##'                ask=FALSE, echo=FALSE, run.donttest=TRUE)
##' dcalib		# the 'TDRcalibrate' that was created
##'
##' diveX <- extractDive(divesTDR, 9, getDAct(dcalib, "dive.id"))
##' plotTDR(diveX)
##'
##' diveX <- extractDive(dcalib, 5:10)
##' plotTDR(diveX)
##'
##' }
setMethod("extractDive", signature(obj="TDR", diveNo="numeric",
                                   id="numeric"), # for TDR object
          function(obj, diveNo, id) {
              if (length(id) != length(getTime(obj))) {
                  stop ("id and obj must have equal number of rows")
              }
              okpts <- .diveIndices(id, unique(diveNo))
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

##' @describeIn extractDive Extract data on TDRcalibrate object
setMethod("extractDive",                # for TDRcalibrate
          signature(obj="TDRcalibrate", diveNo="numeric", id="missing"),
          function(obj, diveNo) {
              ctdr <- getTDR(obj)
              okpts <- .diveIndices(getDAct(obj, "dive.id"),
                                    unique(diveNo))
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

##' Describe the Time Budget of Major Activities from "TDRcalibrate"
##' object.
##'
##' Summarize the major activities recognized into a time budget.
##'
##' Ignored trivial aquatic periods are collapsed into the enclosing dry
##' period.
##'
##' @aliases timeBudget
##' @param obj \code{\link{TDRcalibrate}} object.
##' @param ignoreZ logical: whether to ignore trivial aquatic periods.
##' @return A \code{\link{data.frame}} with components:
##'
##' \item{phaseno}{A numeric vector numbering each period of activity.}
##'
##' \item{activity}{A factor labelling the period with the corresponding
##' activity.}
##'
##' \item{beg, end}{\code{\link{POSIXct}} objects indicating the beginning
##' and end of each period.}
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{calibrateDepth}}
##' @keywords methods
##' @describeIn timeBudget Base method for computing time budget from
##'     TDRcalibrate object
##' @examples
##' \donttest{## Too long for checks
##' ## Continuing the Example from '?calibrateDepth':
##' utils::example("calibrateDepth", package="diveMove",
##'                ask=FALSE, echo=FALSE, run.donttest=TRUE)
##' dcalib		# the 'TDRcalibrate' that was created
##'
##' timeBudget(dcalib, TRUE)
##'
##' }
setMethod("timeBudget",            # a table of general attendance pattern
          signature(obj="TDRcalibrate", ignoreZ="logical"),
          function(obj, ignoreZ) {
              act <- getGAct(obj, "activity")
              tt <- getTime(getTDR(obj))
              interval <- getDtime(getTDR(obj))
              if (ignoreZ) {            # ignore the short baths
                  act[act == "Z"] <- "L"
                  attlist <- .rleActivity(tt, act, interval)
                  actlabel <- rle(as.vector(act))$values
                  phase.no <- seq(along=actlabel)
              } else {                  # count the short baths
                  attlist <- getGAct(obj)
                  actlabel <- rle(as.vector(act))$values
                  phase.no <- seq(along=actlabel)
              }
              data.frame(phase.no=phase.no, activity=actlabel,
                         beg=attlist[[3]], end=attlist[[4]],
                         row.names=NULL)
          })



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
