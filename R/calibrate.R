
##' Calibrate Depth and Generate a "TDRcalibrate" object
##'
##' Detect periods of major activities in a \acronym{TDR} record, calibrate
##' depth readings, and generate a \code{\link{TDRcalibrate}} object
##' essential for subsequent summaries of diving behaviour.
##'
##' This function is really a wrapper around \code{.detPhase},
##' \code{.detDive}, and \code{.zoc} which perform the work on simplified
##' objects.  It performs wet/dry phase detection, zero-offset correction
##' of depth, and detection of dives, as well as proper labelling of the
##' latter.
##'
##' The procedure starts by zero-offset correcting depth (see \sQuote{ZOC}
##' below), and then a factor is created with value \dQuote{L} (dry) for
##' rows with NAs for \code{depth} and value \dQuote{W} (wet) otherwise.
##' This assumes that \acronym{TDR}s were programmed to turn off recording
##' of depth when instrument is dry (typically by means of a salt-water
##' switch).  If this assumption cannot be made for any reason, then a
##' logical vector as long as the time series should be supplied as
##' argument \code{wet.cond} to indicate which observations should be
##' considered wet.  This argument is directly analogous to the
##' \code{subset} argument in \code{\link{subset.data.frame}}, so it can
##' refer to any variable in the \code{\link{TDR}} object (see
##' \sQuote{Note} section below).  The duration of each of these phases of
##' activity is subsequently calculated.  If the duration of a dry phase
##' (\dQuote{L}) is less than \code{dry.thr}, then the values in the factor
##' for that phase are changed to \dQuote{W} (wet).  The duration of phases
##' is then recalculated, and if the duration of a phase of wet activity is
##' less than \code{wet.thr}, then the corresponding value for the factor
##' is changed to \dQuote{Z} (trivial wet).  The durations of all phases
##' are recalculated a third time to provide final phase durations.
##'
##' Some instruments produce a peculiar pattern of missing data near the
##' surface, at the beginning and/or end of dives.  The argument
##' \code{interp.wet} may help to rectify this problem by using an
##' interpolating spline function to impute the missing data, constraining
##' the result to a minimum depth of zero.  Please note that this optional
##' step is performed after ZOC and before identifying dives, so that
##' interpolation is performed through dry phases coded as wet because
##' their duration was briefer than \code{dry.thr}.  Therefore,
##' \code{dry.thr} must be chosen carefully to avoid interpolation through
##' legitimate dry periods.
##'
##' The next step is to detect dives whenever the zero-offset corrected
##' depth in an underwater phase is below the specified dive threshold.  A
##' new factor with finer levels of activity is thus generated, including
##' \dQuote{U} (underwater), and \dQuote{D} (diving) in addition to the
##' ones described above.
##'
##' Once dives have been detected and assigned to a period of wet activity,
##' phases within dives are identified using the descent, ascent and wiggle
##' criteria (see \sQuote{Detection of dive phases} below).  This procedure
##' generates a factor with levels \dQuote{D}, \dQuote{DB}, \dQuote{B},
##' \dQuote{BA}, \dQuote{DA}, \dQuote{A}, and \dQuote{X}, breaking the
##' input into descent, descent/bottom, bottom, bottom/ascent, ascent,
##' descent/ascent (ocurring when no bottom phase can be detected) and
##' non-dive (surface), respectively.
##'
##' ## ZOC
##'
##' This procedure is required to correct drifts in the pressure transducer
##' of \acronym{TDR} records and noise in depth measurements.  Three
##' methods are available to perform this correction.
##'
##' Method \dQuote{visual} calls \code{\link{plotTDR}}, which plots depth
##' and, optionally, speed vs. time with the ability of zooming in and out
##' on time, changing maximum depths displayed, and panning through time.
##' The button to zero-offset correct sections of the record allows for the
##' collection of \sQuote{x} and \sQuote{y} coordinates for two points,
##' obtained by clicking on the plot region. The first point clicked
##' represents the offset and beginning time of section to correct, and the
##' second one represents the ending time of the section to correct.
##' Multiple sections of the record can be corrected in this manner, by
##' panning through the time and repeating the procedure.  In case there's
##' overlap between zero offset corrected windows, the last one prevails.
##'
##' Method \dQuote{offset} can be used when the offset is known in advance,
##' and this value is used to correct the entire time series.  Therefore,
##' offset=0 specifies no correction.
##'
##' Method \dQuote{filter} implements a smoothing/filtering mechanism where
##' running quantiles can be applied to depth measurements in a recursive
##' manner (Luque and Fried 2011), using \code{.depth.filter}.  The method
##' calculates the first running quantile defined by \code{probs[1]} on a
##' moving window of size \code{k[1]}.  The next running quantile, defined
##' by \code{probs[2]} and \code{k[2]}, is applied to the smoothed/filtered
##' depth measurements from the previous step, and so on.  The corrected
##' depth measurements (d) are calculated as:
##'
##' \deqn{d=d_{0} - d_{n}}{d=d[0] - d[n]}
##'
##' where \eqn{d_{0}}{d[0]} is original depth and \eqn{d_{n}}{d[n]} is the
##' last smoothed/filtered depth.  This method is under development, but
##' reasonable results can be achieved by applying two filters (see
##' \sQuote{Examples}).  The default \code{na.rm=TRUE} works well when
##' there are no level shifts between non-NA phases in the data, but
##' \code{na.rm=FALSE} is better in the presence of such shifts.  In other
##' words, there is no reason to pollute the moving window with NAs when
##' non-NA phases can be regarded as a continuum, so splicing non-NA phases
##' makes sense.  Conversely, if there are level shifts between non-NA
##' phases, then it is better to retain NA phases to help the algorithm
##' recognize the shifts while sliding the window(s).  The search for the
##' surface can be limited to specified bounds during smoothing/filtering,
##' so that observations outside these bounds are interpolated using the
##' bounded smoothed/filtered series.
##'
##' Once the whole record has been zero-offset corrected, remaining depths
##' below zero, are set to zero, as these are assumed to indicate values at
##' the surface.
##'
##' ## Detection of dive phases
##'
##' The process for each dive begins by taking all observations below the
##' dive detection threshold, and setting the beginning and end depths to
##' zero, at time steps prior to the first and after the last,
##' respectively.  The latter ensures that descent and ascent derivatives
##' are non-negative and non-positive, respectively, so that the end and
##' beginning of these phases are not truncated.  The next step is to fit a
##' model to each dive.  Two models can be chosen for this purpose:
##' \sQuote{unimodal} (default) and \sQuote{smooth.spline}.
##'
##' Both models consist of a cubic spline, and its first derivative is
##' evaluated to investigate changes in vertical rate.  Therefore, at least
##' 4 observations are required for each dive, so the time series is
##' linearly interpolated at equally spaced time steps if this limit is not
##' achieved in the current dive.  Wiggles at the beginning and end of the
##' dive are assumed to be zero offset correction errors, so depth
##' observations at these extremes are interpolated between zero and the
##' next observations when this occurs.
##'
##' ### \sQuote{unimodal}
##'
##' In this default model, the spline is constrained to be unimodal
##' (Koellmann et al. 2014), assuming the diver must return to the surface
##' to breathe.  The model is fitted using the uniReg package (see
##' \code{\link[uniReg]{unireg}}).  This model and constraint are
##' consistent with the definition of dives in air-breathers, so is
##' certainly appropriate for this group of divers.  A major advantage of
##' this approach over the next one is that the degree of smoothing is
##' determined via restricted maximum likelihood, and has no influence on
##' identifying the transition between descent and ascent.  Therefore,
##' unimodal regression splines make the latter transition clearer compared
##' to using smoothing splines.
##'
##' However, note that dives with less than five samples are fit using
##' smoothing splines (see section below) regardless, as they produce the
##' same fit as unimodal regression but much faster.  Therefore, ensure
##' that the parameters for that model are appropriate for the data,
##' although defaults are reasonable.
##'
##' ### \sQuote{smooth.spline}
##'
##' In this model, specified via \code{dive.model="smooth.spline"}, a
##' smoothing spline is used to model each dive (see
##' \code{\link{smooth.spline}}), using the chosen smoothing parameter.
##'
##' Dive phases identified via this model, however, are highly sensitive to
##' the degree of smoothing (\code{smooth.par}) used, thus making it
##' difficult to determine what amount of smoothing is adequate.
##'
##' A comparison of these methods is shown in the Examples section of
##' \code{\link{diveModel}}.
##'
##' The first derivate of the spline is evaluated at a set of knots to
##' calculate the vertical rate throughout the dive and determine the end
##' of descent and beginning of ascent.  This set of knots is established
##' using a regular time sequence with beginning and end equal to the
##' extremes of the input sequence, and with length equal to \eqn{N \times
##' knot.factor}{N * \code{knot.factor}}.  Equivalent procedures are used
##' for detecting descent and ascent phases.
##'
##' Once one of the models above has been fitted to each dive, the quantile
##' corresponding to (\code{descent.crit.q}) of all the positive
##' derivatives (rate of descent) at the beginning of the dive is used as
##' threshold for determining the end of descent.  Descent is deemed to
##' have ended at the \emph{first} minimum derivative, and the nearest
##' input time observation is considered to indicate the end of descent.
##' The sign of the comparisons is reversed for detecting the ascent.  If
##' observed depth to the left and right of the derivative defining the
##' ascent are the same, the right takes precedence.
##'
##' The particular dive phase categories are subsequently defined using
##' simple set operations.
##'
##' @param x An object of class \code{\link{TDR}} for
##'     \code{\link{calibrateDepth}} or an object of class
##'     \code{\link{TDRcalibrate}} for \code{\link{calibrateSpeed}}.
##' @param dry.thr numeric: dry error threshold in seconds.  Dry phases
##'     shorter than this threshold will be considered as wet.
##' @param wet.cond logical: indicates which observations should be
##'     considered wet.  If it is not provided, records with non-missing
##'     depth are assumed to correspond to wet conditions (see
##'     \sQuote{Details} and \sQuote{Note} below).
##' @param wet.thr numeric: wet threshold in seconds. At-sea phases shorter
##'     than this threshold will be considered as trivial wet.
##' @param dive.thr numeric: threshold depth below which an underwater
##'     phase should be considered a dive.
##' @param zoc.method character string to indicate the method to use for
##'     zero offset correction.  One of \dQuote{visual}, \dQuote{offset},
##'     or \dQuote{filter} (see \sQuote{Details}).
##' @param ... Arguments required for ZOC methods \code{filter} (\code{k},
##'     \code{probs}, \code{depth.bounds} (defaults to range), \code{na.rm}
##'     (defaults to TRUE)) and \code{offset} (\code{offset}).
##' @param interp.wet logical: if TRUE (default is FALSE), then an
##'     interpolating spline function is used to impute NA depths in wet
##'     periods (\emph{after ZOC}).  \emph{Use with caution}: it may only
##'     be useful in cases where the missing data pattern in wet periods is
##'     restricted to shallow depths near the beginning and end of dives.
##'     This pattern is common in some satellite-linked \acronym{TDR}s.
##' @param dive.model character string specifying what model to use for
##'     each dive for the purpose of dive phase identification.  One of
##'     \dQuote{smooth.spline} or \dQuote{unimodal}, to choose among
##'     smoothing spline or unimodal regression (see \sQuote{Details}).
##'     For dives with less than five observations, smoothing spline
##'     regression is used regardless (see \sQuote{Details}).
##' @param smooth.par numeric scalar representing amount of smoothing
##'     (argument \code{spar} in \code{\link[stats]{smooth.spline}}) when
##'     \code{dive.model="smooth.spline"}.  If it is NULL, then the
##'     smoothing parameter is determined by Generalized Cross-validation
##'     (GCV). Ignored with default \code{dive.model="unimodal"}.
##' @param knot.factor numeric scalar that multiplies the number of samples
##'     in the dive.  This is used to construct the time predictor for the
##'     derivative.
##' @param descent.crit.q numeric: critical quantile of rates of descent
##'     below which descent is deemed to have ended.
##' @param ascent.crit.q numeric: critical quantile of rates of ascent
##'     above which ascent is deemed to have started.
##' @return An object of class \code{\link{TDRcalibrate}}.
##' @note Note that the condition implied with argument \code{wet.cond} is
##'     evaluated after the ZOC procedure, so it can refer to corrected
##'     depth.  In many cases, not all variables in the \code{\link{TDR}}
##'     object are sampled with the same frequency, so they may need to be
##'     interpolated before using them for this purpose.  Note also that
##'     any of these variables may contain similar problems as those dealth
##'     with during ZOC, so programming instruments to record depth only
##'     when wet is likely the best way to ensure proper detection of
##'     wet/dry conditions.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{TDRcalibrate}}, \code{\link{.zoc}},
##'     \code{\link{.depthFilter}}, \code{\link{.detPhase}},
##'     \code{\link{.detDive}}, \code{\link{plotTDR}}, and
##'     \code{\link{plotZOC}} to visually assess ZOC procedure. See
##'     \code{\link{diveModel}}, \code{\link{smooth.spline}},
##'     \code{\link{unireg}} for dive models.
##' @references
##'
##' Koellmann, C., Ickstadt, K. and Fried, R. (2014) Beyond unimodal
##' regression: modelling multimodality with piecewise unimodal, mixture or
##' additive regression. Technical Report 8.
##' \url{https://sfb876.tu-dortmund.de/FORSCHUNG/techreports.html}, SFB 876, TU
##' Dortmund
##'
##' Luque, S.P. and Fried, R. (2011) Recursive filtering for zero offset
##' correction of diving depth time series. PLoS ONE 6:e15850
##' @keywords manip math
##' @examples
##' data(divesTDR)
##' divesTDR
##'
##' \donttest{## Too long for checks
##' ## Consider a 3 m offset, a dive threshold of 3 m, the 1% quantile for
##' ## critical vertical rates, and a set of knots 20 times as long as the
##' ## observed time steps. Default smoothing spline model for dive phase
##' ## detection, using default smoothing parameter.
##' (dcalib <- calibrateDepth(divesTDR, dive.thr=3, zoc.method="offset",
##'                           offset=3, descent.crit.q=0.01, ascent.crit.q=0,
##'                           knot.factor=20))
##'
##' ## Or ZOC algorithmically with method="filter":
##' ## dcalib <- calibrateDepth(divesTDR, dive.thr=3, zoc.method="filter",
##' ##                          k=c(3, 5760), probs=c(0.5, 0.02), na.rm=TRUE,
##' ##                          descent.crit.q=0.01, ascent.crit.q=0,
##' ##                          knot.factor=20))
##'
##' ## If no ZOC required:
##' data(divesTDRzoc)
##' (dcalib <- calibrateDepth(divesTDRzoc, dive.thr=3, zoc.method="offset",
##'                           offset=0, descent.crit.q=0.01, ascent.crit.q=0,
##'                           knot.factor=20))
##'
##' }
"calibrateDepth" <-  function(x, dry.thr=70, wet.cond, wet.thr=3610,
                              dive.thr=4,
                              zoc.method=c("visual", "offset", "filter"),
                              ..., interp.wet=FALSE,
                              dive.model=c("unimodal", "smooth.spline"),
                              smooth.par=0.1, knot.factor=3,
                              descent.crit.q=0, ascent.crit.q=0)
{
    if (!is(x, "TDR")) stop ("x is not a TDR object")
    mCall <- match.call()
    depth <- getDepth(x)
    time <- getTime(x)
    ## ZOC procedure
    zoc.method <- match.arg(zoc.method)
    ell <- list(...)
    ell.names <- names(ell)
    if (zoc.method == "offset" && !"offset" %in% ell.names)
        stop("offset is indispensable for this method")
    if (zoc.method == "filter") {
        if (!("k" %in% ell.names && "probs" %in% ell.names))
            stop("k and probs are indispensable for this method")
        if (!("depth.bounds" %in% ell.names))
            ell$depth.bounds <- range(depth, na.rm=TRUE)
        if (!"na.rm" %in% ell.names) ell$na.rm <- TRUE
    }
    zd <- .zoc(time, depth, method=zoc.method, control=ell)
    if (!is.null(zd)) x@depth <- zd
    ## Detect phases and dives
    if (missing(wet.cond))
        r <- !is.na(zd)
    else {
        e <- substitute(wet.cond)
        r <- eval(e, as.data.frame(x), parent.frame())
        if (!is.logical(r))
            stop("'subset' must evaluate to logical")
        r <- r & !is.na(r)
    }
    detp <- .detPhase(time, zd, dry.thr=dry.thr, wet.cond=r,
                      wet.thr=wet.thr, interval=getDtime(x))

    if (interp.wet) {
        zdepth <- zd
        wet <- detp[[2]] == "W"
        wet.na <- wet & is.na(zdepth)
        if (any(wet.na)) {
            time.out <- time[wet.na]
            interpFun <- splinefun(time[wet], zdepth[wet])
            interp.depth <- interpFun(x=time.out)
            zdepth[wet.na] <- pmax(0, interp.depth) # set negatives to 0
            x@depth <- zdepth
        }
    }

    detd <- .detDive(getDepth(x), detp[[2]], dive.thr)

    ## Identify dive phases
    dive.model <- match.arg(dive.model)
    phaselabs <- .labDivePhase(x, detd[, 1], dive.model=dive.model,
                               smooth.par=smooth.par,
                               knot.factor=knot.factor,
                               descent.crit.q=descent.crit.q,
                               ascent.crit.q=ascent.crit.q)
    phaselabsF <- phaselabs$phase.labels
    diveModels <- phaselabs$dive.models

    new("TDRcalibrate",
        call=mCall,
        tdr=x,
        gross.activity=detp,
        dive.activity=detd,
        dive.phases=phaselabsF,
        dive.models=diveModels,
        dry.thr=dry.thr,
        wet.thr=wet.thr,
        dive.thr=dive.thr)
}


##' Calibrate and build a "TDRcalibrate" object
##'
##' These functions create a \code{\link{TDRcalibrate}} object which is
##' necessary to obtain dive summary statistics.
##'
##' This calibrates speed readings following the procedure outlined in
##' Blackwell et al. (1999).
##'
##' @param x An object of class \code{\link{TDR}} for
##'     \code{\link{calibrateDepth}} or an object of class
##'     \code{\link{TDRcalibrate}} for \code{\link{calibrateSpeed}}.
##' @param tau numeric scalar: quantile on which to regress speed on rate
##'     of depth change; passed to \code{\link[quantreg]{rq}}.
##' @param contour.level numeric scalar: the mesh obtained from the
##'     bivariate kernel density estimation corresponding to this contour
##'     will be used for the quantile regression to define the calibration
##'     line.
##' @param z numeric scalar: only changes in depth larger than this value
##'     will be used for calibration.
##' @param bad numeric vector of length 2 indicating that only rates of
##'     depth change and speed greater than the given value should be used
##'     for calibration, respectively.
##' @param coefs numeric: known speed calibration coefficients from
##'     quantile regression as a vector of length 2 (intercept, slope).  If
##'     provided, these coefficients are used for calibrating speed,
##'     ignoring all other arguments, except \code{x}.
##' @param main,... Arguments passed to \code{\link{rqPlot}}.
##' @param plot logical: whether to plot the results.
##' @param postscript logical: whether to produce postscript file output.
##' @return An object of class \code{\link{TDRcalibrate}}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{TDRcalibrate}}
##' @references
##' Blackwell S, Haverl C, Le Boeuf B, Costa D (1999). A method for calibrating
##' swim-speed recorders.  Marine Mammal Science 15(3):894-905.
##' @keywords manip math
##' @examples
##' \donttest{## Too long for checks
##' ## Continuing the Example from '?calibrateDepth':
##' utils::example("calibrateDepth", package="diveMove",
##'                ask=FALSE, echo=FALSE, run.donttest=TRUE)
##' dcalib		# the 'TDRcalibrate' that was created
##'
##' ## Calibrate speed using only changes in depth > 2 m
##' vcalib <- calibrateSpeed(dcalib, z=2)
##' vcalib
##'
##' }
"calibrateSpeed" <- function(x, tau=0.1, contour.level=0.1, z=0, bad=c(0, 0),
                             main=slot(getTDR(x), "file"), coefs, plot=TRUE,
                             postscript=FALSE, ...)
{
    if (!is(x, "TDRcalibrate")) {
        stop("x must be a TDRcalibrate object")
    } else if (!is(x@tdr, "TDRspeed")) {
        stop("tdr slot in x must be a TDRspeed object")
    }
    tt <- getTDR(x)
    if (!missing(coefs)) {
        newspeed <- (getSpeed(tt) - coefs[1]) / coefs[2]
        speed(x) <- newspeed
        x@speed.calib.coefs <- coefs
        x
    } else {
        ddepth <- abs(diff(getDepth(tt)))
        dtime <- diff(as.numeric(getTime(tt)))
        rddepth <- ddepth / dtime
        curspeed <- getSpeed(tt)[-1]
        ok <- which(ddepth > z & rddepth > bad[1] & curspeed > bad[2])
        rddepth <- rddepth[ok]
        curspeed <- curspeed[ok]
        bandw <- c(bw.nrd(rddepth), bw.nrd(curspeed))
        z <- bkde2D(cbind(rddepth, curspeed), bandwidth=bandw)
        bins <- contourLines(z$x1, z$x2, z$fhat, levels=contour.level)
        ctr.x <- unlist(sapply(bins, "[", "x"), use.names=FALSE)
        ctr.y <- unlist(sapply(bins, "[", "y"), use.names=FALSE)
        rqFit <- rq(ctr.y ~ ctr.x, tau=tau)
        coefs <- coef(rqFit)
        newspeed <- (getSpeed(tt) - coefs[1]) / coefs[2]
        speed(x@tdr) <- newspeed
        x@speed.calib.coefs <- coefs
        prefix <- gsub("(.*)\\..*", "\\1", main)
        "plot.fun" <- function() {
            ctrs <- list(pts=cbind(x=ctr.x, y=ctr.y), level=contour.level)
            rqPlot(rddepth, curspeed, z=z, contours=ctrs,
                   rqFit=rqFit, main=main, ...)
        }
        if (postscript) {
            outfile <- paste(prefix, "_speedcal.eps", sep="")
            postscript(outfile, paper="special", width=6, height=6,
                       horizontal=FALSE,
                       title=paste(prefix, "speed calibration"))
            plot.fun()
            dev.off()
            plot <- FALSE
        }
        if (plot) plot.fun()
        x
    }
}

##' Plot of quantile regression for speed calibrations
##'
##' Plot of quantile regression for assessing quality of speed calibrations
##'
##' The dashed line in the plot represents a reference indicating a one to
##' one relationship between speed and rate of depth change.  The other
##' line represent the quantile regression fit.
##'
##' @param speed numeric vector with speed in m/s.
##' @param rddepth numeric vector with rate of depth change.
##' @param z list with the bivariate kernel density estimates (1st
##'     component the x points of the mesh, 2nd the y points, and 3rd the
##'     matrix of densities).
##' @param contours list with components: \code{pts} which should be a
##'     matrix with columns named \code{x} and \code{y}, \code{level} a
##'     number indicating the contour level the points in \code{pts}
##'     correspond to.
##' @param rqFit object of class \dQuote{rq} representing a quantile
##'     regression fit of rate of depth change on mean speed.
##' @param main character: string with title prefix to include in ouput
##'     plot.
##' @param xlab,ylab character vectors with axis labels.
##' @param colramp function taking an integer n as an argument and
##'     returning n colors.
##' @param col.line color to use for the regression line.
##' @param cex.pts numeric: value specifying the amount by which to enlarge
##'     the size of points.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{diveStats}}
##' @keywords manip arith hplot
"rqPlot" <- function(rddepth, speed, z, contours, rqFit, main="qtRegression",
                     xlab="rate of depth change (m/s)", ylab="speed (m/s)",
                     colramp=colorRampPalette(c("white", "darkblue")),
                     col.line="red", cex.pts=1)
{
    axlims <- range(rddepth, speed, na.rm=TRUE)
    old.par <- par(no.readonly=TRUE)
    on.exit(par(old.par))
    par(pty="s")
    image(z$x1, z$x2, z$fhat, xlim=axlims, ylim=axlims, col=colramp(256),
          main=main, xlab=xlab, ylab=ylab, cex.lab=1.3, las=1)
    abline(0, 1, lty=2)
    contour(z$x1, z$x2, z$fhat, add=TRUE, levels=contours$level)
    box()
    points(rddepth, speed, pch=".", cex=cex.pts)
    contour.pts <- contours$pts
    contour.pts.xrange <- range(contour.pts[, "x"])
    curve(coef(rqFit)[1] + coef(rqFit)[2] * x, col=col.line,
          from=contour.pts.xrange[1], to=contour.pts.xrange[2], add=TRUE)
    mtext(bquote(y == .(round(coef(rqFit)[1], 3)) +
                 .(round(coef(rqFit)[2], 3)) * x))
}

## Declare global variables, if needed
if (getRversion() >= "2.15.1") utils::globalVariables("x")



## TEST ZONE --------------------------------------------------------------
