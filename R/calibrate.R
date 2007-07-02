## $Id: calibrate.R,v 1.3 2007-07-02 18:12:25 sluque Exp $

"calibrateDepth" <-  function(x, dry.thr=70, wet.thr=3610, dive.thr=4,
                              offset, descent.crit.q=0.1, ascent.crit.q=0.1,
                              wiggle.tol=0.80)
{
    ## Value: A TDRcalibrate object.  Detect water/land phases in TDR
    ## object, zoc data, detect dives and their phases, and label them.
    ## Return a TDRcalibrate object.
    ## --------------------------------------------------------------------
    ## Arguments: x=a TDR object; dry.thr, wet.thr and dive.thr (see
    ## detPhase, detDive, and labDivePhase documentation
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDR")) stop ("x is not a TDR object")
    ## Detect trips and dives
    detp <- diveMove:::.detPhase(getTime(x), getDepth(x), dry.thr=dry.thr,
                                 wet.thr=wet.thr, getDtime(x))
    zd <- if (missing(offset)) {
        zoc(getTime(x), getDepth(x))
    } else zoc(getTime(x), getDepth(x), offset=offset)
    if (!is.null(zd)) x@depth <- zd
    detd <- diveMove:::.detDive(getDepth(x), detp[[2]], dive.thr, getDtime(x))

    ## label phases of dives with their activity
    phaselabs <- diveMove:::.labDivePhase(x, detd[, 1],
                                          descent.crit.q=descent.crit.q,
                                          ascent.crit.q=ascent.crit.q,
                                          wiggle.tol=wiggle.tol)

    new("TDRcalibrate",
        tdr=x,
        gross.activity=detp,
        dive.activity=detd,
        dive.phases=phaselabs,
        dry.thr=dry.thr,
        wet.thr=wet.thr,
        dive.thr=dive.thr)
}


"calibrateSpeed" <- function(x, tau=0.1, contour.level=0.1, z=0, bad=c(0, 0),
                             main=slot(getTDR(x), "file"), coefs, plot=TRUE,
                             postscript=FALSE, ...)
{
    ## Value: list with data frame of rate of depth change and speed, the
    ## bivariate kernel densities, and the quantile regression object with
    ## calibration line.
    ## --------------------------------------------------------------------
    ## Arguments: x=TDRcalibrate object; tau=quantile on which to perform
    ## the regression; contour.level=contour to extract the mesh from the
    ## binned bivariate kernel density estimation (0-1); z=only changes in
    ## depth > than this will be used; bad=vector with rate of depth
    ## change and speed, respectively, indicating that only values greater
    ## than those will be used, main=string for the title of plot;
    ## coefs=intercept and slope of the calibration line, if already
    ## known; plot=logical indicating whether to produce a plot;
    ## postscript=logical for whether to produce postscript output;
    ## ...=optional arguments for rqPlot().
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDRcalibrate")) {
        stop("x must be a TDRcalibrate object")
    } else if (!is(x@tdr, "TDRspeed")) {
        stop("tdr slot in x must be a TDRspeed object")
    }
    require(KernSmooth) || stop(paste("KernSmooth package is required",
                                      "for calibrations"))
    require(quantreg) || stop("quantreg package is required for calibrations")
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
        z <- KernSmooth::bkde2D(cbind(rddepth, curspeed), bandwidth=bandw)
        bins <- contourLines(z$x1, z$x2, z$fhat, levels=contour.level)
        ctr.x <- unlist(sapply(bins, "[", "x"), use.names=FALSE)
        ctr.y <- unlist(sapply(bins, "[", "y"), use.names=FALSE)
        rqFit <- quantreg::rq(ctr.y ~ ctr.x, tau=tau)
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


"rqPlot" <- function(rddepth, speed, z, contours, rqFit, main="qtRegression",
                     xlab="rate of depth change (m/s)", ylab="speed (m/s)",
                     colramp=colorRampPalette(c("white", "darkblue")),
                     col.line="red", cex.pts=1)
{
    ## Value: A quantile regression plot for TDR speed calibration
    ## --------------------------------------------------------------------
    ## Arguments: rddepth=rate of depth change (m), speed=speed (m/s),
    ## contours=list of 2 components (pts, a matrix with columns named x
    ## and y containing the points from the contour; and level a number
    ## indicating the contour level the points correspond to);
    ## rqFit=quantile regression fit object; z=a list with the bivariate
    ## kernel density estimates (1st component the x points of the mesh,
    ## 2nd the y points, and 3rd the matrix of densities); main=title to
    ## display in the plot; xlab and ylab=axis titles; colramp=color
    ## function for the densities.
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    axlims <- range(rddepth, speed, na.rm=TRUE)
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
