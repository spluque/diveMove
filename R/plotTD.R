###_ Internal Function
".night" <- function(time, sunrise.time, sunset.time)
{
    ## Value: A list with sunset and sunrise times for dates in 'time'
    ## --------------------------------------------------------------------
    ## Arguments: Passed from plotTD
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    morn.uniq <- unique(format(time, format=paste("%Y-%m-%d", sunrise.time)))
    morn <- as.POSIXct(morn.uniq, tz = attr(time, "tzone")) + 86400
    morn.before <- morn[1] - 86400
    morn.all <- rbind(data.frame(x=morn.before), data.frame(x=morn))[[1]]
    night.uniq <- unique(format(time, format=paste("%Y-%m-%d", sunset.time)))
    night <- as.POSIXct(night.uniq, tz=attr(time, "tzone"))
    night.before <- night[1] - 86400
    night.all <- rbind(data.frame(x=night.before), data.frame(x=night))[[1]]
    list(sunrises=morn.all, sunsets=night.all)
}

###_ Main Function
"plotTD" <- function(time, depth, concurVars=NULL, xlim=NULL, depth.lim=NULL,
                     xlab="time (dd-mmm hh:mm)", ylab.depth="depth (m)",
                     concurVarTitles=deparse(substitute(concurVars)),
                     xlab.format="%d-%b %H:%M", sunrise.time="06:00:00",
                     sunset.time="18:00:00", night.col="gray60",
                     phaseCol=NULL, interact=TRUE, key=TRUE, cex.pts=0.4, ...)
{
    ## Value: Returns (invisibly) a list with coordinates for each zoc'ed
    ## time window.  Also Plot time, depth, and other concurrent data.
    ## --------------------------------------------------------------------
    ## Arguments: time=POSIXct; depth=numeric vector with depth readings,
    ## concurVars=matrix of numeric data with concurrent data to plot,
    ## xlim=POSIXct vector with lower and upper time limits to plot,
    ## depth.lim=vector with lower and upper depth limits, phaseCol=factor
    ## classifying each reading, xlab=title for the x axis,
    ## ylab.depth=title for the depth axis, concurVarTitles=string vector
    ## with titles for the additional variables, xlab.format=format string
    ## for formatting time in x axis, sunrise.time=string specifying the
    ## time of sunrise, sunset.time=string specifying sunset time,
    ## night.col=color for masking night times, key=logical whether to
    ## draw a legend; ...=parameters passed to par.
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    nights <- diveMove:::.night(time, sunrise.time, sunset.time)
    nconcurVars <- ifelse(is.null(concurVars), 0, ncol(concurVars))
    plotrows <- nconcurVars + 1
    ncheight <- 1.35 * (1/plotrows)
    lheights <- c(rep((1 - ncheight)/nconcurVars, nconcurVars), ncheight)
    mardepthonly <- c(4, 4, 1, 1) + 0.1    # for depth plot only
    mardepthmore <- c(4, 4, -0.1, 1) + 0.1 # for depth plot and more vars
    martop <- c(-0.1, 4, 1, 1) + 0.1       # for top plot
    marnontop <- c(-0.1, 4, -0.1, 1) + 0.1 # for plots between depth and top one
    orig <- structure(0, class=class(time), tzone=attr(time, "tzone"))
    "plot.fun" <- function(xlim, ylim) {
        xticks <- orig + seq(from=xlim[1], to=xlim[2], length=20)
        if(is.null(concurVars)) {
            par(las=1, bty="n", mar=mardepthonly, ...)
        } else {
            par(las=1, bty="n", mar=mardepthmore, ...)
            layout(matrix(seq(plotrows, 1), nrow=plotrows, ncol=1),
                   heights=lheights)
        }
        plot(depth ~ time, type="n", xlim=xlim, ylim=ylim,
             xlab=xlab, ylab=ylab.depth, xaxt="n", yaxt="n")
        usr <- par("usr")
        xleft <- pmax(unclass(nights$sunsets), usr[1])
        xright <- pmin(unclass(nights$sunrises), usr[2])
        rect(xleft, usr[3], xright, usr[4], col=night.col, border=NA)
        axis.POSIXct(side=1, time, at=xticks, format=xlab.format)
        axis(side=2)
        lines(time, depth)
        if (!is.null(phaseCol)) {
            phaseCol <- phaseCol[, drop=TRUE]
            colors <- rainbow(nlevels(phaseCol))
            points(time, depth, col=colors[phaseCol], pch=19, cex=cex.pts)
            if (key && nlevels(phaseCol) < 11) {
                legend("bottomright", legend=levels(phaseCol), col=colors,
                       pch=19, cex=0.7, ncol=nlevels(phaseCol), bg="white")
            }
        }
        if (!is.null(concurVars)) {
            if (length(concurVarTitles) != nconcurVars) {
                concurVarTitles <- rep(concurVarTitles, length.out=nconcurVars)
            }
            for (i in seq(nconcurVars)) {
                vari <- concurVars[, i]
                if (i == nconcurVars) par(mar=martop) else par(mar=marnontop)
                ylim <- range(vari, na.rm=TRUE)
                plot(vari ~ time, type="n", xaxt="n", ylim=ylim,
                     xlab="", xlim=xlim, bty="n", ylab=concurVarTitles[i])
                usr <- par("usr")    # to watch out for change in y coords
                rect(xleft, usr[3], xright, usr[4], col=night.col, border=NA)
                lines(time, vari)
                if (!is.null(phaseCol)) { # we already have 'colors'
                    points(time, vari, col=colors[phaseCol], pch=19, cex=cex.pts)
                }
                axis(side=2)
            }
        }
    }
    if (!interact) {
        rx <- range(as.numeric(time))   # max and min of dates
        xlim <- if(is.null(xlim)) rx else as.numeric(xlim)
        ylim <- if (is.null(depth.lim)) {
            rev(range(depth, na.rm=TRUE))
        } else rev(depth.lim)
        plot.fun(xlim=xlim, ylim=ylim)
    } else {
        require(tcltk) || stop("tcltk support is absent")
        rx <- range(as.numeric(time))   # max and min of dates
        diffrx <- diff(rx)
        xlim <- x10 <- if(is.null(xlim)) { # define xlim if not there already
            rx + (diffrx * 0.01)           # add 1% to each side
        } else as.numeric(xlim)
        xlmid <- xm0 <- mean(xlim)     # two vars with date range midpoint
        xr0 <- diff(xlim)                     # range of xlim
        xZoom <- tclVar(100)                  # initialize zoom factor
        xlmid <- tclVar(xlmid)            # initialize date range midpoint
        xZ <- as.numeric(tclvalue(xZoom)) # these 2 are to be dynamically changed
        xM <- as.numeric(tclvalue(xlmid))
        ylim <- if (is.null(depth.lim)) {
            rev(range(depth, na.rm=TRUE)) * 1.1
        } else rev(depth.lim)
        yMax <- tclVar(ylim[1])
        yTop <- as.numeric(tclvalue(yMax))
        replot <- function(...) {
            xZ <<- as.numeric(tclvalue(xZoom))
            xM <<- as.numeric(tclvalue(xlmid))
            xr.half <- (xr0/2) * 100/xZ
            xlim <- xM + c(-xr.half, xr.half)
            yTop <<- as.numeric(tclvalue(yMax))
            ylim <- c(yTop, ylim[2])
            plot.fun(xlim=xlim, ylim=ylim)
        }
        replot.maybe <- function(...) {
            if(as.numeric(tclvalue(xZoom)) != xZ ||
               as.numeric(tclvalue(xlmid)) != xM ||
               as.numeric(tclvalue(yMax)) != yTop) replot()
        }
        coords <- list()
        zocrange <- function() {
            coords[[length(coords) + 1]] <<- locator(2)
            tkgrab.release(base)
        }

        base <- tktoplevel()
        tkwm.title(base, "diveMove")
        tkwm.deiconify(base)
        tkgrab.set(base)
        tkfocus(base)

        base.frame <- tkframe(base, borderwidth=3)

        x.frame <- tkframe(base.frame)
        xr.frame <- tkframe(x.frame, relief="groove", borderwidth=5)
        dep.frame <- tkframe(x.frame, relief="groove", borderwidth=5)
        tkpack(xr.frame, dep.frame, side="left", anchor="s")

        xmid.frame <- tkframe(base.frame, relief="groove", pady=5)

        but.frame <- tkframe(base)
        zoc.pts <- tkbutton(but.frame, text="Zero-Offset Correct a Range",
                            command=zocrange)
        q.but <- tkbutton(but.frame, text="      Quit       ",
                          command=function() tkdestroy(base))
        tkpack(zoc.pts, q.but, side="left", padx=20)

        tkpack(base.frame, x.frame, xmid.frame, but.frame)

        ## Zoom
        diffx <- diff(as.numeric(time))
        diffxOK <- min(diffx[diffx > 0]) * 40 # zoom up to 40 observations
        maxZoom <- (diffrx / diffxOK) * 100 # maximum zoom depends on time range
        tkpack(tklabel(xr.frame, text="Date Zoom (%)"))
        tkpack(tkscale(xr.frame, command=replot.maybe, from=100,
                       to=maxZoom, showvalue=TRUE, variable=xZoom,
                       length=200, orient="horiz"))
        ## Pan
        tkpack(tklabel(xmid.frame, text="Pan through Date"))
        tkpack(tkscale(xmid.frame, command=replot.maybe,
                       from=xm0 - xr0, to=xm0 + xr0, showvalue=FALSE,
                       variable=xlmid, resolution=xr0/2000, length=200,
                       orient="horiz"))
        ## Maximum depth selection
        tkpack(tklabel(dep.frame, text="Max. Depth (m)"))
        tkpack(tkscale(dep.frame, command=replot.maybe, from=0, to=ylim[1],
                       length=150, showvalue=TRUE, variable=yMax,
                       orient="horiz"))

        replot()
        tkwait.window(base)
        invisible(coords)
    }
}


###_* Emacs local variables.
## Local variables:
## allout-widgets-mode-inhibit: t
## allout-layout: (-1 : 0)
## End:
