% $Id$
\name{plot-methods}
\docType{methods}
\alias{plot-methods}

\alias{plot}
\alias{plotTDR}

\alias{plot,TDR,missing-method}
\alias{plot,TDRspeed,missing-method}
\alias{plot,TDRcalibrate,missing-method}

% -------------------------------------------------------------------------

\title{Methods for plotting objects of class "TDR", "TDRspeed", and
  "TDRcalibrate"}


\description{ Main plotting method for objects of these classes.  Plot
  and optionally set zero-offset correction windows in \acronym{TDR}
  records, with the aid of a graphical user interface (GUI), allowing
  for dynamic selection of offset and multiple time windows to perform
  the adjustment. }


\usage{

plotTDR(time, depth, concurVars=NULL, xlim=NULL, depth.lim=NULL,
        xlab="time (dd-mmm hh:mm)", ylab.depth="depth (m)",
        concurVarTitles=deparse(substitute(concurVars)),
        xlab.format="\%d-\%b \%H:\%M", sunrise.time="06:00:00",
        sunset.time="18:00:00", night.col="gray60", phase.factor=NULL,
        interact=TRUE, key=TRUE, cex.pts=0.4, \ldots)

\S4method{plot}{TDR,missing}(x, y, \ldots)
\S4method{plot}{TDRspeed,missing}(x, y, concurVars, concurVarTitles, \ldots)
\S4method{plot}{TDRcalibrate,missing}(x, y, diveNo=seq(max(getDAct(x, "dive.id"))),
     labels="phase.id", concurVars, surface=FALSE, \ldots)

}


\arguments{

  \item{time}{\code{POSIXct} object with date and time.}

  \item{depth}{numeric vector with depth in m.}

  \item{concurVars}{matrix with additional variables in each column to
    plot concurrently with depth.  For the \code{\link{TDRspeed}}
    method, a matrix with variables to plot, in addition to speed, if
    any.  For the \code{\link{TDRcalibrate}} method, a \bold{character}
    vector indicating which additional components from the concurrent
    data frame should also be plotted, if any.}

  \item{xlim}{\code{POSIXct} or numeric vector of length 2, with lower
    and upper limits of time to be plotted.}

  \item{depth.lim}{numeric vector of length 2, with the lower and upper
    limits of depth to be plotted.}

  \item{xlab, ylab.depth}{character strings to label the corresponding
    y-axes.}

  \item{concurVarTitles}{character vector of titles to label each new
    variable given in \var{concurVars}.}

  \item{xlab.format}{character: format string for formatting the x axis;
    see \code{\link{strptime}}.}

  \item{sunrise.time, sunset.time}{character string with time of sunrise
    and sunset, respectively, in 24 hr format.  This is used for shading
    night time.}

  \item{night.col}{color for shading night time.}

  \item{phase.factor}{factor dividing rows into sections.}

  \item{interact}{logical: whether to provide interactive tcltk controls
    and access to the associated ZOC functionality.}

  \item{key}{logical: whether to draw a key.}

  \item{cex.pts}{Passed to \code{\link{points}} to set the relative size
    of points to plot (if any).}

  \item{\ldots}{For \code{plotTDR}, arguments passed to
    \code{\link{par}}; useful defaults \code{las=1}, \code{bty="n"}, and
    \code{mar} (the latter depending on whether additional concurrent
    data will be plotted) are provided, but they can be overridden.  For
    S4 methods; arguments passed to the appropriate methods.}

  \item{x, y}{\code{\link{TDR}}, \code{\link{TDRspeed}}, or
    \code{\link{TDRcalibrate}} object.}

  \item{diveNo}{numeric vector or scalar with dive numbers to plot.}

  \item{labels}{character: one of \dQuote{phase.id} or
    \dQuote{dive.phase}, specifying whether to label observations based
    on the gross phase ID of the \code{\link{TDR}} object, or based on
    each dive phase, respectively.}

  \item{surface}{logical: whether to plot surface readings.}

}


\details{

  This function is used primarily to correct drifts in the
  pressure transducer of \acronym{TDR} records and noise in depth
  measurements via method=\dQuote{visual} in
  \code{\link{calibrateDepth}}.

}


\section{Methods}{
  \describe{

    \item{plot}{\code{signature(x="TDR", y="missing")}: interactive
      graphical display of the data, with zooming and panning
      capabilities.}

    \item{plot}{\code{signature(x="TDRspeed", y="missing")}: As the TDR
      method, but also plots the concurrent speed readings.}

    \item{plot}{\code{signature(x="TDRcalibrate", y="missing")}: plot
      the \code{\link{TDRcalibrate}} object, labelling identified
      sections of it (see \code{Usage}).  By default it plots all dives,
      labelled by the activity phase they belong to.  It produces a plot
      consisting of one or more panels; the first panel shows depth
      against time, and additional panels show other concurrent data in
      the object.}

  }
  
}


\value{ If called with the \code{interact} argument set to \code{TRUE},
  returns a list (invisibly) with as many components as sections of the
  record that were zero-offset corrected, each consisting of two further
  lists with the same components as those returned by
  \code{\link{locator}}. }


\seealso{ \code{\link{calibrateDepth}}, \code{\link{.zoc}} }

% -------------------------------------------------------------------------

\examples{

## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE)
## Use interact=TRUE (default) to set an offset interactively
plotTDR(getTime(divesTDR), getDepth(divesTDR), interact=FALSE)
## Plot the 'TDR' object
plot(divesTDR, interact=FALSE)

## Plot different aspects of the 'TDRcalibrate' object
plot(dcalib, interact=FALSE)
plot(dcalib, diveNo=19:25, interact=FALSE)
plot(dcalib, labels="dive.phase", interact=FALSE)
if (dev.interactive(orNone=TRUE)) {
    ## Add surface observations and interact
    plot(dcalib, labels="dive.phase", surface=TRUE)
    ## Plot one dive
    plot(dcalib, diveNo=200, labels="dive.phase")
}

}


\author{Sebastian P. Luque \email{spluque@gmail.com},
  with many ideas from CRAN package sfsmisc.}

\keyword{methods}
\keyword{iplot}
