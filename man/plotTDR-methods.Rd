\name{plotTDR-methods}
\docType{methods}
\alias{plotTDR-methods}

\alias{plotTDR}

\alias{plotTDR,POSIXt,numeric-method}
\alias{plotTDR,TDR,missing-method}
\alias{plotTDR,TDRcalibrate,missing-method}

% -------------------------------------------------------------------------

\title{Methods for plotting objects of class "TDR" and "TDRcalibrate"}


\description{ Main plotting method for objects of these classes.  Plot
  and optionally set zero-offset correction windows in \acronym{TDR}
  records, with the aid of a graphical user interface (GUI), allowing
  for dynamic selection of offset and multiple time windows to perform
  the adjustment. }


\usage{

\S4method{plotTDR}{POSIXt,numeric}(x, y, concurVars=NULL, xlim=NULL, depth.lim=NULL,
        xlab="time (dd-mmm hh:mm)", ylab.depth="depth (m)",
        concurVarTitles=deparse(substitute(concurVars)),
        xlab.format="\%d-\%b \%H:\%M", sunrise.time="06:00:00",
        sunset.time="18:00:00", night.col="gray60",
        dry.time=NULL, phase.factor=NULL, plot.points=FALSE,
        interact=TRUE, key=TRUE, cex.pts=0.4, \ldots)
\S4method{plotTDR}{TDR,missing}(x, y, concurVars, concurVarTitles, \ldots)
\S4method{plotTDR}{TDRcalibrate,missing}(x, y, what=c("phases", "dive.model"),
        diveNo=seq(max(getDAct(x, "dive.id"))), \ldots)

}


\arguments{

  \item{x}{\code{POSIXct} object with date and time, \code{\link{TDR}},
    or \code{\link{TDRcalibrate}} object.}

  \item{y}{numeric vector with depth in m.}

  \item{concurVars}{matrix with additional variables in each column to
    plot concurrently with depth.  For the (\code{TDR},\code{missing})
    and (\code{TDRcalibrate},\code{missing}) methods, a
    \code{\link{character}} vector naming additional variables from the
    \code{concurrentData} slot to plot, if any.}

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

  \item{dry.time}{subset of time corresponding to observations
    considered to be dry.}

  \item{phase.factor}{factor dividing rows into sections.}

  \item{plot.points}{logical: whether to plot points.}

  \item{interact}{logical: whether to provide interactive tcltk controls
    and access to the associated ZOC functionality.}

  \item{key}{logical: whether to draw a key.}

  \item{cex.pts}{Passed to \code{\link{points}} to set the relative size
    of points to plot (if any).}

  \item{\ldots}{For the \code{(POSIXt,numeric)} method, arguments passed
    to \code{\link{par}} for all methods; useful defaults \code{las=1},
    \code{bty="n"}, and \code{mar} (the latter depending on whether
    additional concurrent data will be plotted) are provided, but they
    can be overridden.  For other methods, except
    \code{(TDRcalibrate,missing)}, these can be any of the arguments for
    the \code{(POSIXt,numeric)} method.  For
    \code{(TDRcalibrate,missing)}, these are arguments for the
    appropriate methods.}

  \item{diveNo}{numeric vector or scalar with dive numbers to plot.}

  \item{what}{character: what aspect of the \code{\link{TDRcalibrate}}
    to plot, which selects the method to use for plotting.}

}


\details{

  This function is used primarily to correct drifts in the
  pressure transducer of \acronym{TDR} records and noise in depth
  measurements via method=\dQuote{visual} in
  \code{\link{calibrateDepth}}.

}


\section{Methods}{
  \describe{

    \item{plotTDR}{\code{signature(x="TDR", y="numeric")}: interactive
      graphical display of time-depth data, with zooming and panning
      capabilities.}

    \item{plotTDR}{\code{signature(x="TDR", y="missing")}: As method
    above.}

    \item{plotTDR}{\code{signature(x="TDRcalibrate", y="missing")}: plot
      selected aspects of \code{\link{TDRcalibrate}} object.  Currently,
      two aspects have plotting methods:

      \itemize{

	\item{ \code{phases} (Optional arguments: \code{concurVars},
	  \code{surface}) Plots all dives, labelled by the activity
	  phase they belong to.  It produces a plot consisting of one or
	  more panels; the first panel shows depth against time, and
	  additional panels show other concurrent data in the object.
	  Optional argument \code{concurVars} is a character vector
	  indicating which additional components from the
	  \code{concurrentData} slot to plot, if any.  Optional argument
	  \code{surface} is a logical: whether to plot surface
	  readings. }

	\item{\code{dive.model} Plots the dive model for the selected
	  dive number (\code{diveNo} argument). }

      }
    }

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

\donttest{## Too long for checks
## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE, run.donttest=TRUE)
## Use interact=TRUE (default) to set an offset interactively
## Plot the 'TDR' object
plotTDR(getTime(divesTDR), getDepth(divesTDR), interact=FALSE)
plotTDR(divesTDR, interact=FALSE)

## Plot different aspects of the 'TDRcalibrate' object
plotTDR(dcalib, interact=FALSE)
plotTDR(dcalib, diveNo=19:25, interact=FALSE)
plotTDR(dcalib, what="dive.model", diveNo=25)
if (dev.interactive(orNone=TRUE)) {
    ## Add surface observations and interact
    plotTDR(dcalib, surface=TRUE)
    ## Plot one dive
    plotTDR(dcalib, diveNo=200)
}

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com},
  with many ideas from CRAN package sfsmisc.}

\keyword{methods}
\keyword{iplot}
