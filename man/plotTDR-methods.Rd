% $Id$
\name{plotTDR-methods}
\docType{methods}
\alias{plotTDR-methods}

\alias{plotTDR}

\alias{plotTDR,TDR-method}
\alias{plotTDR,TDRspeed-method}
\alias{plotTDR,TDRcalibrate-method}

% -------------------------------------------------------------------------

\title{Methods for plotting objects of class "TDR", "TDRspeed", and
  "TDRcalibrate"}


\description{
  Main plotting method for objects of these classes.
}


\usage{
\S4method{plotTDR}{TDR}(x, \ldots)
\S4method{plotTDR}{TDRspeed}(x, concurVars, concurVarTitles, \ldots)
\S4method{plotTDR}{TDRcalibrate}(x, what=c("TDR", "phase.models"),
        diveNo=seq(max(getDAct(x, "dive.id"))),
        labels="phase.id", concurVars, surface=FALSE, \ldots)
}


\arguments{

  \item{x}{\code{\link{TDR}}, \code{\link{TDRspeed}}, or
    \code{\link{TDRcalibrate}} object.}

  \item{concurVars, concurVarTitles, \ldots}{Arguments passed to
    \code{\link{plotTD}}. For the \code{\link{TDRspeed}} method,
    \var{concurVars} is a matrix with variables to plot, in addition to
    speed, if any.  \var{concurVarTitles} in this case is a character
    vector with axis labels for speed and the additional variables
    supplied in \var{concurVars}.  For the \code{\link{TDRcalibrate}}
    method, \var{concurVars} is a \bold{character} vector indicating
    which additional components from the concurrent data frame should
    also be plotted, if any.}

  \item{what}{String specifying what aspect of the TDRcalibrate object
    should be plotted.}

  \item{diveNo}{Numeric vector with dive numbers to plot.}

  \item{labels}{One of \dQuote{phase.id} or \dQuote{dive.phase},
    specifying whether to label observations based on the gross phase ID
    of the \code{\link{TDR}} object, or based on each dive
    phase, respectively.}

  \item{surface}{Logical indicating whether to plot surface readings.}

}


\section{Methods}{
  \describe{

    \item{plotTDR}{\code{signature(x="TDR")}: interactive graphical
      display of the data, with zooming and panning capabilities.}

    \item{plotTDR}{\code{signature(x="TDRspeed")}: As the TDR method,
      but also plots the concurrent speed readings.}

    \item{plotTDR}{\code{signature(x="TDRcalibrate")}: plot the
      \code{\link{TDRcalibrate}} object, labelling identified sections
      of it (see \code{Usage}).  By default it plots all dives, labelled
      by the activity phase they belong to.

      Argument \code{what} specifies what aspect of the
      \code{\link{TDRcalibrate}} object should be plotted: \dQuote{TDR}
      specifies plotting dives in the \code{\link{TDR}} object,
      \dQuote{phase.models} specifies plotting the phase model for a
      particular dive.

      It produces a multi-panel figure.  If \code{what="TDR"}, the
      bottom panel shows depth against time, and possibly more panels
      above it showing other concurrent data.  If
      \code{what="phase.models"}, the top panel shows the observed depth
      against time, the cubic spline smoother, the identified descent
      and ascent phases (which form the basis for identifying the rest
      of the dive phases), while the bottom panel shows the first
      derivative of the smooth trace.}

  }
}


\value{ If called with the \code{interact} argument set to \code{TRUE},
  returns coordinates from the \dQuote{visual} ZOC method (see
  \code{\link{calibrateDepth}} and underlying \code{\link{plotTD}}). }


\seealso{ \code{\link{calibrateDepth}}, \code{\link{plotTD}},
  \code{\link{.zoc}} }

% -------------------------------------------------------------------------

\examples{

## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE)
## Plot the 'TDR' object
plotTDR(divesTDR, interact=FALSE)

## Plot different aspects of the 'TDRcalibrate' object
plotTDR(dcalib, interact=FALSE)
plotTDR(dcalib, diveNo=19:25, interact=FALSE)
plotTDR(dcalib, labels="dive.phase", interact=FALSE)
if (dev.interactive(orNone=TRUE)) { # add surface observations and interact
    plotTDR(dcalib, labels="dive.phase", surface=TRUE)

## Plot the dive phase model for one dive
plotTDR(dcalib, what="phase.models", diveNo=200)

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{methods}
