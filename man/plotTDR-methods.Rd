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
\S4method{plotTDR}{TDRcalibrate}(x, diveNo=seq(max(getDAct(x, "dive.id"))),
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

  \item{diveNo}{numeric vector or scalar with dive numbers to plot.}

  \item{labels}{character: one of \dQuote{phase.id} or
    \dQuote{dive.phase}, specifying whether to label observations based
    on the gross phase ID of the \code{\link{TDR}} object, or based on
    each dive phase, respectively.}

  \item{surface}{logical: whether to plot surface readings.}

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
      by the activity phase they belong to.  It produces a plot
      consisting of one or more panels; the first panel shows depth
      against time, and additional panels show other concurrent data in
      the object.}

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
    ## Plot one dive
    plotTDR(dcalib, diveNo=200, labels="dive.phase")
}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{methods}
