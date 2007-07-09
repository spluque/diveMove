\name{plotTDR-methods}
\encoding{latin1}
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

  \item{x}{\code{"\linkS4class{TDR}"}, \code{"\linkS4class{TDRspeed}"},
    or \code{"\linkS4class{TDRcalibrate}"}object.}

  \item{concurVars, concurVarTitles, \ldots}{Arguments passed to
    \code{\link{plotTD}}. For the \code{\link{TDRspeed}} method,
    \var{concurVars} is a matrix with variables to plot, in addition to
    speed, if any.  \var{concurVarTitles} in this case is a character
    vector with axis labels for speed and the additional variables
    supplied in \var{concurVars}.  For the TDRcalibrate method,
    \var{concurVars} is a \bold{character} vector indicating which
    additional components from the concurrent data frame should also be
    plotted, if any.}

  \item{diveNo}{Numeric vector with dive numbers to plot.}

  \item{labels}{One of \dQuote{phase.id} or \dQuote{dive.phase},
    specifying whether to label observations based on the gross phase ID
    of the \code{"\linkS4class{TDR}"} object, or based on each dive
    phase, respectively.}

  \item{surface}{Logical indicating whether to plot surface readings.}

}


\section{Methods}{
  \describe{

    \item{plotTDR}{\code{signature(x="TDR")}: interactive graphical
      display of the data, with zooming and panning capabilities.}

    \item{plotTDR}{\code{signature(x="TDRspeed")}: As the TDR method,
      but also plots the concurrent speed readings.}

    \item{plotTDR}{\code{signature(x="TDRcalibrate")}: plot the TDR
      object, labelling identified sections of it (see \code{Usage}).}

  }
}


\value{ If called with the \code{interact} argument set to \code{TRUE},
  returns coordinates from the ZOC procedure (see \code{\link{zoc}}). }


\seealso{
  \code{\link{zoc}}
}

% -------------------------------------------------------------------------

\examples{

data(divesTDR)
divesTDR

plotTDR(divesTDR, interact=FALSE)

data(divesTDRcalibrate)
divesTDRcalibrate

plotTDR(divesTDRcalibrate, interact=FALSE)
plotTDR(divesTDRcalibrate, diveNo=19:25, interact=FALSE)
plotTDR(divesTDRcalibrate, labels="dive.phase", interact=FALSE)

}

\author{\enc{Sebastián}{Sebastian} P. Luque \email{spluque@gmail.com}}

\keyword{methods}
