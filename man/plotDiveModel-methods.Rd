% $Id: plotDiveModel-methods.Rd 200 2008-11-04 03:06:40Z sluque $
\name{plotDiveModel-methods}
\docType{methods}
\alias{plotDiveModel-methods}

\alias{plotDiveModel}
\alias{plotDiveModel,diveModel,missing-method}
\alias{plotDiveModel,numeric,numeric-method}
\alias{plotDiveModel,TDRcalibrate,missing-method}

% -------------------------------------------------------------------------

\title{ Methods for plotting models of dive phases }


\description{ Methods for function \code{plotDiveModel}. }


\usage{
\S4method{plotDiveModel}{diveModel,missing}(x, diveNo)

\S4method{plotDiveModel}{numeric,numeric}(x, y, times.s, depths.s, d.crit, a.crit,
              diveNo=1, times.deriv, depths.deriv,
              d.crit.rate, a.crit.rate)

\S4method{plotDiveModel}{TDRcalibrate,missing}(x, diveNo)
}


\arguments{

  \item{x}{A \code{\link{diveModel}} (diveMode,missing method),
    \code{\link{numeric}} vector of time step observations
    (numeric,numeric method), or \code{\link{TDRcalibrate}} object
    (TDRcalibrate,numeric method).}

  \item{diveNo}{integer representing the dive number selected for
    plotting.}

  \item{y}{numeric vector with depth observations at each time step.}

  \item{times.s}{numeric vector with time steps used to generate the
    smoothing spline (i.e. the knots, see \code{\link{diveModel}}).}

  \item{depths.s}{numeric vector with smoothed depth (see
  \code{\link{diveModel}}).}

  \item{d.crit}{integer denoting the index where descent ends in the
    observed time series (see \code{\link{diveModel}}).}

  \item{a.crit}{integer denoting the index where ascent begins in the
    observed time series (see \code{\link{diveModel}}).}

  \item{times.deriv}{numeric vector representing the time steps where
    the derivative of the smoothing spline was evaluated (see
    \code{\link{diveModel}}).}

  \item{depths.deriv}{numeric vector representing the derivative of the
    smoothing spline evaluated at \code{times.deriv} (see
    \code{\link{diveModel}}).}

  \item{d.crit.rate}{numeric scalar: vertical rate of descent
    corresponding to the quantile used (see \code{\link{diveModel}}).}

  \item{a.crit.rate}{numeric scalar: vertical rate of ascent
    corresponding to the quantile used (see \code{\link{diveModel}}).}

}


\section{Methods}{

  All methods produce a double panel plot.  The top panel shows the
  depth against time, the cubic spline smoother, the identified descent
  and ascent phases (which form the basis for identifying the rest of
  the dive phases), while the bottom panel shows the first derivative of
  the smooth trace.

  \describe{

    \item{\code{signature(x = "diveModel", y = "missing")}}{ Given a
      \code{\link{diveModel}} object and (possibly) the dive number that
      it corresponds to, the plot shows the model data. }

    \item{\code{signature(x = "numeric", y = "numeric")}}{ This is the
      main method, which requires all aspects of the model to be
      provided. }

    \item{\code{signature(x = "TDRcalibrate", y = "missing")}}{ Given a
      \code{\link{TDRcalibrate}} object and a dive number to extract
      from it, this method plots the observed data and the model.  The
      intended use of this method is through \code{\link{plotTDR}} when
      \code{what="dive.model"}. }
    
}}


\seealso{ \code{\link{diveModel}} }

% -------------------------------------------------------------------------

\examples{

## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE)

## 'diveModel' method
dm <- getDiveModel(dcalib, 100)
plotDiveModel(dm, diveNo=100)

## 'TDRcalibrate' method
plotDiveModel(dcalib, diveNo=100)

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{methods}
