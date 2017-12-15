\name{labDive-internal}
\alias{labDive-internal}

\alias{.cutDive}
\alias{.labDive}
\alias{.labDivePhase}

% -------------------------------------------------------------------------

\title{Internal Functions used for Detection of Dives}


\description{
  These functions provide information for particular dives,
}


\usage{
.cutDive(x, dive.model, smooth.par=NULL, knot.factor, sigmasq=2,
         g=min(max(10, nrow(x) - 4), 25), ordpen=2,
         descent.crit.q, ascent.crit.q)
.labDive(act, string)
.labDivePhase(x, diveID, \ldots)
}


\arguments{

  \item{x}{For \code{.labDivePhase}, a class \code{\link{TDR}} object.
    For \code{.cutDive}, a 3-col matrix with subscript in original TDR
    object, non NA depths, and numeric vector representing
    \code{\link{POSIXct}} times.}

  \item{dive.model, smooth.par, knot.factor, sigmasq, g, ordpen,
        descent.crit.q, ascent.crit.q}{Passed from \code{calibrateDepth}.}

  \item{g, ordpen}{default arguments for \code{\link{unireg}}, so only
    relevant for \code{divemodel="unimodal"}.}

  \item{act}{factor with values to label.}

  \item{string}{character string belonging to a level of \code{act} to
    search for and label sequentially.}

  \item{diveID}{numeric vector indexing each dive (non-dives should be
    0)}

  \item{\ldots}{Arguments passed to \code{.cutDive} (\code{smooth.par}
    and \code{knot.factor}).}

}


\details{These functions are for internal use and are not meant to be
  called by the user.}


\value{

  \code{.labDive} returns a matrix with as many rows as its first two
  arguments with two columns: dive.id, and postdive.id, each one
  sequentially numbering each dive and postdive period.

  \code{.labDivePhase} returns a list with a factor having levels
  \dQuote{D}, \dQuote{DB}, \dQuote{B}, \dQuote{BA}, \dQuote{A},
  \dQuote{DA}, and \dQuote{X}, breaking the input into descent,
  descent/bottom, bottom, bottom/ascent, ascent, and non-dive,
  respectively.  If \code{x} contains no dives, only level \dQuote{X} is
  present for all readings.  The list also contains another list with
  \code{\link{diveModel}} objects for each dive.

  \code{.cutDive} generates a \code{\link{diveModel}} object with
  details of model used to detect dive phases using cubic spline
  smoothing and its first derivative.

}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{internal}
