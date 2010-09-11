% $Id$
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
.cutDive(x, smooth.par, knot.factor)
.labDive(act, string)
.labDivePhase(x, diveID, \ldots)
}


\arguments{

  \item{x}{For \code{.labDivePhase}, a class \code{\link{TDR}} object.
    For \code{.cutDive}, a 3-col matrix with subscript in original TDR
    object, non NA depths, and numeric vector representing
    \code{\link{POSIXct}} times.}

  \item{smooth.par, knot.factor}{Passed from \code{calibrateDepth}.}

  \item{act}{Factor with values to label.}

  \item{string}{A character belonging to a level of \code{act} to search
    for and label sequentially.}

  \item{diveID}{Numeric vector indexing each dive (non-dives should be
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

  \code{.labDivePhase} returns a factor with levels \dQuote{D},
  \dQuote{DB}, \dQuote{B}, \dQuote{BA}, \dQuote{A}, \dQuote{DA}, and
  \dQuote{X}, breaking the input into descent, descent/bottom, bottom,
  bottom/ascent, ascent, and non-dive, respectively.  If \code{x}
  contains no dives, only level \dQuote{X} is present for all readings.

  .cutDive generates a character vector that breaks a dive into descent,
  descent/bottom, bottom, bottom/ascent, ascent, and/or descent/ascent
  given a proportion of maximum depth for bottom time.  It returns a
  character matrix with orig ID and corresponding label.

}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{internal}
