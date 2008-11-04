% $Id$
\name{detDive-internal}
\alias{detDive-internal}

\alias{.detDive}

% -------------------------------------------------------------------------

\title{Detect dives from depth readings}


\description{Identify dives in \acronym{TDR} records based on a dive
  threshold.}


\usage{
.detDive(zdepth, act, dive.thr=4, \dots)
}


\arguments{

  \item{zdepth}{Vector of zero-offset corrected depths.}

  \item{act}{Factor as long as \code{depth} coding activity, with levels
  specified as in \code{\link{.detPhase}}.}

  \item{dive.thr}{Threshold depth below which an underwater phase
    should be considered a dive.}

  \item{\dots}{The sampling interval in seconds.}

}


\value{A data frame with the following elements for \code{.detDive}

  \item{dive.id}{Numeric vector numbering each dive in the record.}

  \item{dive.activity}{Factor with levels \dQuote{L}, \dQuote{W},
    \dQuote{U}, \dQuote{D}, and \dQuote{Z}, see \code{\link{.detPhase}}.
    All levels may be represented.}

  \item{postdive.id}{Numeric vector numbering each postdive interval
    with the same value as the preceding dive.}

}


\seealso{\code{\link{.detPhase}}, \code{\link{zoc}}}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{internal}% at least one, from doc/KEYWORDS
