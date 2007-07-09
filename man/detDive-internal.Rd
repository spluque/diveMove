\name{detDive-internal}
\encoding{latin1}
\alias{detDive-internal}

\alias{.detDive}

% -------------------------------------------------------------------------

\title{Detect dives from depth readings}



\description{Identify dives in TDR records based on a dive threshold.}


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

  \item{dive.activity}{Factor with levels \sQuote{L}, \sQuote{W},
    \sQuote{U}, \sQuote{D}, and \sQuote{Z}, see \code{\link{.detPhase}}.
    All levels may be represented.}

  \item{postdive.id}{Numeric vector numbering each postdive interval
    with the same value as the preceding dive.}

}


% -------------------------------------------------------------------------

\author{\enc{Sebastián}{Sebastian} P. Luque \email{spluque@gmail.com}}


\seealso{\code{\link{.detPhase}}, \code{\link{zoc}}}


\keyword{internal}% at least one, from doc/KEYWORDS
