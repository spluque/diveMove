% $Id$
\name{detDive-internal}
\alias{detDive-internal}

\alias{.detDive}

% -------------------------------------------------------------------------

\title{Detect dives from depth readings}


\description{Identify dives in \acronym{TDR} records based on a dive
  threshold.}


\usage{
.detDive(zdepth, act, dive.thr)
}


\arguments{

  \item{zdepth}{numeric vector of zero-offset corrected depths.}

  \item{act}{factor as long as \code{depth} coding activity, with levels
  specified as in \code{\link{.detPhase}}.}

  \item{dive.thr}{numeric scalar: threshold depth below which an
    underwater phase should be considered a dive.}

}


\value{A \code{\link{data.frame}} with the following elements for
  \code{.detDive}

  \item{dive.id}{Numeric vector numbering each dive in the record.}

  \item{dive.activity}{Factor with levels \dQuote{L}, \dQuote{W},
    \dQuote{U}, \dQuote{D}, and \dQuote{Z}, see \code{\link{.detPhase}}.
    All levels may be represented.}

  \item{postdive.id}{Numeric vector numbering each postdive interval
    with the same value as the preceding dive.}

}


\seealso{\code{\link{.detPhase}}, \code{\link{.zoc}}}

% -------------------------------------------------------------------------

\examples{

## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE)
dcalib		# the 'TDRcalibrate' that was created

tdr <- getTDR(dcalib)

## Extract the gross activity from an already calibrated TDR object
gross.act <- getGAct(dcalib)
detd <- diveMove:::.detDive(getDepth(tdr), gross.act[[2]], 3)

}

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{internal}% at least one, from doc/KEYWORDS
