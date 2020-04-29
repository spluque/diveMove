\name{diveStats}

\alias{diveStats}
\alias{oneDiveStats}
\alias{stampDive}

% -------------------------------------------------------------------------

\title{Per-dive statistics}



\description{Calculate dive statistics in \acronym{TDR} records.}


\usage{
diveStats(x, depth.deriv=TRUE)
oneDiveStats(x, interval, speed=FALSE)
stampDive(x, ignoreZ=TRUE)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{x}{A \code{\link{TDRcalibrate-class}} object for
    \code{diveStats} and \code{stampDive}, and a
    \code{\link{data.frame}} containing a single dive's data (a factor
    identifying the dive phases, a POSIXct object with the time for each
    reading, a numeric depth vector, and a numeric speed vector) for
    \code{oneDiveStats}.}

  \item{depth.deriv}{logical: should depth derivative statistics be
    calculated?}

  \item{interval}{numeric scalar: sampling interval for interpreting
    \code{x}.}

  \item{speed}{logical: should speed statistics be calculated?}

  \item{ignoreZ}{logical: whether phases should be numbered considering
    all aquatic activities (\dQuote{W} and \dQuote{Z}) or ignoring
    \dQuote{Z} activities.}

}


\details{\code{diveStats} calculates various dive statistics based on
  time and depth for an entire \acronym{TDR} record.
  \code{oneDiveStats} obtains these statistics from a single dive, and
  \code{stampDive} stamps each dive with associated phase information.}


\value{A \code{\link{data.frame}} with one row per dive detected
  (durations are in s, and linear variables in m):

  \item{begdesc}{A \code{POSIXct} object, specifying the start time of
    each dive.}

  \item{enddesc}{A \code{POSIXct} object, as \code{begdesc} indicating
    descent's end time.}

  \item{begasc}{A \code{POSIXct} object, as \code{begdesc} indicating
    the time ascent began.}

  \item{desctim}{Descent duration of each dive.}

  \item{botttim}{Bottom duration of each dive.}

  \item{asctim}{Ascent duration of each dive.}

  \item{divetim}{Dive duration.}

  \item{descdist}{Numeric vector with last descent depth.}

  \item{bottdist}{Numeric vector with the sum of absolute depth
    differences while at the bottom of each dive; measure of amount of
    \dQuote{wiggling} while at bottom.}

  \item{ascdist}{Numeric vector with first ascent depth.}

  \item{bottdep.mean}{Mean bottom depth.}

  \item{bottdep.median}{Median bottom depth.}

  \item{bottdep.sd}{Standard deviation of bottom depths.}

  \item{maxdep}{Numeric vector with maximum depth.}

  \item{desc.tdist}{Numeric vector with descent total distance,
    estimated from speed.}

  \item{desc.mean.speed}{Numeric vector with descent mean speed.}

  \item{desc.angle}{Numeric vector with descent angle, from the surface
    plane.}

  \item{bott.tdist}{Numeric vector with bottom total distance, estimated
    from speed.}

  \item{bott.mean.speed}{Numeric vector with bottom mean speed.}

  \item{asc.tdist}{Numeric vector with ascent total distance, estimated
    from speed.}

  \item{asc.mean.speed}{Numeric vector with ascent mean speed.}

  \item{asc.angle}{Numeric vector with ascent angle, from the bottom
    plane.}

  \item{postdive.dur}{Postdive duration.}

  \item{postdive.tdist}{Numeric vector with postdive total distance,
    estimated from speed.}

  \item{postdive.mean.speed}{Numeric vector with postdive mean speed.}

  If \code{depth.deriv=TRUE}, 21 additional columns with the minimum,
  first quartile, median, mean, third quartile, maximum, and standard
  deviation of the depth derivative for each phase of the dive.  The
  number of columns also depends on argument \code{speed}.

  \code{stampDive} returns a \code{\link{data.frame}} with phase number,
  activity, and start and end times for each dive.}


\seealso{\code{\link{calibrateDepth}}, \code{\link{.detPhase}},
  \code{\link{TDRcalibrate-class}}}

% -------------------------------------------------------------------------

\examples{

\donttest{## Too long for checks
## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE, run.donttest=TRUE)
dcalib		# the 'TDRcalibrate' that was created

tdrX <- diveStats(dcalib)
stamps <- stampDive(dcalib, ignoreZ=TRUE)
tdrX.tab <- data.frame(stamps, tdrX)
summary(tdrX.tab)

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{arith}% at least one, from doc/KEYWORDS
\keyword{math}
