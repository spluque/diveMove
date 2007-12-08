% $Id: diveStats.Rd,v 1.5 2007-12-08 21:50:54 sluque Exp $
\name{diveStats}

\alias{diveStats}
\alias{oneDiveStats}
\alias{stampDive}

% -------------------------------------------------------------------------

\title{Per-dive statistics}



\description{Calculate dive statistics in TDR records.}


\usage{
diveStats(x)
oneDiveStats(x, interval, speed=FALSE)
stampDive(x, ignoreZ=TRUE)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{x}{A \code{\link{TDRcalibrate-class}} object for
    \code{diveStats} and \code{stampDive}, and a data frame containing a
    single dive's data (a factor identifying the dive phases, a POSIXct
    object with the time for each reading, a numeric depth vector, and a
    numeric speed vector) for \code{oneDiveStats}.}

  \item{interval}{Sampling interval for interpreting \code{x}.}

  \item{speed}{Logical; should speed statistics be calculated?}

  \item{ignoreZ}{Logical indicating whether trips should be numbered
    considering all aquatic activities (\dQuote{W} and \dQuote{Z}) or
    ignoring \dQuote{Z} activities.}

}


\details{\code{diveStats} calculates various dive statistics based on
  time and depth for an entire TDR record.  \code{oneDiveStats} obtains
  these statistics from a single dive, and \code{stampDive} stamps each
  dive with associated trip information.}


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

  \item{descdist}{Numeric vector with descent depth.}

  \item{bottdist}{Numeric vector with the sum of absolute depth
    differences while at the bottom of each dive; measure of amount of
    \dQuote{wiggling} while at bottom.}

  \item{ascdist}{Numeric vector with ascent depth.}

  \item{desc.tdist}{Numeric vector with descent total distance,
    estimated from speed.}

  \item{desc.mean.speed}{Numeric vector with descent mean speed.}

  \item{desc.angle}{Numeric vector with descent angle.}

  \item{bott.tdist}{Numeric vector with bottom total distance, estimated
    from speed.}

  \item{bott.mean.speed}{Numeric vector with bottom mean speed.}

  \item{asc.tdist}{Numeric vector with ascent total distance, estimated
    from speed.}

  \item{asc.mean.speed}{Numeric vector with ascent mean speed.}

  \item{asc.angle}{Numeric vector with ascent angle.}

  \item{divetim}{Dive duration.}

  \item{maxdep}{Numeric vector with maximum depth.}

  \item{postdive.dur}{Postdive duration.}

  \item{postdive.tdist}{Numeric vector with postdive total distance,
    estimated from speed.}

  \item{postdive.mean.speed}{Numeric vector with postdive mean speed.}

  The number of columns depends on the value of speed.

  \code{stampDive} returns a data.frame with trip number, trip type, and
  start and end times for each dive.}


\seealso{\code{\link{.detPhase}}, \code{\link{zoc}},
  \code{\link{TDRcalibrate-class}}}

% -------------------------------------------------------------------------

\examples{

data(divesTDRcalibrate)
divesTDRcalibrate

tdrX <- diveStats(divesTDRcalibrate)
stamps <- stampDive(divesTDRcalibrate, ignoreZ=TRUE)
tdrX.tab <- data.frame(stamps, tdrX)
summary(tdrX.tab)

}



\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{arith}% at least one, from doc/KEYWORDS
\keyword{math}
