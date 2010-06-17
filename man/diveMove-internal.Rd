% $Id$
\name{diveMove-internal}

\alias{.depthFilter}
\alias{.diveIndices}
\alias{.getInterval}
\alias{.speedStats}
\alias{.night}
\alias{.rleActivity}
\alias{.speedNames}
\alias{.speedCol}
\alias{.zoc}

% -------------------------------------------------------------------------

\title{Internal diveMove Functions}


\description{Functions used for very particular tasks within larger
  functions in diveMove}


\usage{
.depthFilter(depth, k, probs, na.rm)
.diveIndices(diveID, diveNo)
.getInterval(time)
.speedStats(x, vdist)
.night(time, sunrise.time, sunset.time)
.rleActivity(time, act, interval)
.speedCol(x)
.zoc(time, depth, method, control)
}


\arguments{

  \item{depth}{Numeric vector with depth in m.}

  \item{k}{Vector of moving window width integers (coerced to integer by
    \code{\link[caTools]{runquantile}}) to be applied sequentially.}

  \item{probs}{Vector of quantiles to extract at each step indicated by
    \code{k} (so it must be as long as \code{k}).}

  \item{na.rm}{Should \code{\link{NA}} values be removed prior to
    calculating running quantiles?}

  \item{diveID}{Numeric vector of all dive and non dive IDs.}

  \item{diveNo}{Numeric vector of unique dive indices to extract from
    \code{diveID}.}

  \item{time}{\code{POSIXct} object representing time.}

  \item{x}{For \code{.speedStats}, a matrix with a dive's section data
    (time, speed).  A single dive's data, a 3-col matrix with time,
    depth, and speed.  For .speedCol, a data frame where names are
    searched for strings matching \code{.speedNames} (see Details).}

  \item{vdist}{Vertical distance travelled during this time.  If
    \code{vdist} is missing, then it's all horizontal movements (no
    angles).}

  \item{sunrise.time, sunset.time}{Passed from \code{plotTD}.}

  \item{act}{A numeric vector indicating the activity for every element
    of \code{time}.}

  \item{interval}{Sampling interval in seconds.}

  \item{method}{Character string to indicate the method to use for
    zero offset correction.  One of \dQuote{visual}, \dQuote{offset}, or
    \dQuote{filter} (see \sQuote{Details}).}

  \item{control}{Named list of control parameters to be used for ZOC
    method \code{filter} (\code{k}, \code{probs}, \code{na.rm}) and
    \code{offset} (\code{offset}).}

}


\details{These functions are not meant to be called directly by the
  user, as he/she could not care less (right?).  This may change in the
  future.  Nonetheless, they can be called in the usual way for
  functions that are not exported: \code{diveMove:::.functionName}.

  \code{.speedNames} is a character vector with possible names for a
  speed vector.

  \code{.rleActivity} takes a factor indicating different activity
  phases, their associated time, and the sampling interval to return a
  factor uniquely identifying each phase of activity, i.e. labelling
  them.  In addition, it returns the duration of each phase, and their
  beginning and end times.

}


\value{

  \code{.diveIndices} returns a numeric vector with the indices of dives
  (and their beginning/end indices) in \code{diveID}.

  \code{.getInterval} returns a scalar, the mode of intervals between
  time readings.

  \code{.speedStats} returns a 3-column matrix with total distance, mean
  speed, and angle for a section of a dive.

  \code{.night} returns a list with sunrise and sunset times for dates
  in \code{time}.

  \code{.speedCol} returns column number where speed is located in x.

  \code{.rleActivity} returns a list with components:

  \itemize{

    \item{time.br:} {A factor dividing \code{act} into different periods
      of activity.}

    \item{time.peract:} {The duration of each period of activity.}

    \item{beg.time, end.time:} {POSIXct objects indicating the beginning
      and ending times of each period of activity.}

  }

  \code{.zoc} returns a vector with all corrected depths based on
  \sQuote{method}.  Depths < 0 are set to 0.

  \code{.depthFilter} returns a \code{\link{matrix}} with the filtered
  depths at each step, and corrected depth as last column, so it has
  dimensions \code{length(depth)} rows by \code{length(k) + 1} columns.

}


% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{internal}
