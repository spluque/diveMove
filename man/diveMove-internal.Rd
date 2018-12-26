\name{diveMove-internal}

\alias{.depthFilter}
\alias{.derivStats}
\alias{.diveIndices}
\alias{.getInterval}
\alias{.plotTDR}
\alias{.plotZOCfilters}
\alias{.plotZOCtdrs}
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
.depthFilter(depth, k, probs, depth.bounds, na.rm)
.derivStats(x, diveNo)
.diveIndices(diveID, diveNo)
.diveMatches(diveID, diveNo)
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
    \code{\link{.runquantile}}) to be applied sequentially.}

  \item{probs}{Vector of quantiles to extract at each step indicated by
    \code{k} (so it must be as long as \code{k}).}

  \item{depth.bounds}{Numeric vector of length two, with minimum and
    maximum depth to bound the search for the surface.}

  \item{na.rm}{Should \code{\link{NA}} values be removed prior to
    calculating running quantiles?}

  \item{diveID}{Numeric (or character for \code{.diveMatches}) vector of
    all dive and non dive IDs.}

  \item{diveNo}{Numeric vector of unique dive indices to extract from
    \code{diveID}, or \code{x} in the case of \code{.derivStats}.  For
    \code{.derivStats} any duplicates are ignored.}

  \item{time}{\code{POSIXct} object representing time.}

  \item{x}{For \code{.speedStats}, a matrix with a dive's section data.
    A single dive's data: 3-col matrix with time, depth, and speed.  For
    .speedCol, a \code{\link{data.frame}} where names are searched for
    strings matching \code{.speedNames} (see Details).  For
    \code{.derivStats}, a \code{\link{TDRcalibrate}} object.}

  \item{vdist}{Vertical distance travelled during this time.  If
    \code{vdist} is missing, then it's all horizontal movements (no
    angles).}

  \item{sunrise.time, sunset.time}{Passed from \code{plotTDR}.}

  \item{act}{A numeric vector indicating the activity for every element
    of \code{time}.}

  \item{interval}{Sampling interval in seconds.}

  \item{method}{Character string to indicate the method to use for
    zero offset correction.  One of \dQuote{visual}, \dQuote{offset}, or
    \dQuote{filter} (see \sQuote{Details}).}

  \item{control}{Named list of control parameters to be used for ZOC
    method \code{filter} (\code{k}, \code{probs}, \code{na.rm},
    \code{depth.bounds}) and \code{offset} (\code{offset}).}

}


\details{These functions are not meant to be called directly by the
  user, as he/she could not care less (right?).  This may change in the
  future.  Nonetheless, they can be called in the usual way for
  functions that are not exported: \code{diveMove:::.functionName}.

  \code{.depthFilter} performs the zero-offset correction with
  method=\dQuote{filter}.  It takes a numeric vector and applies a
  running quantile to it, and the process is repeated zero or more times
  on the filtered/smooth vector from the previous process.

  \code{.derivStats} obtains summary statistics (summary() and sd()) of
    derivatives for descent, bottom, and ascent phases for each dive.
    It takes a \code{\link{TDRcalibrate}} object.

  \code{.diveIndices} takes a numeric vector with sequences of dive
  numbers, and selects the indices of dive numbers listed uniquely in
  the vector given as its second argument.

  \code{.diveMatches} takes a numeric or character vector with sequences
  of dive numbers, and selects the indices of dive numbers listed
  uniquely in the vector given as its first argument.  Stops if no
  matches are found, and warns if some were ignored.

  \code{.getInterval} takes a \code{\link{POSIXct}} object and
  calculates the the most frequent interval between successive elements.

  \code{.plotTDR} is the underlying plotting function for
  \code{\link{TDR}} objects.  Arguments are documented in the main S4
  method (\code{\link{plotTDR}}).

  \code{.plotZOCfilters} and \code{plotZOCtdrs} are the underlying
  plotting functions for visually assessing the ZOC procedure.
  Arguments are documented in the main S4 methods
  (\code{\link{plotZOC}}).

  \code{.speedStats} takes matrix of time and speed, and an optional
  vector of vertical distance travelled between segments, and calculates
  the total vertical distance travelled, mean speed, and angle.  Input
  and output correspond to a single section of a dive.

  \code{.night} takes a \code{\link{POSIXct}} object, sunrise and sunset
  time strings to provide a list of sunrise and sunset times for each
  day in the \code{\link{POSIXct}} input.

  \code{.rleActivity} takes a factor indicating different activity
  phases, their associated time, and the sampling interval to return a
  factor uniquely identifying each phase of activity, i.e. labelling
  them.  In addition, it returns the duration of each phase, and their
  beginning and end times.

  \code{.speedNames} is a character vector with possible names for a
  speed vector.

  \code{.speedCol} takes a \code{\link{data.frame}} and checks whether
  there is a column with a name matching one of those in
  \code{\link{.speedNames}}.

  \code{.zoc} is the main workhorse for the zero-offset correction
  \acronym{ZOC} procedure in \code{\link{calibrateDepth}}.  It takes a
  \code{\link{POSIXct}} object, corresponding \code{\link{numeric}}
  depth vector, a string representing a method to use for the procedure,
  and a list of control parameters for the chosen method to perform ZOC.

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

\examples{

data(divesTDR)

times <- getTime(divesTDR)
depths <- getDepth(divesTDR)

\donttest{## This can take too long
## .depthFilter

d.filter <- diveMove:::.depthFilter(depth=depths, k=c(3, 5760),
                                    probs=c(0.5, 0.02), depth.bounds=c(0, 5),
                                    na.rm=TRUE)
## Set negative depths to zero
d.filter[d.filter[, 3] < 0, 3] <- 0

## Plotting

idx <- seq(95, 6000)		# try different slices
idx <- seq(nrow(d.filter))
## Look at the top 10 m only plus a few meters above
ylim <- c(-10, 5)

layout(seq(3))
plot(times[idx], -depths[idx], type="l", col="gray",
     ylab="Depth (m)", ylim=ylim)
abline(h=0, lty=2)
legend("topleft", legend="original", lty=1, col="gray")
plot(times[idx], -d.filter[idx, 1], type="l", col=2,
     ylab="Depth (m)", ylim=ylim)
abline(h=0, lty=2)
lines(times[idx], -d.filter[idx, 2])
legend("topleft", legend=colnames(d.filter)[-3],
       lty=1, col=c(2, 1))
plot(times[idx], -d.filter[idx, 3], type="l", ylim=ylim,
     ylab="Depth (m)"); abline(h=0, lty=2)
legend("topleft", legend=paste("Original -", colnames(d.filter)[2]),
       lty=1)
}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{internal}
