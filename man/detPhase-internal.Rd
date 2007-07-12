\name{detPhase-internal}
\alias{detPhase-internal}

\alias{.detPhase}

% -------------------------------------------------------------------------

\title{Detect phases of activity from depth readings}



\description{Functions to identify sections of a TDR record displaying
  one of three possible activities: dry, wet, and trivial wet.}


\usage{
.detPhase(time, depth, dry.thr, wet.thr, \dots)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{time}{\code{POSIXct} object with date and time for all depths.}

  \item{depth}{Numeric vector with depth readings.}

  \item{dry.thr, wet.thr}{Passed from \code{\link{calibrateDepth}}.}

  \item{\dots}{Passed from \code{\link{calibrateDepth}}; sampling
    interval in seconds.}

}


\details{\code{.detPhase} first creates a factor with value \sQuote{L}
  (dry) for rows with NAs for \code{depth} and value \sQuote{W} (wet)
  otherwise. It subsequently calculates the duration of each of these
  phases of activity.  If the duration of an dry phase (\sQuote{L}) is
  less than \code{dry.thr}, then the values in the factor for that phase
  are changed to \sQuote{W} (wet).  The duration of phases is then
  recalculated, and if the duration of a phase of wet activity is less
  than \code{wet.thr}, then the corresponding value for the factor is
  changed to \sQuote{Z} (trivial wet).  The durations of all phases are
  recalculated a third time to provide final phase durations.}


\value{A list with components:

  \item{phase.id}{Numeric vector identifying each activity phase,
    starting from 1 for every input record.}

  \item{activity}{Factor with levels \sQuote{L} indicating dry,
    \sQuote{W} indicating wet, \sQuote{U} for underwater (above dive
    criterion), \sQuote{D} for diving, \sQuote{Z} for trivial wet animal
    activities.  Only \sQuote{L}, \sQuote{W}, and \sQuote{Z} are
    actually represented.}

  \item{begin}{A \code{POSIXct} object as long as the number of unique
    activity phases identified, indicating the start times for each
    activity phase.}

  \item{end}{A \code{POSIXct} object as long as the number of unique
    activity phases identified, indicating the end times for each
    activity phase.}

}


\seealso{\code{\link{.detDive}}}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com} and Andy Liaw.}


\keyword{internal}% at least one, from doc/KEYWORDS
