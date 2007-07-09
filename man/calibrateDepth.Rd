\name{calibrateDepth}
\encoding{latin1}
\alias{calibrateDepth}

% -------------------------------------------------------------------------

\title{Calibrate Depth and Generate a "TDRcalibrate" object}



\description{Detect periods of major activities in a TDR record,
  calibrate depth readings, and generate a "TDRcalibrate" object
  essential for subsequent summaries of diving behaviour.}


\usage{
calibrateDepth(x, dry.thr=70, wet.thr=3610, dive.thr=4, offset,
               descent.crit.q=0.1, ascent.crit.q=0.1, wiggle.tol=0.8)
}


\arguments{

  \item{x}{An object of class \code{TDR} for
    \code{\link{calibrateDepth}} or an object of class
    \code{TDRcalibrate} for \code{\link{calibrateSpeed}}.}

  \item{dry.thr}{Dry error threshold in seconds.  Dry phases shorter
    than this threshold will be considered as wet.}

  \item{wet.thr}{Wet threshold in seconds. At-sea phases shorter than
    this threshold will be considered as trivial wet.}

  \item{dive.thr}{Threshold depth below which an underwater phase
    should be considered a dive.}

  \item{offset}{Argument to \code{\link{zoc}}.  If not provided, the
    offset is obtained using an interactive plot of the data.}

  \item{descent.crit.q}{Critical quantile of rates of descent below
    which descent is deemed to have ended.}

  \item{ascent.crit.q}{Critical quantile of rates of ascent above which
    ascent is deemed to have started.}

  \item{wiggle.tol}{Proportion of maximum depth above which wiggles
    should not be allowed to define the end of descent.  It's also the
    proportion of maximum depth below which wiggles should be considered
    part of bottom phase.}

}


\details{This function is really a wrapper around \code{.detPhase} and
  \code{.detDive}, which perform the work on simplified objects.  It
  performs zero-offset correction of depth, wet/dry phase detection, and
  detection of dives, as well as proper labelling of the latter.

  The procedure starts by first creating a factor with value \sQuote{L}
  (dry) for rows with NAs for \code{depth} and value \sQuote{W} (wet)
  otherwise. It subsequently calculates the duration of each of these
  phases of activity.  If the duration of an dry phase (\sQuote{L}) is
  less than \code{dry.thr}, then the values in the factor for that phase
  are changed to \sQuote{W} (wet).  The duration of phases is then
  recalculated, and if the duration of a phase of wet activity is less
  than \code{wet.thr}, then the corresponding value for the factor is
  changed to \sQuote{Z} (trivial wet).  The durations of all phases are
  recalculated a third time to provide final phase durations.

  The next step is to detect dives whenever the zero-offset corrected
  depth in an underwater phase is below the supplied dive threshold.  A
  new factor with finer levels of activity is thus generated, including
  \sQuote{U} (underwater), and \sQuote{D} (diving) in addition to the
  ones described above.

  Once dives have been detected and assigned to a period of wet
  activity, phases within dives are detected using the descent, ascent
  and wiggle criteria.  This procedure generates a factor with levels
  \dQuote{D}, \dQuote{DB}, \dQuote{B}, \dQuote{BA}, \dQuote{A},
  \dQuote{DA}, and \dQuote{X}, breaking the input into descent,
  descent/bottom, bottom, bottom/ascent, ascent, and non-dive,
  respectively.

}


\value{An object of class \code{\link{TDRcalibrate}}.}


\seealso{\code{\link{TDRcalibrate}}, \code{\link{zoc}}}

% -------------------------------------------------------------------------

\examples{

data(divesTDR)
divesTDR

## Consider a 3 m offset, and a dive threshold of 3 m
dcalib <- calibrateDepth(divesTDR, dive.thr=3, offset=3)
if (dev.interactive(orNone=TRUE)) {
    plotTDR(dcalib, labels="dive.phase", surface=TRUE)
}

}


\author{\enc{Sebastián}{Sebastian} P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}% __ONLY ONE__ keyword per line
