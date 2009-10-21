% $Id$
\name{calibrateDepth}
\alias{calibrateDepth}

% -------------------------------------------------------------------------

\title{Calibrate Depth and Generate a "TDRcalibrate" object}


\description{Detect periods of major activities in a \acronym{TDR}
  record, calibrate depth readings, and generate a
  \code{\link{TDRcalibrate}} object essential for subsequent summaries
  of diving behaviour.}


\usage{
calibrateDepth(x, dry.thr=70, wet.thr=3610, dive.thr=4, offset,
               descent.crit.q=0.1, ascent.crit.q=0.1, wiggle.tol=0.8)
}


\arguments{

  \item{x}{An object of class \code{\link{TDR}} for
    \code{\link{calibrateDepth}} or an object of class
    \code{\link{TDRcalibrate}} for \code{\link{calibrateSpeed}}.}

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

  The procedure starts by first creating a factor with value \dQuote{L}
  (dry) for rows with NAs for \code{depth} and value \dQuote{W} (wet)
  otherwise. It subsequently calculates the duration of each of these
  phases of activity.  If the duration of an dry phase (\dQuote{L}) is
  less than \code{dry.thr}, then the values in the factor for that phase
  are changed to \dQuote{W} (wet).  The duration of phases is then
  recalculated, and if the duration of a phase of wet activity is less
  than \code{wet.thr}, then the corresponding value for the factor is
  changed to \dQuote{Z} (trivial wet).  The durations of all phases are
  recalculated a third time to provide final phase durations.

  The next step is to detect dives whenever the zero-offset corrected
  depth in an underwater phase is below the supplied dive threshold.  A
  new factor with finer levels of activity is thus generated, including
  \dQuote{U} (underwater), and \dQuote{D} (diving) in addition to the
  ones described above.

  Once dives have been detected and assigned to a period of wet
  activity, phases within dives are detected using the descent, ascent
  and wiggle criteria.  This procedure generates a factor with levels
  \dQuote{D}, \dQuote{DB}, \dQuote{B}, \dQuote{BA}, \dQuote{A},
  \dQuote{DA}, and \dQuote{X}, breaking the input into descent,
  descent/bottom, bottom, bottom/ascent, ascent, and non-dive,
  respectively.

}

\section{Detection of dive phases}{

  A bottom depth is defined as the maximum depth multiplied by a factor
  (\code{wiggle.tol}, [0, 1])

  \describe{

    \item{Descent}{Using all depths from the first one in the dive down
    to the maximum depth, the rate of descent for each segment is
    calculated, and a critical rate is defined as the quantile
    (\code{descent.crit.q}) of all the positive rates of descent.  This
    subsetting avoids defining a critical rate that may be negative, due
    to wiggling during the descent.

    To allow detection of wiggles during descent, a vector of the
    indices of the rates of descent that were lower than the critical
    value is defined, and a logical vector with TRUE for rates of
    descent >= 0 above the bottom depth defined previously is also
    created.

    The following tests are performed (in order):

    If there were any rates below the critical value, as well as any
    descent wiggles, the indices where the wiggles occurred are removed.
    If this resulted in removal of all indices, then the index defining
    the end of descent is the number of rates of descent, otherwise it
    is the last after the removal.

    If there were not any rates below the critical value, the index
    defining the end of descent is the number of rates of descent,
    otherwise it is the first of them.}


  \item{Ascent}{The order of depths is reversed in order to detect
    ascent starting from the bottom, taking all depths after the maximum
    depth.  The rate of ascent for each segment is calculated, and a
    critical rate is defined as the quantile (\code{ascent.crit.q}) of
    all the positive rates of ascent, analogously to the descent
    detection procedure.

    To allow detection of bottom wiggling, a vector of the indices of
    the rates of ascent that were higher than the critical value is
    defined, and a logical vector with TRUE for rates of ascent <= 0
    below the bottom depth defined previously is also created.

    The following tests are performed (in order):

    If there were any bottom wiggles, then the index defining the
    beginning of ascent is that corresponding to the maximum depth plus
    that corresponding to the last bottom wiggle, otherwise:

    If there were no rates above the critical value, then the index
    defining the beginning of ascent is the last reading below the
    surface.  Otherwise, it is the one corresponding to the maximum
    depth plus the first of the indices of rates above the critical
    value.}

}

The particular dive phase categories are subsequently defined using
simple set operations.

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


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}% __ONLY ONE__ keyword per line
