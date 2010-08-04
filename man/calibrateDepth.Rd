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
calibrateDepth(x, dry.thr=70, wet.thr=3610, dive.thr=4,
               zoc.method=c("visual", "offset", "filter"), \ldots,
               interp.wet=FALSE, descent.crit.q=0.1,
               ascent.crit.q=0.1, wiggle.tol=0.8)
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

  \item{zoc.method}{Character string to indicate the method to use for
    zero offset correction.  One of \dQuote{visual}, \dQuote{offset}, or
    \dQuote{filter} (see \sQuote{Details}).}

  \item{\ldots}{Arguments required for ZOC methods \code{filter}
    (\code{k}, \code{probs}, \code{depth.bounds} (defaults to range),
    \code{na.rm} (defaults to TRUE)) and \code{offset} (\code{offset}).}

  \item{interp.wet}{Logical.  If TRUE (default is FALSE), then an
  interpolating spline function is used to impute NA depths in wet
  periods (\emph{after ZOC}).  \emph{Use with caution}: it may only be
  useful in cases where the missing data pattern in wet periods is
  restricted to shallow depths near the beginning and end of dives.
  This pattern is common in some satellite-linked \acronym{TDR}s.}

  \item{descent.crit.q}{Critical quantile of rates of descent below
    which descent is deemed to have ended.}

  \item{ascent.crit.q}{Critical quantile of rates of ascent above which
    ascent is deemed to have started.}

  \item{wiggle.tol}{Proportion of maximum depth above which wiggles
    should not be allowed to define the end of descent.  It's also the
    proportion of maximum depth below which wiggles should be considered
    part of bottom phase.}

}


\details{This function is really a wrapper around \code{.detPhase},
  \code{.detDive}, and \code{.zoc} which perform the work on simplified
  objects.  It performs wet/dry phase detection, zero-offset correction
  of depth, and detection of dives, as well as proper labelling of the
  latter.

  The procedure starts by first creating a factor with value \dQuote{L}
  (dry) for rows with NAs for \code{depth} and value \dQuote{W} (wet)
  otherwise. It subsequently calculates the duration of each of these
  phases of activity.  If the duration of a dry phase (\dQuote{L}) is
  less than \code{dry.thr}, then the values in the factor for that phase
  are changed to \dQuote{W} (wet).  The duration of phases is then
  recalculated, and if the duration of a phase of wet activity is less
  than \code{wet.thr}, then the corresponding value for the factor is
  changed to \dQuote{Z} (trivial wet).  The durations of all phases are
  recalculated a third time to provide final phase durations.

  Zero-offset correction of depth is performed at this stage (see
  \sQuote{ZOC} below).

  Some instruments produce a peculiar pattern of missing data near the
  surface, at the beginning and/or end of dives.  The argument
  \code{interp.wet} may help to rectify this problem by using an
  interpolating spline function to impute the missing data, constraining
  the result to a minimum depth of zero.  Please note that this optional
  step is performed after ZOC and before identifying dives, so that
  interpolation is performed through dry phases coded as wet because
  their duration was briefer than \code{dry.thr}.  Therefore,
  \code{dry.thr} must be chosen carefully to avoid interpolation through
  legitimate dry periods.
  
  The next step is to detect dives whenever the zero-offset corrected
  depth in an underwater phase is below the specified dive threshold.  A
  new factor with finer levels of activity is thus generated, including
  \dQuote{U} (underwater), and \dQuote{D} (diving) in addition to the
  ones described above.

  Once dives have been detected and assigned to a period of wet
  activity, phases within dives are identified using the descent, ascent
  and wiggle criteria (see \sQuote{Detection of dive phases} below).
  This procedure generates a factor with levels \dQuote{D}, \dQuote{DB},
  \dQuote{B}, \dQuote{BA}, \dQuote{A}, \dQuote{DA}, and \dQuote{X},
  breaking the input into descent, descent/bottom, bottom,
  bottom/ascent, ascent, and non-dive, respectively.

}

\section{ZOC}{This procedure is required to correct drifts in the
  pressure transducer of \acronym{TDR} records and noise in depth
  measurements.  Three methods are available to perform this correction.

  Method \dQuote{visual} calls \code{\link{plotTD}}, which plots depth
  and, optionally, speed vs. time with the ability of zooming in and out
  on time, changing maximum depths displayed, and panning through time.
  The button to zero-offset correct sections of the record allows for
  the collection of \sQuote{x} and \sQuote{y} coordinates for two
  points, obtained by clicking on the plot region. The first point
  clicked represents the offset and beginning time of section to
  correct, and the second one represents the ending time of the section
  to correct.  Multiple sections of the record can be corrected in this
  manner, by panning through the time and repeating the procedure.  In
  case there's overlap between zero offset corrected windows, the last
  one prevails.

  Method \dQuote{offset} can be used when the offset is known in
  advance, and this value is used to correct the entire time series.
  Therefore, offset=0 specifies no correction.

  Method \dQuote{filter} implements a smoothing/filtering mechanism
  where running quantiles can be applied to depth measurements in a
  recursive manner, using \code{.depth.filter}. It relies on function
  \code{\link[caTools]{runquantile}} from the \code{caTools} package.
  The method calculates the first running quantile defined by
  \code{probs[1]} on a moving window of size \code{k[1]}.  The next
  running quantile, defined by \code{probs[2]} and \code{k[2]}, is
  applied to the smoothed/filtered depth measurements from the previous
  step, and so on.  The corrected depth measurements (d) are calculated
  as:

  \deqn{d=d_{0} - d_{n}}{d=d[0] - d[n]}

  where \eqn{d_{0}}{d[0]} is original depth and \eqn{d_{n}}{d[n]} is the
  last smoothed/filtered depth.  This method is under development, but
  reasonable results can be achieved by applying two filters (see
  \sQuote{Examples}).  The default \code{na.rm=TRUE} works well when
  there are no level shifts between non-NA phases in the data, but
  \code{na.rm=FALSE} is better in the presence of such shifts.  In other
  words, there is no reason to pollute the moving window with NAs when
  non-NA phases can be regarded as a continuum, so splicing non-NA
  phases makes sense.  Conversely, if there are level shifts between
  non-NA phases, then it is better to retain NA phases to help the
  algorithm recognize the shifts while sliding the window(s).  The
  search for the surface can be limited to specified bounds during
  smoothing/filtering, so that observations outside these bounds are
  interpolated using the bounded smoothed/filtered series.

  Once the whole record has been zero-offset corrected, remaining depths
  below zero, are set to zero, as these are assumed to indicate values
  at the surface.

}


\section{Detection of dive phases}{

  A bottom depth is defined as the maximum depth multiplied by factor
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


\references{

  Luque, S.P. and Fried, R. (Submitted) Recursive filtering for zero
  offset correction of diving depth time series.

}


\value{An object of class \code{\link{TDRcalibrate}}.}


\seealso{\code{\link{TDRcalibrate}}, \code{\link{.zoc}},
  \code{\link{.depthFilter}} \code{\link{.detPhase}},
  \code{\link{.detDive}}.}

% -------------------------------------------------------------------------

\examples{

data(divesTDR)
divesTDR

## Consider a 3 m offset, and a dive threshold of 3 m
dcalib <- calibrateDepth(divesTDR, dive.thr=3, zoc.method="offset",
                         offset=3)
## Or ZOC algorithmically with method="filter":
\dontrun{## This can take a while due to large window needed for 2nd
## filter in this dataset
dcalib <- calibrateDepth(divesTDR, dive.thr=3, zoc.method="filter",
                         k=c(3, 5760), probs=c(0.5, 0.02), na.rm=TRUE)
}
if (dev.interactive(orNone=TRUE)) {
    plotTDR(dcalib, labels="dive.phase", surface=TRUE)
}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}% __ONLY ONE__ keyword per line
