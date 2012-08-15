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
calibrateDepth(x, dry.thr=70, wet.cond, wet.thr=3610, dive.thr=4,
               zoc.method=c("visual", "offset", "filter"), \ldots,
               interp.wet=FALSE, smooth.par=0.1, knot.factor=3,
               descent.crit.q=0, ascent.crit.q=0)
}


\arguments{

  \item{x}{An object of class \code{\link{TDR}} for
    \code{\link{calibrateDepth}} or an object of class
    \code{\link{TDRcalibrate}} for \code{\link{calibrateSpeed}}.}

  \item{dry.thr}{numeric: dry error threshold in seconds.  Dry phases
    shorter than this threshold will be considered as wet.}

  \item{wet.cond}{logical: indicates which observations should be
    considered wet.  If it is not provided, records with non-missing
    depth are assumed to correspond to wet conditions (see
    \sQuote{Details} and \sQuote{Note} below).}

  \item{wet.thr}{numeric: wet threshold in seconds. At-sea phases
    shorter than this threshold will be considered as trivial wet.}

  \item{dive.thr}{numeric: threshold depth below which an underwater
    phase should be considered a dive.}

  \item{zoc.method}{character string to indicate the method to use for
    zero offset correction.  One of \dQuote{visual}, \dQuote{offset}, or
    \dQuote{filter} (see \sQuote{Details}).}

  \item{\ldots}{Arguments required for ZOC methods \code{filter}
    (\code{k}, \code{probs}, \code{depth.bounds} (defaults to range),
    \code{na.rm} (defaults to TRUE)) and \code{offset} (\code{offset}).}

  \item{interp.wet}{logical: if TRUE (default is FALSE), then an
    interpolating spline function is used to impute NA depths in wet
    periods (\emph{after ZOC}).  \emph{Use with caution}: it may only be
    useful in cases where the missing data pattern in wet periods is
    restricted to shallow depths near the beginning and end of dives.
    This pattern is common in some satellite-linked \acronym{TDR}s.}

  \item{smooth.par}{numeric scalar representing amount of smoothing
    (argument \code{spar} in \code{\link[stats]{smooth.spline}}.  If it
    is NULL, then the smoothing parameter is determined by Generalized
    Cross-validation (GCV).}

  \item{knot.factor}{numeric scalar that multiplies the number of
    samples in the dive.  This is used to construct the time predictor
    for the derivative.}

  \item{descent.crit.q}{numeric: critical quantile of rates of descent
    below which descent is deemed to have ended.}

  \item{ascent.crit.q}{numeric: critical quantile of rates of ascent
    above which ascent is deemed to have started.}

}


\details{This function is really a wrapper around \code{.detPhase},
  \code{.detDive}, and \code{.zoc} which perform the work on simplified
  objects.  It performs wet/dry phase detection, zero-offset correction
  of depth, and detection of dives, as well as proper labelling of the
  latter.

  The procedure starts by zero-offset correcting depth (see \sQuote{ZOC}
  below), and then a factor is created with value \dQuote{L} (dry) for
  rows with NAs for \code{depth} and value \dQuote{W} (wet) otherwise.
  This assumes that \acronym{TDR}s were programmed to turn off recording
  of depth when instrument is dry (typically by means of a salt-water
  switch).  If this assumption cannot be made for any reason, then a
  logical vector as long as the time series should be supplied as
  argument \code{wet.cond} to indicate which observations should be
  considered wet.  This argument is directly analogous to the
  \code{subset} argument in \code{\link{subset.data.frame}}, so it can
  refer to any variable in the \code{\link{TDR}} object (see
  \sQuote{Note} section below).  The duration of each of these phases of
  activity is subsequently calculated.  If the duration of a dry phase
  (\dQuote{L}) is less than \code{dry.thr}, then the values in the
  factor for that phase are changed to \dQuote{W} (wet).  The duration
  of phases is then recalculated, and if the duration of a phase of wet
  activity is less than \code{wet.thr}, then the corresponding value for
  the factor is changed to \dQuote{Z} (trivial wet).  The durations of
  all phases are recalculated a third time to provide final phase
  durations.

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
  \dQuote{B}, \dQuote{BA}, \dQuote{DA}, \dQuote{A}, and \dQuote{X},
  breaking the input into descent, descent/bottom, bottom,
  bottom/ascent, ascent, descent/ascent (ocurring when no bottom phase
  can be detected) and non-dive (surface), respectively.

}

\section{ZOC}{This procedure is required to correct drifts in the
  pressure transducer of \acronym{TDR} records and noise in depth
  measurements.  Three methods are available to perform this correction.

  Method \dQuote{visual} calls \code{\link{plotTDR}}, which plots depth
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
  recursive manner (Luque and Fried 2011), using
  \code{.depth.filter}. It relies on function
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

  The process for each dive begins by taking all observations below the
  dive detection threshold, and setting the beginning and end depths to
  zero, at time steps prior to the first and after the last,
  respectively.  The latter ensures that descent and ascent derivatives
  are non-negative and non-positive, respectively, so that the end and
  beginning of these phases are not truncated.  A smoothing spline is
  used to model the dive and its derivative to investigate its changes
  in vertical rate.  This method requires at least 4 observations (see
  \code{\link[stats]{smooth.spline}}), so the time series is linearly
  interpolated at equally spaced time steps if this limit is not
  achieved in the current dive.  Wiggles at the beginning and end of the
  dive are assumed to be zero offset correction errors, so depth
  observations at these extremes are interpolated between zero and the
  next observations when this occurs.

  A set of knots is established to fit the smoothing spline by using an
  regular time sequence with beginning and end equal to the extremes of
  the input sequence, and with length equal to \eqn{N \times
  knot.factor}{N * \code{knot.factor}}.  The first derivate of the
  spline is evaluated at the same set of knots to calculate the vertical
  rate throughout the dive and determine the end of descent and
  beginning of ascent.  Equivalent procedures are used for detecting
  descent and ascent phases.

  The quantile corresponding to (\code{descent.crit.q}) of all the
  positive derivatives (rate of descent) at the beginning of the dive is
  used as threshold for determining the end of descent.  Descent is
  deemed to have ended at the \emph{first} minimum derivative, and the
  nearest input time observation is considered to indicate the end of
  descent.  The sign of the comparisons is reversed for detecting the
  ascent.  If observed depth to the left and right of the derivative
  defining the ascent are the same, the right takes precedence.

  The particular dive phase categories are subsequently defined using
  simple set operations.

}

\note{Note that the condition implied with argument \code{wet.cond} is
  evaluated after the ZOC procedure, so it can refer to corrected depth.
  In many cases, not all variables in the \code{\link{TDR}} object are
  sampled with the same frequency, so they may need to be interpolated
  before using them for this purpose.  Note also that any of these
  variables may contain similar problems as those dealth with during
  ZOC, so programming instruments to record depth only when wet is
  likely the best way to ensure proper detection of wet/dry conditions.}

\references{

  Luque, S.P. and Fried, R. (2011) Recursive filtering for zero offset
  correction of diving depth time series. PLoS ONE 6:e15850

}


\value{An object of class \code{\link{TDRcalibrate}}.}


\seealso{\code{\link{TDRcalibrate}}, \code{\link{.zoc}},
  \code{\link{.depthFilter}}, \code{\link{.detPhase}},
  \code{\link{.detDive}}, \code{\link{plotTDR}}, and
  \code{\link{plotZOC}} to visually assess ZOC procedure.}

% -------------------------------------------------------------------------

\examples{

data(divesTDR)
divesTDR

## Consider a 3 m offset, a dive threshold of 3 m, the 1\% quantile for
## critical vertical rates, and a set of knots 20 times as long as the
## observed time steps.
(dcalib <- calibrateDepth(divesTDR, dive.thr=3, zoc.method="offset",
                          offset=3, descent.crit.q=0.01, ascent.crit.q=0,
                          knot.factor=20))
## Or ZOC algorithmically with method="filter":
\dontrun{## This can take a while due to large window needed for 2nd
## filter in this dataset
(dcalib <- calibrateDepth(divesTDR, dive.thr=3, zoc.method="filter",
                          k=c(3, 5760), probs=c(0.5, 0.02), na.rm=TRUE,
                          descent.crit.q=0.01, ascent.crit.q=0,
                          knot.factor=20))
}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}% __ONLY ONE__ keyword per line
