\name{zoc}

\alias{zoc}
\alias{plotTD}

% -------------------------------------------------------------------------

\title{Interactive zero-offset correction of TDR data}


\description{Correct zero-offset in TDR records, with the aid of a
  graphical user interface (GUI), allowing for dynamic selection of
  offset and multiple time windows to perform the adjustment.}


\usage{
zoc(time, depth, offset)
plotTD(time, depth, concurVars=NULL, xlim=NULL, depth.lim=NULL,
       xlab="time (dd-mmm hh:mm)", ylab.depth="depth (m)",
       concurVarTitles=deparse(substitute(concurVars)),
       xlab.format="\%d-\%b \%H:\%M", sunrise.time="06:00:00",
       sunset.time="18:00:00", night.col="gray60",
       phaseCol=NULL, interact=TRUE, key=TRUE, cex.pts=0.4, \ldots)

}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{time}{\code{POSIXct} object with date and time.}

  \item{depth}{Numeric vector with depth in m.}

  \item{offset}{Known amount of meters to subtract for zero-offset
    correcting depth throughout the entire TDR record.}

  \item{concurVars}{Matrix with additional variables in each column to
    plot concurrently with depth.}

  \item{xlim}{Vector of length 2, with lower and upper limits of time to
    be plotted.}

  \item{depth.lim}{Numeric vector of length 2, with the lower and upper
    limits of depth to be plotted.}

  \item{xlab, ylab.depth}{Strings to label the corresponding
    y-axes.}

  \item{concurVarTitles}{Character vector of titles to label each new
    variable given in \var{concurVars}.}

  \item{xlab.format}{Format string for formatting the x axis; see
    \code{\link{strptime}}.}

  \item{sunrise.time, sunset.time}{Character string with time of sunrise
    and sunset, respectively, in 24 hr format.  This is used for shading
    night time.}

  \item{night.col}{Color for shading night time.}

  \item{phaseCol}{Factor dividing rows into sections.}

  \item{interact}{Logical; whether to provide interactive tcltk controls
    and access to the associated ZOC functionality.}

  \item{key}{Logical indicating whether to draw a key.}

  \item{cex.pts}{Passed to \code{\link{points}} to set the relative size
    of points to plot (if any).}

  \item{\ldots}{Arguments passed to \code{\link{par}}; useful defaults
    \code{las=1}, \code{bty="n"}, and \code{mar} (the latter depending
    on whether additional concurrent data will be plotted) are provided,
    but they can be overridden.}

}


\details{These functions are used primarily to correct, visually, drifts
  in the pressure transducer of TDR records. \code{zoc} calls
  \code{plotDive}, which plots depth and, optionally, speed vs. time
  with the possibility zooming in and out on time, changing maximum
  depths displayed, and panning through time. The option to zero-offset
  correct sections of the record gathers x and y coordinates for two
  points, obtained by clicking on the plot region. The first point
  clicked indicates the offset and beginning time of section to correct,
  and the second one indicates the ending time of the section to
  correct.  Multiple sections of the record can be corrected in this
  manner, by panning through the time and repeating the procedure.  In
  case there's overlap between zero offset corrected windows, the last
  one prevails.

  Once the whole record has been zero-offset corrected, remaining points
  with depth values lower than zero, are turned into zeroes, as these
  are assumed to be values at the surface.}


\value{ \code{zoc} returns a numeric vector, as long as \code{depth} of
  zero-offset corrected depths.

  \code{plotTD} returns (invisibly) a list with as many components as
  sections of the record that were zero-offset corrected, each
  consisting of two further lists with the same components as those
  returned by \code{\link{locator}}.}


\seealso{\code{\link{calibrateDepth}}, and \code{\link{plotTDR}}.}

% -------------------------------------------------------------------------

\examples{

data(divesTDR)

## Use interact=TRUE (default) to set the offset interactively
depth.zoc <- zoc(getTime(divesTDR), getDepth(divesTDR), offset=3)
plotTD(getTime(divesTDR), depth.zoc, interact=FALSE)

}



\author{Sebastián P. Luque \email{spluque@gmail.com}, with many ideas
  from CRAN package sfsmisc.}


\keyword{iplot}% at least one, from doc/KEYWORDS
