% $Id$
\name{plotTD}

\alias{plotTD}

% -------------------------------------------------------------------------

\title{Interactive plotting and zero-offset correction windows for "TDR"
  data}


\description{Plot and optionally set zero-offset correction windows in
  \acronym{TDR} records, with the aid of a graphical user interface
  (GUI), allowing for dynamic selection of offset and multiple time
  windows to perform the adjustment.}


\usage{
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


\details{This function is used primarily to correct drifts in the
  pressure transducer of \acronym{TDR} records and noise in depth
  measurements via method=\dQuote{visual} in
  \code{\link{calibrateDepth}}.}


\value{A list (invisibly) with as many components as sections of the
  record that were zero-offset corrected, each consisting of two further
  lists with the same components as those returned by
  \code{\link{locator}}.}


\seealso{\code{\link{calibrateDepth}}, and \code{\link{plotTDR}}.}

% -------------------------------------------------------------------------

\examples{

data(divesTDR)

## Use interact=TRUE (default) to set the offset interactively
plotTD(getTime(divesTDR), getDepth(divesTDR), interact=FALSE)

}


\author{Sebastian P. Luque \email{spluque@gmail.com},
  with many ideas from CRAN package sfsmisc.}


\keyword{iplot}% at least one, from doc/KEYWORDS
