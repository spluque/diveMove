\name{rqPlot}

\alias{rqPlot}

% -------------------------------------------------------------------------

\title{Plot of quantile regression for speed calibrations}



\description{Plot of quantile regression for assessing quality of speed
  calibrations}


\usage{
rqPlot(rddepth, speed, z, contours, rqFit, main="qtRegression",
       xlab="rate of depth change (m/s)", ylab="speed (m/s)",
       colramp=colorRampPalette(c("white", "darkblue")),
       col.line="red", cex.pts=1)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{speed}{Speed in m/s.}

  \item{rddepth}{Numeric vector with rate of depth change.}

  \item{z}{A list with the bivariate kernel density estimates (1st
    component the x points of the mesh, 2nd the y points, and 3rd the
    matrix of densities).}

  \item{contours}{List with components: \code{pts} which should be a
    matrix with columns named \code{x} and \code{y}, \code{level} a
    number indicating the contour level the points in \code{pts}
    correspond to.}

  \item{rqFit}{Object of class \dQuote{rq} representing a quantile
    regression fit of rate of depth change on mean speed.}

  \item{main}{String; title prefix to include in ouput plot.}

  \item{xlab, ylab}{axis labels.}

  \item{colramp}{Function taking an integer n as an argument and
    returning n colors.}

  \item{col.line}{Color to use for the regression line.}

  \item{cex.pts}{A numerical value specifying the amount by which to
    enlarge the size of points.}

}


\details{The dashed line in the plot represents a reference indicating a
  one to one relationship between speed and rate of depth change.  The
  other line represent the quantile regression fit.}


\seealso{\code{\link{diveStats}}}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{arith}
\keyword{hplot}
