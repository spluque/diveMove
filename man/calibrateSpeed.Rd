\name{calibrateSpeed}
\alias{calibrateSpeed}

% -------------------------------------------------------------------------

\title{Calibrate and build a "TDRcalibrate" object}


\description{These functions create a \code{\link{TDRcalibrate}} object
  which is necessary to obtain dive summary statistics.}


\usage{
calibrateSpeed(x, tau=0.1, contour.level=0.1, z=0, bad=c(0, 0),
               main=slot(getTDR(x), "file"), coefs, plot=TRUE,
               postscript=FALSE, \ldots)
}


\arguments{

  \item{x}{An object of class \code{\link{TDR}} for
    \code{\link{calibrateDepth}} or an object of class
    \code{\link{TDRcalibrate}} for \code{\link{calibrateSpeed}}.}

  \item{tau}{numeric scalar: quantile on which to regress speed on rate
    of depth change; passed to \code{\link[quantreg]{rq}}.}

  \item{contour.level}{numeric scalar: the mesh obtained from the
    bivariate kernel density estimation corresponding to this contour
    will be used for the quantile regression to define the calibration
    line.}

  \item{z}{numeric scalar: only changes in depth larger than this value
    will be used for calibration.}

  \item{bad}{numeric vector of length 2 indicating that only rates of
    depth change and speed greater than the given value should be used
    for calibration, respectively.}

  \item{coefs}{numeric: known speed calibration coefficients from
    quantile regression as a vector of length 2 (intercept, slope).  If
    provided, these coefficients are used for calibrating speed,
    ignoring all other arguments, except \code{x}.}

  \item{main, \ldots}{Arguments passed to \code{\link{rqPlot}}.}

  \item{plot}{logical: whether to plot the results.}

  \item{postscript}{logical: whether to produce postscript file output.}

}


\details{This calibrates speed readings following the procedure outlined
  in Blackwell et al. (1999).}


\value{An object of class \code{\link{TDRcalibrate}}.}


\seealso{\code{\link{TDRcalibrate}}}


\references{

  Blackwell S, Haverl C, Le Boeuf B, Costa D (1999). A method for
  calibrating swim-speed recorders.  Marine Mammal Science
  15(3):894-905.

}

% -------------------------------------------------------------------------

\examples{

## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE)
dcalib		# the 'TDRcalibrate' that was created

## Calibrate speed using only changes in depth > 2 m
vcalib <- calibrateSpeed(dcalib, z=2)
vcalib

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{math}% __ONLY ONE__ keyword per line
