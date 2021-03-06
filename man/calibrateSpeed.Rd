% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calibrate.R
\name{calibrateSpeed}
\alias{calibrateSpeed}
\title{Calibrate and build a "TDRcalibrate" object}
\usage{
calibrateSpeed(
  x,
  tau = 0.1,
  contour.level = 0.1,
  z = 0,
  bad = c(0, 0),
  main = slot(getTDR(x), "file"),
  coefs,
  plot = TRUE,
  postscript = FALSE,
  ...
)
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

\item{main, ...}{Arguments passed to \code{\link{rqPlot}}.}

\item{coefs}{numeric: known speed calibration coefficients from
quantile regression as a vector of length 2 (intercept, slope).  If
provided, these coefficients are used for calibrating speed,
ignoring all other arguments, except \code{x}.}

\item{plot}{logical: whether to plot the results.}

\item{postscript}{logical: whether to produce postscript file output.}
}
\value{
An object of class \code{\link{TDRcalibrate}}.
}
\description{
These functions create a \code{\link{TDRcalibrate}} object which is
necessary to obtain dive summary statistics.
}
\details{
This calibrates speed readings following the procedure outlined in
Blackwell et al. (1999).
}
\examples{
\donttest{## Too long for checks
## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE, run.donttest=TRUE)
dcalib		# the 'TDRcalibrate' that was created

## Calibrate speed using only changes in depth > 2 m
vcalib <- calibrateSpeed(dcalib, z=2)
vcalib

}
}
\references{
Blackwell S, Haverl C, Le Boeuf B, Costa D (1999). A method for calibrating
swim-speed recorders.  Marine Mammal Science 15(3):894-905.
}
\seealso{
\code{\link{TDRcalibrate}}
}
\author{
Sebastian P. Luque \email{spluque@gmail.com}
}
\keyword{manip}
\keyword{math}
