\name{diveMove-package}
\docType{package}

\alias{diveMove-package}
\alias{diveMove}
\concept{time depth recorder}
\concept{Dive analysis}
% -------------------------------------------------------------------------

\title{Dive Analysis and Calibration}


\description{This package is a collection of functions for visualizing,
  and analyzing depth and speed data from time-depth recorders
  \acronym{TDR}s.  These can be used to zero-offset correct depth,
  calibrate speed, and divide the record into different phases, or time
  budget.  Functions are provided for calculating summary dive
  statistics for the whole record, or at smaller scales within dives.}


\seealso{A vignette with a guide to this package is available by doing
  \code{vignette("diveMove")}.
  \code{\link[diveMove:diveMove-package]{TDR-class}},
  \code{\link[diveMove:diveMove-package]{calibrateDepth}},
  \code{\link[diveMove:diveMove-package]{calibrateSpeed}},
  \code{\link[diveMove:diveMove-package]{timeBudget}},
  \code{\link[diveMove:diveMove-package]{stampDive}}. }

% -------------------------------------------------------------------------

\examples{

\donttest{## Too long for checks
## read in data and create a TDR object
zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove", mustWork=TRUE)
(sealX <- readTDR(zz, speed=TRUE, sep=";", na.strings="", as.is=TRUE))

if (dev.interactive(orNone=TRUE)) plotTDR(sealX)  # html plotly

## detect periods of activity, and calibrate depth, creating
## a "TDRcalibrate" object
if (dev.interactive(orNone=TRUE)) dcalib <- calibrateDepth(sealX)
## Use the "offset" ZOC method to zero-offset correct depth at 3 m
(dcalib <- calibrateDepth(sealX, zoc.method="offset", offset=3))

if (dev.interactive(orNone=TRUE)) {
    ## plot all readings and label them with the phase of the record
    ## they belong to, excluding surface readings
    plotTDR(dcalib, surface=FALSE)
    ## plot the first 300 dives, showing dive phases and surface readings
    plotTDR(dcalib, diveNo=seq(300), surface=TRUE)
}

## calibrate speed (using changes in depth > 1 m and default remaining arguments)
(vcalib <- calibrateSpeed(dcalib, z=1))

## Obtain dive statistics for all dives detected
dives <- diveStats(vcalib)
head(dives)

## Attendance table
att <- timeBudget(vcalib, FALSE) # taking trivial aquatic activities into account
att <- timeBudget(vcalib, TRUE)  # ignoring them
## Identify which phase each dive belongs to
stamps <- stampDive(vcalib)
sumtab <- data.frame(stamps, dives)
head(sumtab)

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{ package }
