\name{dives}
\docType{data}

\alias{dives}
\alias{divesTDR}
\alias{divesTDRcalibrate}
\alias{divesSummary}

% -------------------------------------------------------------------------

\title{Sample TDR data from a fur seal}



\description{This data set is meant to show a typical organization of
  a TDR *.csv file, suitable as input for \code{\link{readTDR}}, or to
  construct a \code{\link{TDR}} object.  \code{divesTDR} and
  \code{divesTDRcalibrate} are example \code{\link{TDR}} and
  \code{\link{TDRcalibrate}} objects.}


\format{

  A comma separated value (csv) file with 69560 TDR readings with the
  following columns:

  \item{date}{Date}

  \item{time}{Time}

  \item{depth}{Depth in m}

  \item{light}{Light level}

  \item{temperature}{Temperature in C}

  \item{speed}{Speed in m/s}

  The data are also provided as a TDR object (*.RData format) for
  convenience.
}


\details{

  The data are a subset of an entire TDR record, so they are not meant
  to make valid inferences from this particular individual/deployment.

  \code{divesTDR} is a \code{\link{TDR}} object representation of the
  data in \code{dives}.

  \code{divesTDRcalibrate} is a \code{\link{TDRcalibrate}} object
  representing the data in dives, calibrated at default criteria (see
  \code{\link{calibrateDepth}}), and 3 m offset.

  \code{divesSummary} is a data frame containing a summary of all dives
  in this dataset (see \code{\link{diveStats}} and
  \code{\link{stampDive}} for the information contained in this object.

}


\seealso{\code{\link{readTDR}}, \code{\link{diveStats}}.}


\source{Sebastián P. Luque, Christophe Guinet, John P.Y. Arnould}

% -------------------------------------------------------------------------

\keyword{datasets}
