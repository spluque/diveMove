\name{dives}
\docType{data}

\alias{dives}
\alias{divesTDR}
\alias{divesTDRzoc}

% -------------------------------------------------------------------------

\title{Sample of TDR data from a fur seal}



\description{This data set is meant to show a typical organization of a
  \acronym{TDR} *.csv file, suitable as input for \code{\link{readTDR}},
  or to construct a \code{\link{TDR}} object.  \code{divesTDR} is an
  example \code{\link{TDR}} object.}


\format{

  Bzip2-compressed file.  A comma separated value (csv) file with 34199
  \acronym{TDR} readings, measured at 5 s intervals, with the following
  columns:

  \describe{
    \item{date}{Date}

    \item{time}{Time}

    \item{depth}{Depth in m}

    \item{light}{Light level}

    \item{temperature}{Temperature in degrees Celsius}

    \item{speed}{Speed in m/s}
  }

  The data are also provided as a \code{\link{TDR}} object (*.RData
  format) for convenience.

}


\details{

  The data are a subset of an entire \acronym{TDR} record, so they are
  not meant to make valid inferences from this particular
  individual/deployment.

  \code{divesTDR} is a \code{\link{TDR}} object representation of the
  data in \code{dives}.

  \code{divesTDRzoc} is the same data, but has been zero-offset corrected
  with the "filter" method (\code{k=c(3, 5760), probs=c(0.5, 0.02),
  na.rm=TRUE, depth.bounds=range(getDepth(divesTDR))}).


}


\seealso{\code{\link{readTDR}}, \code{\link{diveStats}}.}


\source{Sebastian P. Luque, Christophe Guinet, John P.Y. Arnould}

% -------------------------------------------------------------------------

\examples{

zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove", mustWork=TRUE)
str(read.csv(zz, sep=";", na.strings=""))

}


\keyword{datasets}
