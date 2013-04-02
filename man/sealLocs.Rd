% $Id$
\name{sealLocs}
\docType{data}

\alias{sealLocs}

% -------------------------------------------------------------------------

\title{Ringed and Gray Seal ARGOS Satellite Location Data}


\description{Satellite locations of a gray (Stephanie) and a ringed
(Ringy) seal caught and released in New York.}


\format{Bzip2-compressed file.  A \code{\link{data.frame}} with the
  following information:

  \describe{
    \item{id}{String naming the seal the data come from.}

    \item{time}{The date and time of the location.}

    \item{class}{The ARGOS location quality classification.}

    \item{lon, lat}{x and y geographic coordinates of each location.}
  }

}


\seealso{
  \code{\link{readLocs}}, \code{\link{distSpeed}}.
}


\source{WhaleNet Satellite Tracking Program
  \url{http://whale.wheelock.edu/Welcome.html}.}

% -------------------------------------------------------------------------

\examples{

zz <- system.file(file.path("data", "sealLocs.csv"),
                  package="diveMove", mustWork=TRUE)
str(read.csv(zz, sep=";"))

}


\keyword{datasets}
