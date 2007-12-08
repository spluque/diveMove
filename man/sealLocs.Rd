% $Id: sealLocs.Rd,v 1.4 2007-12-08 21:50:54 sluque Exp $
\name{sealLocs}
\docType{data}

\alias{sealLocs}

% -------------------------------------------------------------------------

\title{Ringed and Gray Seal ARGOS Satellite Location Data}


\description{Satellite locations of a gray (Stephanie) and a ringed
(Ringy) seal caught and released in New York.}


\format{A data frame with the following information:

  \item{id}{String naming the seal the data come from.}

  \item{time}{The date and time of the location.}

  \item{class}{The ARGOS location quality classification.}

  \item{lon, lat}{x and y geographic coordinates of each location.}

}


\seealso{
  \code{\link{readLocs}}, \code{\link{distSpeed}}.
}


\source{WhaleNet Satellite Tracking Program
  \url{http://whale.wheelock.edu/Welcome.html}.}

% -------------------------------------------------------------------------

\keyword{datasets}
