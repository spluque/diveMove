\name{extractDive-methods}
\docType{methods}
\alias{extractDive-methods}

\alias{extractDive}
\alias{extractDive,TDR,numeric,numeric-method}
\alias{extractDive,TDRcalibrate,numeric,missing-method}

% -------------------------------------------------------------------------

\title{Extract Dives from "TDR" or "TDRcalibrate" Objects}


\description{ Extract data corresponding to a particular dive(s),
  referred to by number. }


\usage{
\S4method{extractDive}{TDR,numeric,numeric}(obj, diveNo, id)
\S4method{extractDive}{TDRcalibrate,numeric,missing}(obj, diveNo)
}


\arguments{

  \item{obj}{\code{\link{TDR}} object.}

  \item{diveNo}{numeric vector or scalar with dive numbers to extract.
    Duplicates are ignored.}

  \item{id}{numeric vector or scalar of dive numbers from where
    \code{diveNo} should be chosen.}

}


\value{
  An object of class \code{\link{TDR}} or \code{\link{TDRspeed}}.
}

% -------------------------------------------------------------------------

\examples{

\donttest{## Too long for checks
## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE)
dcalib		# the 'TDRcalibrate' that was created

diveX <- extractDive(divesTDR, 9, getDAct(dcalib, "dive.id"))
plotTDR(diveX, interact=FALSE)

diveX <- extractDive(dcalib, 5:10)
plotTDR(diveX, interact=FALSE)

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{methods}
