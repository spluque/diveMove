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

  \item{obj}{\code{"\linkS4class{TDR}"} object.}
  \item{diveNo}{Numeric vector or scalar with dive numbers to extract.}
  \item{id}{Numeric vector of dive numbers from where \code{diveNo}
    should be chosen.}

}


\value{
  An object of class \code{TDR} or \code{TDRspeed}.
}

% -------------------------------------------------------------------------

\examples{

data(divesTDR)
divesTDR
data(divesTDRcalibrate)
divesTDRcalibrate

diveX <- extractDive(divesTDR, 9, getDAct(divesTDRcalibrate, "dive.id"))
plotTDR(diveX, interact=FALSE)

diveX <- extractDive(divesTDRcalibrate, 5:10)
plotTDR(diveX, interact=FALSE)

}

\author{Sebastián P. Luque \email{spluque@gmail.com}}

\keyword{methods}
