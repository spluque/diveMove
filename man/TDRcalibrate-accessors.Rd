\name{TDRcalibrate-accessors}
\encoding{latin1}
\docType{methods}
\alias{TDRcalibrate-methods}
\alias{TDRcalibrate-accessors}

\alias{getDAct}
\alias{getDPhaseLab}
\alias{getGAct}
\alias{getTDR}
\alias{getSpeedCoef}

\alias{getDAct,TDRcalibrate,missing-method}
\alias{getDAct,TDRcalibrate,character-method}
\alias{getDPhaseLab,TDRcalibrate,missing-method}
\alias{getDPhaseLab,TDRcalibrate,numeric-method}
\alias{getGAct,TDRcalibrate,missing-method}
\alias{getGAct,TDRcalibrate,character-method}
\alias{show,TDRcalibrate-method}
\alias{getTDR,TDRcalibrate-method}
\alias{getSpeedCoef,TDRcalibrate-method}

% -------------------------------------------------------------------------

\title{Methods to Show and Extract Basic Information from "TDRcalibrate"
  Objects}


\description{
  Plot, print summaries and extract information from "TDRcalibrate"
  objects.
}

\usage{
\S4method{getDAct}{TDRcalibrate,missing}(x)
\S4method{getDAct}{TDRcalibrate,character}(x, y)
\S4method{getDPhaseLab}{TDRcalibrate,missing}(x)
\S4method{getDPhaseLab}{TDRcalibrate,numeric}(x, diveNo)
\S4method{getGAct}{TDRcalibrate,missing}(x)
\S4method{getGAct}{TDRcalibrate,character}(x, y)
}


\arguments{

  \item{x}{\code{"\linkS4class{TDRcalibrate}"} object.}

  \item{diveNo}{numeric vector with dive numbers to plot.}

  \item{y}{string; \dQuote{dive.id}, \dQuote{dive.activity}, or
    \dQuote{postdive.id} in the case of \code{getDAct}, to extract the
    numeric dive ID, the factor identifying dive phases in each dive, or
    the numeric postdive ID, respectively.  In the case of
    \code{getGAct} it should be one of \dQuote{phase.id},
    \dQuote{activity}, \dQuote{begin}, or \dQuote{end}, to extract the
    numeric phase ID for each observation, a factor indicating what
    major activity the observation corresponds to, or the beginning and
    end times of each phase in the record, respectively.}  }


\section{Show Methods}{
  \describe{

    \item{show}{\code{signature(object="TDRcalibrate")}: prints an
      informative summary of the data.}

  }

}


\section{Extractor Methods}{
  \describe{

    \item{getDAct}{\code{signature(x="TDRcalibrate", y="missing")}: this
      accesses the \code{dive.activity} slot of
      \code{\link{TDRcalibrate}} objects.  Thus, it extracts a data
      frame with vectors identifying all readings to a particular dive
      and postdive number, and a factor identifying all readings to a
      particular activity.}

    \item{getDAct}{\code{signature(x="TDRcalibrate", y = "character")}:
	as the method for missing \code{y}, but selects a particular
	vector to extract.  See \code{\link{TDRcalibrate}} for possible
	strings.}

    \item{getDPhaseLab}{\code{signature(x="TDRcalibrate", diveNo =
	"missing")}: extracts a factor identifying all readings to a
	particular dive phase.  This accesses the dive.phases slot of
	\code{\link{TDRcalibrate}} objects, which is a factor.}

    \item{getDPhaseLab}{\code{signature(x="TDRcalibrate", diveNo =
	"numeric")}: as the method for missing \code{y}, but selects
	data from a particular dive number to extract.}

    \item{getGAct}{\code{signature(x="TDRcalibrate", y="missing")}: this
      accesses the \code{gross.activity} slot of
      \code{\link{TDRcalibrate}} objects, which is a named list.  It
      extracts elements that divide the data into major wet and dry
      activities.}

    \item{getGAct}{\code{signature(x="TDRcalibrate", y="character")}: as
	the method for missing \code{y}, but extracts particular
	elements.}

    \item{getTDR}{\code{signature(x="TDRcalibrate")}: this accesses the
      tdr slot of \code{\link{TDRcalibrate}} objects, which is a
      \code{\link{TDR}} object.}

    \item{getSpeedCoef}{\code{signature(x="TDRcalibrate")}: this
      accesses the \code{speed.calib.coefs} slot of
      \code{\link{TDRcalibrate}} objects; the speed calibration
      coefficients.}

  }

}


\value{ The extractor methods return an object of the same class as
  elements of the slot they extracted. }

% -------------------------------------------------------------------------

\examples{

data(divesTDRcalibrate)

divesTDRcalibrate		# show

## Beginning times of each successive phase in record
getGAct(divesTDRcalibrate, "begin")

## Factor of dive IDs
dids <- getDAct(divesTDRcalibrate, "dive.id")
table(dids[dids > 0])		# samples per dive

## Factor of dive phases for given dive
getDPhaseLab(divesTDRcalibrate, 19)

}

\author{\enc{Sebastián}{Sebastian} P. Luque \email{spluque@gmail.com}}

\keyword{methods}
