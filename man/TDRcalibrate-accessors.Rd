\name{TDRcalibrate-accessors}
\docType{methods}
\alias{TDRcalibrate-methods}
\alias{TDRcalibrate-accessors}

\alias{getDAct}
\alias{getDPhaseLab}
\alias{getDiveModel}
\alias{getDiveDeriv}
\alias{getGAct}
\alias{getSpeedCoef}
\alias{getTDR}

\alias{getDAct,TDRcalibrate,missing-method}
\alias{getDAct,TDRcalibrate,character-method}
\alias{getDPhaseLab,TDRcalibrate,missing-method}
\alias{getDPhaseLab,TDRcalibrate,numeric-method}
\alias{getDiveModel,TDRcalibrate,missing-method}
\alias{getDiveModel,TDRcalibrate,numeric-method}
\alias{getDiveDeriv,diveModel-method}
\alias{getDiveDeriv,TDRcalibrate-method}
\alias{getGAct,TDRcalibrate,missing-method}
\alias{getGAct,TDRcalibrate,character-method}
\alias{show,TDRcalibrate-method}
\alias{show,diveModel-method}
\alias{getSpeedCoef,TDRcalibrate-method}
\alias{getTDR,TDRcalibrate-method}

% -------------------------------------------------------------------------

\title{Methods to Show and Extract Basic Information from "TDRcalibrate"
  Objects}


\description{ Show and extract information from
  \code{\link{TDRcalibrate}} objects. }

\usage{
\S4method{getDAct}{TDRcalibrate,missing}(x)
\S4method{getDAct}{TDRcalibrate,character}(x, y)
\S4method{getDPhaseLab}{TDRcalibrate,missing}(x)
\S4method{getDPhaseLab}{TDRcalibrate,numeric}(x, diveNo)
\S4method{getDiveModel}{TDRcalibrate,missing}(x)
\S4method{getDiveModel}{TDRcalibrate,numeric}(x, diveNo)
\S4method{getDiveDeriv}{diveModel}(x, phase=c("all", "descent", "bottom", "ascent"))
\S4method{getDiveDeriv}{TDRcalibrate}(x, diveNo, phase=c("all", "descent", "bottom", "ascent"))
\S4method{getGAct}{TDRcalibrate,missing}(x)
\S4method{getGAct}{TDRcalibrate,character}(x, y)
\S4method{getSpeedCoef}{TDRcalibrate}(x)
\S4method{getTDR}{TDRcalibrate}(x)
}


\arguments{

  \item{x}{\code{\link{TDRcalibrate}} object.}

  \item{diveNo}{numeric vector with dive numbers to extract information
    from.}

  \item{y}{string; \dQuote{dive.id}, \dQuote{dive.activity}, or
    \dQuote{postdive.id} in the case of \code{getDAct}, to extract the
    numeric dive ID, the factor identifying activity phases (with
    underwater and diving levels possibly represented), or the numeric
    postdive ID, respectively.  In the case of \code{getGAct} it should
    be one of \dQuote{phase.id}, \dQuote{activity}, \dQuote{begin}, or
    \dQuote{end}, to extract the numeric phase ID for each observation,
    a factor indicating what major activity the observation corresponds
    to (where diving and underwater levels are not represented), or the
    beginning and end times of each phase in the record, respectively.}

  \item{phase}{character vector indicating phase of the dive for which
    to extract the derivative.}

}


\section{Show Methods}{
  \describe{

    \item{show}{\code{signature(object="TDRcalibrate")}: prints an
      informative summary of the data.}

    \item{show}{\code{signature(object="diveModel")}: prints an
      informative summary of a dive model.}

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

    \item{getDAct}{\code{signature(x="TDRcalibrate", y="character")}: as
	the method for missing \code{y}, but selects a particular vector
	to extract.  See \code{\link{TDRcalibrate}} for possible
	strings.}

    \item{getDPhaseLab}{\code{signature(x="TDRcalibrate",
	diveNo="missing")}: extracts a factor identifying all readings
	to a particular dive phase.  This accesses the
	\code{dive.phases} slot of \code{\link{TDRcalibrate}} objects,
	which is a factor.}

    \item{getDPhaseLab}{\code{signature(x="TDRcalibrate",
	diveNo="numeric")}: as the method for missing \code{y}, but
	selects data from a particular dive number to extract.}

    \item{getDiveModel}{\code{signature(x="TDRcalibrate",
	diveNo="missing")}: extracts a list with all dive phase models.
	This accesses the dive.models slot of \code{\link{TDRcalibrate}}
	objects.}

    \item{getDiveModel}{\code{signature(x="TDRcalibrate",
	diveNo="numeric")}: as the method for missing \code{diveNo}, but
	selects data from a particular dive number to extract.}

    \item{getDiveDeriv}{\code{signature(x="TDRcalibrate")}: extracts the
	derivative (list) of the dive model (smoothing spline) from the
	dive.models slot of \code{\link{TDRcalibrate}} objects for one
	or all phases of a dive.}

    \item{getDiveDeriv}{\code{signature(x="diveModel")}: as the method
	for \code{TDRcalibrate}, but selects data from one or all phases
	of a dive.}

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


\seealso{\code{\link{diveModel}}, \code{\link{plotDiveModel}},
\code{\link{plotTDR}}.}

% -------------------------------------------------------------------------

\examples{

\donttest{## Too long for checks
## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE, , run.donttest=TRUE)
dcalib		# the 'TDRcalibrate' that was created

## Beginning times of each successive phase in record
getGAct(dcalib, "begin")

## Factor of dive IDs
dids <- getDAct(dcalib, "dive.id")
table(dids[dids > 0])		# samples per dive

## Factor of dive phases for given dive
getDPhaseLab(dcalib, 19)
## Full dive model
(dm <- getDiveModel(dcalib, 19))
str(dm)

## Derivatives
getDiveDeriv(dcalib, diveNo=19)
(derivs.desc <- getDiveDeriv(dcalib, diveNo=19, phase="descent"))
(derivs.bott <- getDiveDeriv(dcalib, diveNo=19, phase="bottom"))
(derivs.asc <- getDiveDeriv(dcalib, diveNo=19, phase="ascent"))
if (require(lattice)) {
    fl <- c("descent", "bottom", "ascent")
    bwplot(~ derivs.desc$y + derivs.bott$y + derivs.asc$y,
           outer=TRUE, allow.multiple=TRUE, layout=c(1, 3),
           xlab=expression(paste("Vertical rate (", m \%.\% s^-1, ")")),
           strip=strip.custom(factor.levels=fl))
}

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{methods}
