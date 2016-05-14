\name{timeBudget-methods}
\docType{methods}

\alias{timeBudget}
\alias{timeBudget,TDRcalibrate,logical-method}

% -------------------------------------------------------------------------

\title{Describe the Time Budget of Major Activities from "TDRcalibrate"
  object.}


\description{
  Summarize the major activities recognized into a time budget.
}


\usage{
\S4method{timeBudget}{TDRcalibrate,logical}(obj, ignoreZ)

}


\arguments{
  \item{obj}{\code{\link{TDRcalibrate}} object.}

  \item{ignoreZ}{logical: whether to ignore trivial aquatic periods.}

}


\value{
  A \code{\link{data.frame}} with components:

  \item{phaseno}{A numeric vector numbering each period of activity.}

  \item{activity}{A factor labelling the period with the corresponding
    activity.}

  \item{beg, end}{\code{\link{POSIXct}} objects indicating the beginning
    and end of each period.}
}


\details{
  Ignored trivial aquatic periods are collapsed into the enclosing dry period.
}


\seealso{\code{\link{calibrateDepth}}}

% -------------------------------------------------------------------------

\examples{

\donttest{## Too long for checks
## Continuing the Example from '?calibrateDepth':
utils::example("calibrateDepth", package="diveMove",
               ask=FALSE, echo=FALSE)
dcalib		# the 'TDRcalibrate' that was created

timeBudget(dcalib, TRUE)

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{methods}
