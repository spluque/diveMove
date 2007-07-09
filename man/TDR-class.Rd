\name{TDR-class}
\encoding{latin1}
\docType{class}

\alias{TDR-class}
\alias{TDRspeed-class}
\alias{TDR}
\alias{TDRspeed}

% -------------------------------------------------------------------------

\title{Classes "TDR" and "TDRspeed" for representing TDR information}


\description{These classes store information gathered by time-depth
  recorders.}


\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("TDR", \dots)} and \code{new("TDRspeed", \dots)}.

  \code{TDR} objects contain concurrent time and depth readings, as well
  as a string indicating the file the data originates from, and a number
  indicating the sampling interval for these data.  \code{TDRspeed}
  extends \code{TDR} objects containing additional concurrent speed
  readings.}


\section{Slots}{
  In class \emph{TDR}:
  \describe{

    \item{\code{file}:}{Object of class \dQuote{character}, string
      indicating the file where the data comes from.}

    \item{\code{dtime}:}{Object of class \dQuote{numeric}, sampling
      interval in seconds.}

    \item{\code{time}:}{Object of class \dQuote{POSIXct}, time stamp for
      every reading.}

    \item{\code{depth}:}{Object of class \dQuote{numeric}, depth (m)
      readings.}

    \item{\code{concurrentData}:}{Object of class \dQuote{data.frame},
      optional data collected concurrently.}

  }

  Class \emph{TDRspeed} must also satisfy the condition that a component
  of the concurrentData slot is named speed or velocity, containing the
  measured speed, a vector of class \dQuote{numeric} containing speed
  measurements in (m/s) readings.

  }

}


\details{Since the data to store in objects of these clases usually come
  from a file, the easiest way to construct such objects is with the
  function \code{readTDR} to retrieve all the necessary information.
  The methods listed above can thus be used to access all slots.}

% -------------------------------------------------------------------------

\author{\enc{Sebastián}{Sebastian} P. Luque \email{spluque@gmail.com}}


\seealso{\code{\link{readTDR}}, \code{\link{TDRcalibrate}}.}


\keyword{classes}
