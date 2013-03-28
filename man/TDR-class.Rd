% $Id$
\name{TDR-class}
\docType{class}
\alias{TDR-class}

\alias{TDRspeed-class}
\alias{TDR}
\alias{TDRspeed}

% -------------------------------------------------------------------------

\title{Classes "TDR" and "TDRspeed" for representing TDR information}


\description{ These classes store information gathered by time-depth
  recorders. }


\section{Objects from the Class}{Objects can be created by calls of the
  form \code{new("TDR", \dots)} and \code{new("TDRspeed", \dots)}.

  \sQuote{TDR} objects contain concurrent time and depth readings, as
  well as a string indicating the file the data originates from, and a
  number indicating the sampling interval for these data.
  \sQuote{TDRspeed} extends \sQuote{TDR} objects containing additional
  concurrent speed readings.}


\section{Slots}{
  In class \emph{TDR}:
  \describe{

    \item{\code{file}:}{Object of class \sQuote{character}, string
      indicating the file where the data comes from.}

    \item{\code{dtime}:}{Object of class \sQuote{numeric}, sampling
      interval in seconds.}

    \item{\code{time}:}{Object of class \code{\link{POSIXct}}, time
      stamp for every reading.}

    \item{\code{depth}:}{Object of class \sQuote{numeric}, depth (m)
      readings.}

    \item{\code{concurrentData}:}{Object of class
      \code{\link{data.frame}}, optional data collected concurrently.}

  }

  Class \sQuote{TDRspeed} must also satisfy the condition that a
  component of the concurrentData slot is named speed or velocity,
  containing the measured speed, a vector of class \sQuote{numeric}
  containing speed measurements in (m/s) readings.

  }


\details{Since the data to store in objects of these clases usually come
  from a file, the easiest way to construct such objects is with the
  function \code{\link{readTDR}} to retrieve all the necessary
  information.  The methods listed above can thus be used to access all
  slots.}


\seealso{\code{\link{readTDR}}, \code{\link{TDRcalibrate}}.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{classes}
