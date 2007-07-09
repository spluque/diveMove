\name{TDR-accessors}
\docType{methods}
\alias{TDR-methods}
\alias{TDR-accessors}

\alias{as.TDRspeed}
\alias{ccData<-}
\alias{depth<-}
\alias{speed<-}
\alias{getCCData}
\alias{getDepth}
\alias{getDtime}
\alias{getFileName}
\alias{getTime}
\alias{getSpeed}

\alias{[,TDR-method}
\alias{as.data.frame,TDR-method}
\alias{coerce,TDR,data.frame-method}
\alias{as.TDRspeed,TDR-method}
\alias{coerce,TDR,TDRspeed-method}
\alias{getCCData,TDR,missing-method}
\alias{getCCData,TDR,character-method}
\alias{ccData<-,TDR,data.frame-method}
\alias{getDepth,TDR-method}
\alias{getDtime,TDR-method}
\alias{depth<-,TDR,numeric-method}
\alias{getFileName,TDR-method}
\alias{show,TDR-method}
\alias{getTime,TDR-method}
\alias{getSpeed,TDRspeed-method}
\alias{speed<-,TDRspeed,numeric-method}

% -------------------------------------------------------------------------

\title{Coerce, Extractor, and Replacement methods for class "TDR"
  objects}


\description{
  Basic methods for manipulating objects of class "TDR".
}


\section{Show Methods}{
  \describe{

    \item{show}{\code{signature(object="TDR")}: print an informative
      summary of the data.}

  }
}

\section{Coerce Methods}{
  \describe{

    \item{as.data.frame}{\code{signature(x="TDR")}: Coerce object to
      data.frame.  This method returns a data frame, with attributes
      \sQuote{file} and \sQuote{dtime} indicating the source file and
      the interval between samples.}

    \item{as.data.frame}{\code{signature(x="TDRspeed")}: Coerce object
      to data.frame.  Returns an object as for \code{\link{TDR}}
      objects.}

    \item{as.TDRspeed}{\code{signature(x="TDR")}: Coerce object to
      TDRspeed class.}

  }
}

\section{Extractor Methods}{
  \describe{

    \item{[}{\code{signature(x="TDR")}: Subset a TDR object; these
      objects can be subsetted on a single index \var{i}.  Selects
      given rows from object.}

    \item{getDepth}{\code{signature(x = "TDR")}: depth slot accessor.}

    \item{getCCData}{\code{signature(x="TDR", y="missing")}:
      concurrentData slot accessor.}

    \item{getCCData}{\code{signature(x="TDR", y="character")}: access
      component named \var{y} in \var{x}.}

    \item{getDtime}{\code{signature(x = "TDR")}: sampling interval
      accessor.}

    \item{getFileName}{\code{signature(x="TDR")}: source file name
      accessor.}

    \item{getTime}{\code{signature(x = "TDR")}: time slot accessor.}

    \item{getSpeed}{\code{signature(x = "TDRspeed")}: speed accessor for
      \code{TDRspeed} objects.}

  }
}

\section{Replacement Methods}{
  \describe{

    \item{depth<-}{\code{signature(x="TDR")}: depth replacement.}

    \item{speed<-}{\code{signature(x="TDR")}: speed replacement.}

    \item{ccData<-}{\code{signature(x="TDR")}: concurrent data frame
      replacement.}

  }
}


\seealso{ \code{\link{extractDive}}, \code{\link{plotTD}}.  }

% -------------------------------------------------------------------------

\examples{
data(divesTDR)

## Retrieve the name of the source file
getFileName(divesTDR)
## Retrieve concurrent temperature measurements
temp <- getCCData(divesTDR, "temperature")

## Coerce to a data frame
dives.df <- as.data.frame(divesTDR)
head(dives.df)

## Replace speed measurements
newspeed <- getSpeed(divesTDR) + 2
speed(divesTDR) <- newspeed
}

\author{Sebastián P. Luque \email{spluque@gmail.com}}

\keyword{methods}
