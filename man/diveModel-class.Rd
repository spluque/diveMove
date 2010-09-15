% $Id: diveModel-class.Rd 200 2008-11-04 03:06:40Z sluque $
\name{diveModel-class}
\Rdversion{1.1}
\docType{class}

\alias{diveModel-class}
\alias{diveModel}

% -------------------------------------------------------------------------

\title{Class "diveModel" for representing a model for identifying dive
  phases}


\description{ Details of model used to identify the different phases of
  a dive. }


\section{Objects from the Class}{ Objects can be created by calls of the
  form \code{new("diveModel", ...)}.

  \sQuote{diveModel} objects contain all relevant details of the process
  to identify phases of a dive.  Objects of this class are typically
  generated during depth calibration, using
  \code{\link{calibrateDepth}}, more specifically
  \code{\link{.cutDive}}.
  
}


\section{Slots}{

  \describe{
    
    \item{\code{label.matrix}:}{Object of class \code{"matrix"}.  A
      2-column character matrix with row numbers matching each
      observation to the full \code{\link{TDR}} object, and a vector
      labelling the phases of each dive.}
    
    \item{\code{dive.spline}:}{Object of class \code{"smooth.spline"}.
      Details of cubic smoothing spline fit (see
      \code{\link[stats]{smooth.spline}}).}

    \item{\code{spline.deriv}:}{Object of class \code{"list"}.  A list
      with the first derivative of the smoothing spline (see
      \code{\link[stats]{predict.smooth.spline}}).}

    \item{\code{descent.crit}:}{Object of class \code{"numeric"}.  The
      index of the observation at which the descent was deemed to have
      ended (from initial surface observation).}

    \item{\code{ascent.crit}:}{Object of class \code{"numeric"}.  the
      index of the observation at which the ascent was deemed to have
      ended (from initial surface observation).}

    \item{\code{descent.crit.rate}:}{Object of class \code{"numeric"}.
      The rate of descent corresponding to the critical quantile
      used.}

    \item{\code{ascent.crit.rate}:}{Object of class \code{"numeric"}.
      The rate of ascent corresponding to the critical quantile
      used.}
   
  }
  
}


\seealso{\code{\link{getDiveDeriv}}, \code{\link{plotDiveModel}}}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}

\examples{

showClass("diveModel")

}


\keyword{classes}
