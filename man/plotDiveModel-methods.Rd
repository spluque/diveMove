% $Id: plotDiveModel-methods.Rd 200 2008-11-04 03:06:40Z sluque $
\name{plotDiveModel-methods}
\docType{methods}
\alias{plotDiveModel-methods}

\alias{plotDiveModel}
\alias{plotDiveModel,diveModel,missing-method}
\alias{plotDiveModel,numeric,numeric-method}
\alias{plotDiveModel,TDRcalibrate,numeric-method}

% -------------------------------------------------------------------------

\title{ Methods for plotting models of dive phases }


\description{ Methods for function \code{plotDiveModel}. }


% \usage{
% \S4method{plotDiveModel}{diveModel,missing}(x)
% \S4method{plotDiveModel}{numeric,numeric}(x, y)
% \S4method{plotDiveModel}{TDRcalibrate,numeric}(x, y)
% }


% \arguments{

%   \item{x}{A \code{\link{diveModel}}, \code{\link{numeric}} vector of
%     depth observations, or \code{\link{TDRcalibrate}} object.}

%   \item{y}{Numeric object with variable modelled.}

%   \item{\ldots}{Arguments passed to the underlying
%     \code{\link{plotBouts2.nls}} and \code{\link{plotBouts2.mle}}.}

% }


\section{Methods}{

  \describe{

    \item{\code{signature(x = "diveModel", y = "missing")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(x = "numeric", y = "numeric")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(x = "TDRcalibrate", y = "numeric")}}{
      %%  ~~describe this method here~~
    }
    
}}


\seealso{ \code{\link{diveModel}} }

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{methods}
