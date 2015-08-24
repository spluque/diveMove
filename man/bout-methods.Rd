\name{bout-methods}
\docType{methods}
\alias{bout-methods}

\alias{bec2}
\alias{bec3}
\alias{plotBouts}

\alias{plotBouts,nls-method}
\alias{plotBouts,mle-method}
\alias{bec2,nls-method}
\alias{bec2,mle-method}
\alias{bec3,nls-method}

% -------------------------------------------------------------------------

\title{Methods for Plotting and Extracting the Bout Ending Criterion}


\description{ Plot results from fitted mixture of 2-process Poisson
  models, and calculate the bout ending criterion.  }


\usage{
\S4method{plotBouts}{nls}(fit, \ldots)
\S4method{plotBouts}{mle}(fit, x, \ldots)
\S4method{bec2}{nls}(fit)
\S4method{bec2}{mle}(fit)
\S4method{bec3}{nls}(fit)
}


\arguments{

  \item{fit}{\code{\link{nls}} or \code{\link[stats4]{mle}} object.}

  \item{x}{numeric object with variable modelled.}

  \item{\ldots}{Arguments passed to the underlying
    \code{\link{plotBouts2.nls}} and \code{\link{plotBouts2.mle}}.}

}


\section{General Methods}{
  \describe{

    \item{plotBouts}{\code{signature(fit="nls")}: Plot fitted 2- or
      3-process model of log frequency vs the interval mid points,
      including observed data.}

    \item{plotBouts}{\code{signature(x="mle")}: As the \code{nls}
      method, but models fitted through maximum likelihood method.  This
      plots the fitted model and a density plot of observed data.}

    \item{bec2}{\code{signature(fit="nls")}: Extract the estimated bout
      ending criterion from a fitted 2-process model.}

    \item{bec2}{\code{signature(fit="mle")}: As the nls method, but
      extracts the value from a maximum likelihood model.}

    \item{bec3}{\code{signature(fit="nls")}: Extract the estimated bout
      ending criterion from a fitted 3-process model.}

  }
}


\seealso{ \code{\link{bouts.mle}}, \code{\link{bouts2.nls}},
  \code{\link{bouts3.nls}} for examples. }


\references{

  Berdoy, M. (1993) Defining bouts of behaviour: a three-process model.
  Animal Behaviour \bold{46}, 387-396.

  Langton, S.; Collett, D. and Sibly, R. (1995) Splitting behaviour into
  bouts; a maximum likelihood approach.  Behaviour \bold{132}, 9-10.

  Luque, S. P. and Guinet, C. (2007) A maximum likelihood approach for
  identifying dive bouts improves accuracy, precision, and objectivity.
  Behaviour \bold{144}, 1315-1332.

  Mori, Y.; Yoda, K. and Sato, K. (2001) Defining dive bouts using a
  sequential differences analysis.  Behaviour \bold{138}, 1451-1466.

  Sibly, R.; Nott, H. and Fletcher, D. (1990) Splitting behaviour into
  bouts.  Animal Behaviour \bold{39}, 63-69.

}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{methods}
