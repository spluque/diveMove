% $Id: bouts3NLS.Rd 464 2011-03-28 07:22:06Z sluque $
\name{bouts3NLS}

\alias{bouts3.nlsFUN}
\alias{bouts3.nls}
\alias{bouts3.nlsBEC}
\alias{plotBouts3.nls}

% -------------------------------------------------------------------------

\title{Fit mixture of 3 Poisson Processes to Log Frequency data}


\description{ Functions to model a mixture of 3 random Poisson processes
  to histogram-like data of log frequency vs interval mid points.  This
  follows Sibly et al. (1990) method, adapted for a three-process model
  by Berdoy (1993).
  }


\usage{
bouts3.nlsFUN(x, a1, lambda1, a2, lambda2, a3, lambda3)
bouts3.nls(lnfreq, start, maxiter)
bouts3.nlsBEC(fit)
plotBouts3.nls(fit, lnfreq, bec.lty, \ldots)
}


\arguments{

  \item{x}{numeric vector with values to model.}

  \item{a1, lambda1, a2, lambda2, a3, lambda3}{numeric: parameters from
    the mixture of Poisson processes.}

  \item{lnfreq}{\code{\link{data.frame}} with named components
    \var{lnfreq} (log frequencies) and corresponding \var{x} (mid points
    of histogram bins).}

  \item{start, maxiter}{Arguments passed to \code{\link{nls}}.}

  \item{fit}{nls object.}

  \item{bec.lty}{Line type specification for drawing the BEC reference
    line.}

  \item{\ldots}{Arguments passed to \code{\link{plot.default}}.}

}


\value{

  \code{bouts3.nlsFUN} returns a numeric vector evaluating the mixture of 3
  Poisson process.

  \code{bouts3.nls} returns an nls object resulting from fitting this
  model to data.

  \code{bouts3.nlsBEC} returns a number corresponding to the bout ending
  criterion derived from the model.

  \code{plotBouts3.nls} plots the fitted model with the corresponding
  data.

}


\details{
  \code{bouts3.nlsFUN} is the function object defining the nonlinear
  least-squares relationship in the model.  It is not meant to be used
  directly, but is used internally by \code{bouts3.nls}.

  \code{bouts3.nls} fits the nonlinear least-squares model itself.

  \code{bouts3.nlsBEC} calculates the BEC from a list object, as the one
  that is returned by \code{\link{nls}}, representing a fit of the
  model.  \code{plotBouts3.nls} plots such an object.
}


\references{

  Sibly, R.; Nott, H. and Fletcher, D. (1990) Splitting behaviour into
  bouts. Animal Behaviour \bold{39}, 63-69.

  Berdoy, M. (1993) Defining bouts of behaviour: a three-process model.
  Animal Behaviour \bold{46}, 387-396.

}


\seealso{
  \code{\link{bouts.mle}} for a better approach;
  \code{\link{boutfreqs}}; \code{\link{boutinit}}
}

% -------------------------------------------------------------------------

\examples{

## Using the Example from '?diveStats':
utils::example("diveStats", package="diveMove",
               ask=FALSE, echo=FALSE)
## Postdive durations
postdives <- tdrX.tab$postdive.dur
postdives.diff <- abs(diff(postdives))
## Remove isolated dives
postdives.diff <- postdives.diff[postdives.diff < 4000]

## Construct histogram
lnfreq <- boutfreqs(postdives.diff, bw=0.1, plot=FALSE)

startval <- boutinit(lnfreq, c(50, 400))
## Drop names by wrapping around as.vector()
startval.l <- list(a1=as.vector(startval[[1]]["a"]),
                   lambda1=as.vector(startval[[1]]["lambda"]),
                   a2=as.vector(startval[[2]]["a"]),
                   lambda2=as.vector(startval[[2]]["lambda"]),
                   a3=as.vector(startval[[3]]["a"]),
                   lambda3=as.vector(startval[[3]]["lambda"]))

## Fit the 3 process model
bout.fit <- bouts3.nls(lnfreq, start=startval.l, maxiter=500)
summary(bout.fit)
plotBouts(bout.fit)

## Estimated BEC
bec3(bout.fit)

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{models}
\keyword{manip}
