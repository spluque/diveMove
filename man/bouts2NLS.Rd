\name{bouts2NLS}

\alias{bouts2.nlsFUN}
\alias{bouts2.nls}
\alias{bouts2.nlsBEC}
\alias{plotBouts2.nls}

% -------------------------------------------------------------------------

\title{Fit mixture of 2 Poisson Processes to Log Frequency data}


\description{
  Functions to model a mixture of 2 random Poisson processes to
  histogram-like data of log frequency vs interval mid points.  This
  follows Sibly et al. (1990) method.
}

\usage{
bouts2.nlsFUN(x, a1, lambda1, a2, lambda2)
bouts2.nls(lnfreq, start, maxiter)
bouts2.nlsBEC(fit)
plotBouts2.nls(fit, lnfreq, bec.lty, \ldots)
}

\arguments{

  \item{x}{Numeric vector with values to model.}

  \item{a1, lambda1, a2, lambda2}{Parameters from the mixture of Poisson
    processes.}

  \item{lnfreq}{data frame with named components \var{lnfreq} (log
    frequencies) and corresponding \var{x} (mid points of histogram
    bins).}

  \item{start, maxiter}{Arguments passed to \code{\link{nls}}.}

  \item{fit}{nls object.}

  \item{bec.lty}{Line type specification for drawing the BEC reference
    line.}

  \item{\ldots}{Arguments passed to \code{\link{plot.default}}.}

}

\value{

  \code{bouts2.nlsFUN} returns a numeric vector evaluating the mixture of 2
  Poisson process.

  \code{bouts2.nls} returns an nls object resulting from fitting this
  model to data.

  \code{bouts2.nlsBEC} returns a number corresponding to the bout ending
  criterion derived from the model.

  \code{plotBouts2.nls} plots the fitted model with the corresponding
  data.

}

\details{

}

\references{

  Sibly, R.; Nott, H. and Fletcher, D. (1990) Splitting behaviour into
  bouts Animal Behaviour \bold{39}, 63-69.

}

\seealso{
  \code{\link{bouts.mle}} for a better approach.
}

% -------------------------------------------------------------------------

\examples{

data(divesSummary)
## Postdive durations
postdives <- divesSummary$postdive.dur[divesSummary$trip.no == 2]
postdives.diff <- abs(diff(postdives))
## Remove isolated dives
postdives.diff <- postdives.diff[postdives.diff < 2000]

## Construct histogram
lnfreq <- boutfreqs(postdives.diff, bw=0.1, plot=FALSE)
startval <- boutinit(lnfreq, 50)

## Fit the 2 process model
bout.fit1 <- bouts2.nls(lnfreq, start=startval, maxiter=500)
summary(bout.fit1)
plotBouts(bout.fit1)

## Estimated BEC
bec2(bout.fit1)

}


\author{Sebastián P. Luque \email{spluque@gmail.com}}

\keyword{models}
\keyword{manip}
