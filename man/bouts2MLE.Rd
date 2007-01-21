\name{bouts2MLE}

\alias{bouts2.mleFUN}
\alias{bouts2.ll}
\alias{bouts2.LL}
\alias{bouts.mle}
\alias{bouts2.mleBEC}
\alias{plotBouts2.mle}
\alias{plotBouts2.cdf}

% -------------------------------------------------------------------------

\title{Maximum Likelihood Model of mixture of 2 Poisson Processes}


\description{

  Functions to model a mixture of 2 random Poisson processes to identify
  bouts of behaviour.  This follows Langton et al. (1995).

}

\usage{
bouts2.mleFUN(x, p, lambda1, lambda2)
bouts2.ll(x)
bouts2.LL(x)
bouts.mle(ll.fun, start, x, \ldots)
bouts2.mleBEC(fit)
plotBouts2.mle(fit, x, xlab="x", y.log.lab="Log Frequency",
               y.dens.lab="Density", bec.lty=2)
plotBouts2.cdf(fit, x, draw.bec=FALSE, bec.lty=2, \ldots)
}

\arguments{

  \item{x}{Numeric vector with values to model.}

  \item{p, lambda1, lambda2}{Parameters of the mixture of Poisson
    processes.}

  \item{ll.fun}{function returning the negative of the maximum
    likelihood function that should be maximized.  This should be a
    valid \code{minuslogl} argument to \code{\link[stats4]{mle}}.}

  \item{start, \ldots}{Arguments passed to \code{\link[stats4]{mle}}.
    For \code{plotBouts2.cdf}, arguments passed to
    \code{\link{plot.ecdf}}.}

  \item{fit}{\code{\link[stats4]{mle}} object.}

  \item{xlab, y.log.lab, y.dens.lab}{Titles for the x, upper y, and
    lower y axes.}

  \item{bec.lty}{Line type specification for drawing the BEC reference
    line.}

  \item{draw.bec}{Logical; do we draw the BEC?}

}

\value{
  An object of class \code{\link[stats4]{mle}}.
}

\details{ For now only a mixture of 2 Poisson processes is supported.
  Even in this relatively simple case, it is very important to provide
  good starting values for the parameters.

  One useful strategy for this is to proceed in 4 steps.  First, fit a
  broken stick model to the log frequencies of binned data (see
  \code{\link{boutinit}}), to obtain estimates of 4 parameters
  corresponding to a 2-process model (Sibly et al. 1990).  Second,
  calculate parameter \var{p} from the 2 alpha parameters obtained from
  the broken stick model, to get 3 tentative initial values for the
  2-process model from Langton et al. (1995).  Third, obtain MLE
  estimates for these 3 parameters, but using a reparameterized version
  of the -log L2 function.  Lastly, obtain the final MLE estimates for
  the 3 parameters by using the estimates from step 3, un-transformed
  back to their original scales, maximizing the original
  parameterization of the -log L2 function.

  \code{\link{boutinit}} can be used to perform step 1.  Calculation of
  the mixing parameter \var{p} in step 2 is trivial from these
  estimates.  Function \code{\link{bouts2.LL}} is a reparameterized
  version of the -log L2 function given by Langton et al. (1995), so can
  be used for step 3.  This uses a logit (see \code{\link{logit}})
  transformation of the mixing parameter \var{p}, and log
  transformations for both density parameters \var{lambda1} and
  \var{lambda2}.  Function \code{\link{bouts2.ll}} is the -log L2
  function corresponding to the un-transformed model, hence can be used
  for step 4.

  bouts.mle is the function performing the main job of maximizing the
  -log L2 functions, and is essentially a wrapper around
  \code{\link[stats4]{mle}}.  It only takes the -log L2 function, a list
  of starting values, and the variable to be modelled, all of which are
  passed to \code{\link[stats4]{mle}} for optimization.  Additionally,
  any other arguments are also passed to \code{\link[stats4]{mle}},
  hence great control is provided for fitting any of the -log L2
  functions.

  In practice, step 3 does not pose major problems using the
  reparameterized -log L2 function, but it might be useful to use method
  \dQuote{L-BFGS-B} with appropriate lower and upper bounds.  Step 4 can
  be a bit more problematic, because the parameters are usually on very
  different scales.  Therefore, it is almost always the rule to use
  method \dQuote{L-BFGS-B}, again with bounding the parameter searc, as
  well as passing a \code{control} list with proper \code{parscale} for
  controlling the optimization.  See \code{Note} below for useful
  constraints which can be tried.

}

\note{

  In the case of a mixture of 2 Poisson processes, useful values for
  lower bounds for the \code{bouts.LL} reparameterization are
  \code{c(-2, -5, -10)}.  For \code{bouts2.ll}, useful lower bounds are
  \code{rep(1e-08, 3)}.  A useful parscale argument for the latter is
  \code{c(1, 0.1, 0.01)}.

}

\references{

  Langton, S.; Collett, D. & Sibly, R.  Splitting behaviour into bouts;
  a maximum likelihood approach Behaviour, 1995, 132, 9-10.

  Sibly, R.; Nott, H. & Fletcher, D.  Splitting behaviour into bouts
  Animal Behaviour, 1990, 39, 63-69.

}


\seealso{ \code{\link[stats4]{mle}}, \code{\link{optim}},
  \code{\link{logit}}, \code{\link{revLogit}} for transforming and
  fitting a reparameterized model. }

% -------------------------------------------------------------------------

\examples{

data(divesSummary)
postdives <- divesSummary$postdive.dur[divesSummary$trip.no == 2]
postdives.diff <- abs(diff(postdives))

## Remove isolated dives
postdives.diff <- postdives.diff[postdives.diff < 2000]
lnfreq <- boutfreqs(postdives.diff, bw=0.1, plot=FALSE)
def.par <- par(no.readonly=TRUE) # for resetting later
startval <- boutinit(lnfreq, 50)
p <- startval$a1 / (startval$a1 + startval$a2)

## Fit the reparameterized (transformed parameters) model
init.parms <- list(p=logit(p), lambda1=log(startval$lambda1),
                   lambda2=log(startval$lambda2))
bout.fit1 <- bouts.mle(bouts2.LL, start=init.parms, x=postdives.diff,
                       method="L-BFGS-B", lower=c(-2, -5, -10))
coefs <- as.vector(coef(bout.fit1))

## Un-transform and fit the original parameterization
init.parms <- list(p=revLogit(coefs[1]), lambda1=exp(coefs[2]),
                   lambda2=exp(coefs[3]))
bout.fit2 <- bouts.mle(bouts2.ll, x=postdives.diff, start=init.parms,
                       method="L-BFGS-B", lower=rep(1e-08, 3),
                       control=list(parscale=c(1, 0.1, 0.01)))
plotBouts(bout.fit2, postdives.diff)
par(def.par)

## Plot cumulative frequency distribution
plotBouts2.cdf(bout.fit2, postdives.diff)
par(def.par)

## Estimated BEC
bec2(bout.fit2)

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{models}
\keyword{manip}
