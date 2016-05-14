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


\description{ Functions to model a mixture of 2 random Poisson processes
  to identify bouts of behaviour.  This follows Langton et al. (1995). }


\usage{
bouts2.mleFUN(x, p, lambda1, lambda2)
bouts2.ll(x)
bouts2.LL(x)
bouts.mle(ll.fun, start, x, \ldots)
bouts2.mleBEC(fit)
plotBouts2.mle(fit, x, xlab="x", ylab="Log Frequency", bec.lty=2, \ldots)
plotBouts2.cdf(fit, x, draw.bec=FALSE, bec.lty=2, \ldots)
}


\arguments{

  \item{x}{numeric vector with values to model.}

  \item{p, lambda1, lambda2}{numeric: parameters of the mixture of
    Poisson processes.}

  \item{ll.fun}{function returning the negative of the maximum
    likelihood function that should be maximized.  This should be a
    valid \code{minuslogl} argument to \code{\link[stats4]{mle}}.}

  \item{start, \ldots}{Arguments passed to \code{\link[stats4]{mle}}.
    For \code{plotBouts2.cdf}, arguments passed to
    \code{\link{plot.ecdf}}. For \code{plotBouts2.mle}, arguments passed
    to \code{\link{curve}} (must exclude \code{xaxs}, \code{yaxs}). For
    \code{plotBouts2.nls}, arguments passed to \code{\link{plot}} (must
    exclude \code{type}).}

  \item{fit}{\code{\link[stats4]{mle}} object.}

  \item{xlab, ylab}{character: titles for the x and y axes.}

  \item{bec.lty}{Line type specification for drawing the BEC reference
    line.}

  \item{draw.bec}{logical; do we draw the BEC?}

}


\value{

  \code{bouts.mle} returns an object of class \code{\link[stats4]{mle}}.

  \code{bouts2.mleBEC} and \code{bouts2.mleFUN} return a numeric vector.

  \code{bouts2.LL} and \code{bouts2.ll} return a function.

  \code{plotBouts2.mle} and \code{plotBouts2.cdf} return nothing, but
  produce a plot as side effect.

}


\details{ For now only a mixture of 2 Poisson processes is supported.
  Even in this relatively simple case, it is very important to provide
  good starting values for the parameters.

  One useful strategy to get good starting parameter values is to
  proceed in 4 steps.  First, fit a broken stick model to the log
  frequencies of binned data (see \code{\link{boutinit}}), to obtain
  estimates of 4 parameters corresponding to a 2-process model (Sibly et
  al. 1990).  Second, calculate parameter \var{p} from the 2 alpha
  parameters obtained from the broken stick model, to get 3 tentative
  initial values for the 2-process model from Langton et al. (1995).
  Third, obtain MLE estimates for these 3 parameters, but using a
  reparameterized version of the -log L2 function.  Lastly, obtain the
  final MLE estimates for the 3 parameters by using the estimates from
  step 3, un-transformed back to their original scales, maximizing the
  original parameterization of the -log L2 function.

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

  \code{bouts.mle} is the function performing the main job of maximizing
  the -log L2 functions, and is essentially a wrapper around
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
  method \dQuote{L-BFGS-B}, again bounding the parameter search, as well
  as passing a \code{control} list with proper \code{parscale} for
  controlling the optimization.  See \code{Note} below for useful
  constraints which can be tried.

}


\note{

  In the case of a mixture of 2 Poisson processes, useful values for
  lower bounds for the \code{bouts.LL} reparameterization are
  \code{c(-2, -5, -10)}.  For \code{bouts2.ll}, useful lower bounds are
  \code{rep(1e-08, 3)}.  A useful parscale argument for the latter is
  \code{c(1, 0.1, 0.01)}.  However, I have only tested this for cases of
  diving behaviour in pinnipeds, so these suggested values may not be
  useful in other cases.

  The lambdas can be very small for some data, particularly
  \code{lambda2}, so the default \code{ndeps} in \code{\link{optim}} can
  be so large as to push the search outside the bounds given.  To avoid
  this problem, provide a smaller \code{ndeps} value.

}


\references{

  Langton, S.; Collett, D. and Sibly, R. (1995) Splitting behaviour into
  bouts; a maximum likelihood approach.  Behaviour \bold{132}, 9-10.

  Luque, S.P. and Guinet, C. (2007) A maximum likelihood approach for
  identifying dive bouts improves accuracy, precision, and objectivity.
  Behaviour, \bold{144}, 1315-1332.

  Sibly, R.; Nott, H. and Fletcher, D. (1990) Splitting behaviour into
  bouts.  Animal Behaviour \bold{39}, 63-69.

}


\seealso{ \code{\link[stats4]{mle}}, \code{\link{optim}},
  \code{\link{logit}}, \code{\link{unLogit}} for transforming and
  fitting a reparameterized model. }

% -------------------------------------------------------------------------

\examples{

\donttest{## Too long for checks
## Using the Example from '?diveStats':
utils::example("diveStats", package="diveMove",
               ask=FALSE, echo=FALSE)
postdives <- tdrX.tab$postdive.dur[tdrX.tab$phase.no == 2]
postdives.diff <- abs(diff(postdives))

## Remove isolated dives
postdives.diff <- postdives.diff[postdives.diff < 2000]
lnfreq <- boutfreqs(postdives.diff, bw=0.1, plot=FALSE)
startval <- boutinit(lnfreq, 50)
p <- startval[[1]]["a"] / (startval[[1]]["a"] + startval[[2]]["a"])

## Fit the reparameterized (transformed parameters) model
## Drop names by wrapping around as.vector()
init.parms <- list(p=as.vector(logit(p)),
                   lambda1=as.vector(log(startval[[1]]["lambda"])),
                   lambda2=as.vector(log(startval[[2]]["lambda"])))
bout.fit1 <- bouts.mle(bouts2.LL, start=init.parms, x=postdives.diff,
                       method="L-BFGS-B", lower=c(-2, -5, -10))
coefs <- as.vector(coef(bout.fit1))

## Un-transform and fit the original parameterization
init.parms <- list(p=unLogit(coefs[1]), lambda1=exp(coefs[2]),
                   lambda2=exp(coefs[3]))
bout.fit2 <- bouts.mle(bouts2.ll, x=postdives.diff, start=init.parms,
                       method="L-BFGS-B", lower=rep(1e-08, 3),
                       control=list(parscale=c(1, 0.1, 0.01)))
plotBouts(bout.fit2, postdives.diff)

## Plot cumulative frequency distribution
plotBouts2.cdf(bout.fit2, postdives.diff)

## Estimated BEC
bec <- bec2(bout.fit2)

## Label bouts
labelBouts(postdives, rep(bec, length(postdives)),
           bec.method="seq.diff")

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{models}
\keyword{manip}
