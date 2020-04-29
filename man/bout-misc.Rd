\name{bout-misc}

\alias{boutfreqs}
\alias{boutinit}
\alias{labelBouts}
\alias{logit}
\alias{unLogit}

% -------------------------------------------------------------------------

\title{Fit a Broken Stick Model on Log Frequency Data for identification
  of bouts of behaviour}


\description{ Application of methods described by Sibly et al. (1990)
  and Mori et al. (2001) for the identification of bouts of behaviour. }


\usage{
boutfreqs(x, bw, method=c("standard", "seq.diff"), plot=TRUE, \ldots)
boutinit(lnfreq, x.break, plot=TRUE, \ldots)
labelBouts(x, bec, bec.method=c("standard", "seq.diff"))
logit(p)
unLogit(logit)
}


\arguments{

  \item{x}{numeric vector on which bouts will be identified based on
    \dQuote{method}. For \code{labelBouts} it can also be a matrix with
    different variables for which bouts should be identified.}

  \item{bw}{numeric scalar: bin width for the histogram.}

  \item{method, bec.method}{character: method used for calculating the
    frequencies: \dQuote{standard} simply uses x, while
    \dQuote{seq.diff} uses the sequential differences method.}

  \item{plot}{logical, whether to plot results or not.}

  \item{\ldots}{For \code{boutfreqs}, arguments passed to hist (must
    exclude \code{breaks} and \code{include.lowest}); for
    \code{boutinit}, arguments passed to \code{\link{plot}} (must
    exclude \code{type}).}

  \item{lnfreq}{\code{\link{data.frame}} with components \var{lnfreq}
    (log frequencies) and corresponding \var{x} (mid points of histogram
    bins).}

  \item{x.break}{vector of length 1 or 2 with \code{x} value(s) defining
    the break(s) point(s) for broken stick model, such that \code{x} <
    \code{x.break}[1] is 1st process, and \code{x} >= \code{x.break}[1]
    & \code{x} < \code{x.break}[2] is 2nd one, and \code{x} >=
    \code{x.break}[2] is 3rd one.}

  \item{bec}{numeric vector or matrix with values for the bout ending
    criterion which should be compared against the values in x for
    identifying the bouts.}

  \item{p}{numeric vector of proportions (0-1) to transform to the logit
    scale.}

  \item{logit}{numeric scalar: logit value to transform back to original
    scale.}

}


\value{

  \code{boutfreqs} returns a data frame with components \var{lnfreq}
  containing the log frequencies and \var{x}, containing the
  corresponding mid points of the histogram.  Empty bins are excluded.
  A plot (histogram of \emph{input data}) is produced as a side effect
  if argument plot is \code{TRUE}.  See the Details section.

  \code{boutinit} returns a list with as many elements as the number of
  processes implied by \code{x.break} (i.e. \code{length(x.break) + 1}).
  Each element is a vector of length two, corresponding to \code{a} and
  \code{lambda}, which are starting values derived from broken stick
  model.  A plot is produced as a side effect if argument \code{plot} is
  \code{TRUE}.

  \code{labelBouts} returns a numeric vector sequentially labelling each
  row or element of \var{x}, which associates it with a particular bout.

  \code{unLogit} and \code{logit} return a numeric vector with the
  (un)transformed arguments.

}


\details{This follows the procedure described in Mori et al. (2001),
  which is based on Sibly et al. 1990.  Currently, only a two process
  model is supported.

  \code{boutfreqs} creates a histogram with the log transformed
  frequencies of \var{x} with a chosen bin width and upper limit.  Bins
  following empty ones have their frequencies averaged over the number
  of previous empty bins plus one.

  \code{boutinit} fits a "broken stick" model to the log frequencies
  modelled as a function of \var{x} (well, the midpoints of the binned
  data), using chosen value(s) to separate the two or three processes.

  \code{labelBouts} labels each element (or row, if a matrix) of \var{x}
  with a sequential number, identifying which bout the reading belongs
  to.  The \code{bec} argument needs to have the same dimensions as
  \code{x} to allow for situations where \code{bec} within \code{x}.

  \code{logit} and \code{unLogit} are useful for reparameterizing the
  negative maximum likelihood function, if using Langton et al. (1995).

}


\references{

  Langton, S.; Collett, D. and Sibly, R. (1995) Splitting behaviour into
  bouts; a maximum likelihood approach.  Behaviour \bold{132}, 9-10.

  Luque, S.P. and Guinet, C. (2007) A maximum likelihood approach for
  identifying dive bouts improves accuracy, precision, and objectivity.
  Behaviour, \bold{144}, 1315-1332.

  Mori, Y.; Yoda, K. and Sato, K. (2001) Defining dive bouts using a
  sequential differences analysis.  Behaviour, 2001 \bold{138},
  1451-1466.

  Sibly, R.; Nott, H. and Fletcher, D. (1990) Splitting behaviour into
  bouts.  Animal Behaviour \bold{39}, 63-69.

}


\seealso{\code{\link{bouts2.nls}}, \code{\link{bouts.mle}}.  These
  include an example for \code{labelBouts}.}

% -------------------------------------------------------------------------

\examples{

\donttest{## Too long for checks
## Using the Example from '?diveStats':
utils::example("diveStats", package="diveMove",
               ask=FALSE, echo=FALSE, run.donttest=TRUE)
postdives <- tdrX.tab$postdive.dur[tdrX.tab$phase.no == 2]
## Remove isolated dives
postdives <- postdives[postdives < 2000]
lnfreq <- boutfreqs(postdives, bw=0.1, method="seq.diff", plot=FALSE)
boutinit(lnfreq, 50)

## See ?bouts.mle for labelBouts() example

}

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{misc}
\keyword{manip}
