##' Histogram of log-transformed frequencies
##' @param x numeric vector on which bouts will be identified based on
##'     \dQuote{method}. For \code{labelBouts} it can also be a matrix with
##'     different variables for which bouts should be identified.
##' @param bw numeric scalar: bin width for the histogram.
##' @param method character: method used for calculating the frequencies:
##'     \dQuote{standard} simply uses x, while \dQuote{seq.diff} uses the
##'     sequential differences method.
##' @param plot logical, whether to plot results or not.
##' @param ... For \code{boutfreqs}, arguments passed to hist (must exclude
##'     \code{breaks} and \code{include.lowest})
##' @return
##' \code{boutfreqs} returns an object of class \code{Bouts}, with slot
##' \code{lnfreq} consisting of a data frame with components \var{lnfreq}
##' containing the log frequencies and \var{x}, containing the
##' corresponding mid points of the histogram.  Empty bins are excluded. A
##' plot (histogram of \emph{input data}) is produced as a side effect if
##' argument plot is \code{TRUE}.  See the Details section.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
"boutfreqs" <- function(x, bw, method=c("standard", "seq.diff"),
                        plot=TRUE, ...)
{
    method <- match.arg(method)
    switch(method,
           standard = {upper <- max(x, na.rm=TRUE)
                       brks <- seq(min(x, na.rm=TRUE), upper, bw)
                       if (brks[length(brks)] < upper) {
                           brks <- c(brks, brks[length(brks)] + bw)
                       }
                       h <- hist(x, breaks=brks, include.lowest=TRUE,
                                 plot=plot, ...)},
           seq.diff = {diff.x <- abs(diff(x))
                       upper <- max(diff.x, na.rm=TRUE)
                       brks <- seq(0, upper, bw)
                       if (brks[length(brks)] < upper) {
                           brks <- c(brks, brks[length(brks)] + bw)
                       }
                       h <- hist(diff.x, breaks=brks, include.lowest=TRUE,
                                 plot=plot, ...)})
    ok <- which(h$counts > 0)
    freq.adj <- h$counts[ok] / diff(c(0, ok))
    new("Bouts", x=x, method=method,
        lnfreq=data.frame(lnfreq=log(freq.adj), x=h$mids[ok]))
}

##' Utilities for Poisson mixture analyses
##'
##' \code{calc.p} computes \code{p} (proportion) parameter from \code{a}
##' and \code{lambda} coefficients in a broken stick model.
##' @param coefs numeric matrix [2,N] of coefficients (\code{a} and
##'     \code{lambda}) in rows for each process of the model in columns.
##'     Columns are assumed to be in decreasing order with respect to
##'     \code{lambda}
##' @return numeric vector with proportion parameters implied by
##'     \code{coefs}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @rdname bouts-internal
##' @keywords internal
"calc.p" <- function(coefs)
{
    p <- rep(NA_real_, ncol(coefs) - 1)
    for (coln in seq_len(ncol(coefs) - 1)) {
        procn1 <- coefs[, coln]
        procn2 <- coefs[, coln + 1]
        a1 <- procn1[1]
        a2 <- procn2[1]
        p[coln] <- a1 / (a1 + a2)
    }
    p
}

##' Build a list of \code{p} and \code{lambda} parameters from
##'     one-dimensional vector of coefficients
##'
##' \code{build.p.lambda} parses the \code{x} vector, usually returned by
##' the \code{coef} method, where \eqn{x =
##' (p_0,\dots,p_n,\lambda_1,\dots,\lambda_{n+1})}{x=(p_0,...,p_n,lambda_0,...,lambda_n+1)},
##' and build a named list with \code{p} and \code{lambda} elements to use
##' in fitting functions.
##' @param x numeric vector of coefficients
##' @return named (\code{p}, \code{lambda}) list with parsed coefficients.
##' @rdname bouts-internal
"build.p.lambda" <- function(x) {
    npars <- length(x)
    switch(as.character(npars),
           "3" = {
               p <- x[1]
               lambdas <- x[2:length(x)]
           },
           "5" = {
               p <- x[1:2]
               lambdas <- x[3:length(x)]
           },
           stop("Only mixtures of <= 3 process are implemented"))

    list(p=p, lambdas=lambdas)
}

##' Generate samples from a mixture of exponential distributions
##'
##' \code{rmixexp} uses a special definition for the probabilities
##' \code{p_i} to generate random samples from a mixed Poisson distribution
##' with known parameters for each process.  In the two-process case,
##' \code{p} represents the proportion of "fast" to "slow" events in the
##' mixture.  In the three-process case, \code{p_0} represents the
##' proportion of "fast" to "slow" events, and \code{p_1} represents the
##' proportion of "slow" to "slow" *and* "very slow" events.
##' @param n integer output sample size.
##' @param p numeric probabilities for processes generating the output
##'     mixture sample.
##' @param lambdas numeric \code{lambda} (rate) for each process.
##' @return vector of samples.
##' @examples
##' ## Draw samples from a mixture where the first process occurs with
##' ## p < 0.7, and the second process occurs with the remaining
##' ## probability.
##' p <- 0.7
##' lda <- c(0.05, 0.005)
##' (rndprocs2 <- rmixexp(1000, p, lda))
##'
##' ## 3-process
##' p_f <- 0.6    # fast to slow
##' p_svs <- 0.7  # prop of slow to (slow + very slow) procs
##' p_true <- c(p_f, p_svs)
##' lda_true <- c(0.05, 0.01, 8e-4)
##' (rndprocs3 <- rmixexp(1000, p_true, lda_true))
"rmixexp" <- function(n, p, lambdas) {
    if (length(lambdas) != (length(p) + 1)) {
        stop("lambdas must have one more element than p")
    }
    switch(as.character(length(p)),
           "1" = { p.full <- c(p, 1 - p) },
           "2" = {
               p0 <- p[1]
               p1 <- p[2] * (1 - p0)
               p2 <- 1 - (p0 + p1)
               p.full <- c(p0, p1, p2)
           },
           stop("Mixtures of more than 3 processes are not yet implemented"))

    chooser <- sample(length(lambdas), n, replace=TRUE,
                      prob=p.full / sum(p.full))
    rates <- lambdas
    rexp(n, rate=rates[chooser])
}

##' Logit transformation
##'
##' \code{logit} and \code{unLogit} are helpful for reparameterizing the
##' negative maximum likelihood function, if using Langton et al. (1995).
##' @param p numeric vector of proportions (0-1) to transform to the logit
##'     scale.
##' @return \code{unLogit} and \code{logit} return a numeric vector with
##'     the (un)transformed arguments.
##' @rdname bouts-internal
"logit" <- function(p) log(p / (1 - p))

##' Untransform logit
##' @param logit numeric scalar: logit value to transform back to original
##'     scale.
##' @rdname bouts-internal
"unLogit" <- function(logit) exp(logit) / (exp(logit) + 1)

##' Estimated cumulative frequency for two- or three-process Poisson
##' mixture models
##' @param x numeric vector described by model.
##' @param p numeric scalar or vector of proportion parameters.
##' @param lambdas numeric vector of rate parameters.
##' @return numeric vector with cumulative frequency.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @examples
##' utils::example("rmixexp", package="diveMove", ask=FALSE)
##' ## boutsCDF(rndprocs3, p=p_true, lambdas=lda_true)
"boutsCDF" <- function(x, p, lambdas) {
    nprocs <- length(lambdas)

    ## We assume at least two processes
    p0 <- p[1]
    lda0 <- lambdas[1]
    term0 <- 1 - p0 * exp(-lda0 * x)

    switch(as.character(nprocs),
           "2" = {
               lda1 = lambdas[2]
               term1 = (1 - p0) * exp(-lda1 * x)
               cdf = term0 - term1
           },
           "3" = {
               p1 = p[2]
               lda1 = lambdas[2]
               term1 = p1 * (1 - p0) * exp(-lda1 * x)
               lda2 = lambdas[3]
               term2 = (1 - p0) * (1 - p1) * exp(-lda2 * x)
               cdf = term0 - term1 - term2
           },
           stop("Only mixtures of <= 3 process are implemented"))

    cdf
}
