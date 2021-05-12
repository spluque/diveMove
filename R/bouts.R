##' Log likelihood function of parameters given observed data
##'
##' This function defines a closure, where \code{x} will be the object
##' passed to it.
##' @param x numeric vector of independent data to be described by the
##'     function.
##' @param x0 numerical one-dimensional vector of coefficients.
##' @param transformed logical indicating whether coefficients need to be
##'     transformed back to original scale to compute the negative log
##'     likelihood.
##' @return \code{ll.chooser} returns the negative log likelihood function
##'     of the joint distribution.
##' @rdname boutsMLEll
##' @keywords internal
"boutsMLEll.chooser" <- function(x, x0, transformed=TRUE)
{
    pars.l <- build.p.lambda(x0)

    switch(as.character(length(pars.l[["lambdas"]])),
           "2" = {
               function(p, lambda0, lambda1) {
                   if (transformed) {
                       p <- unLogit(p)
                       lambda0 <- exp(lambda0)
                       lambda1 <- exp(lambda1)
                   }
                   -sum(.bouts2MLEll(x, p, lambda0=lambda0,
                                     lambda1=lambda1))
               } },
           "3" = {
               function(p0, p1, lambda0, lambda1, lambda2) {
                   if (transformed) {
                       p0 <- unLogit(p0)
                       p1 <- unLogit(p1)
                       lambda0 <- exp(lambda0)
                       lambda1 <- exp(lambda1)
                       lambda2 <- exp(lambda2)
                   }
                   -sum(.bouts3MLEll(x, p0=p0, p1=p1, lambda0=lambda0,
                                     lambda1=lambda1, lambda2=lambda2))
               } },
           stop("Not implemented"))
}

##' @describeIn boutsMLEll Log likelihood function in a 2-process Poisson
##'     mixture
##' @param p,lambda0,lambda1 numeric: parameters of the model.
##' @return numeric vector
".bouts2MLEll" <- function(x, p, lambda0, lambda1)
{
    term0 <- p * lambda0 * exp(-lambda0 * x)
    term1 <- (1 - p) * lambda1 * exp(-lambda1 * x)
    res <- term0 + term1
    log(res)
}

##' @describeIn boutsMLEll Log likelihood function in a 3-process Poisson
##'     mixture
##' @param p0,p1,lambda2 numeric: parameters of the model.
##' @return numeric vector
".bouts3MLEll" <- function(x, p0, p1, lambda0, lambda1, lambda2)
{
    term0 <- p0 * lambda0 * exp(-lambda0 * x)
    term1 <- p1 * (1 - p0) * lambda1 * exp(-lambda1 * x)
    term2 = (1 - p1) * (1 - p0) * lambda2 * exp(-lambda2 * x)
    res = term0 + term1 + term2
    ## if (any(res <= 0)) message("negatives at:", p0, p1,
    ##                            lambda0, lambda1, lambda2)
    log(res)
}


## TEST ZONE --------------------------------------------------------------
