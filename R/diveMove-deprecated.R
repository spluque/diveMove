
##' Deprecated functions in diveMove
##'
##' These functions are provided for compatibility with older versions of
##' \sQuote{diveMove} only, and will be removed (defunct) in the next
##' release.
##'
##' @name diveMove-deprecated
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @keywords internal
NULL

##' @rdname diveMove-deprecated
##' @section \code{bouts2.nlsFUN}:
##' For \code{bouts2.nlsFUN}, use \code{\link{boutsNLSll}}.
"bouts2.nlsFUN" <- function(x, a1, lambda1, a2, lambda2) {
    .Deprecated("boutsNLSll", package="diveMove",
                msg=paste("'bouts2.nlsFUN' is now deprecated in favor of",
                          "the new generalized 'boutsNLSll'.",
                          "Please see help(boutsNLSll)"))
    boutsNLSll(x, coefs=c(a1, lambda1, a2, lambda2))
}

##' @rdname diveMove-deprecated
##' @section \code{bouts2.nls}:
##' For \code{bouts2.nls}, use \code{\link{fitNLSbouts}}.
"bouts2.nls" <- function(lnfreq, start, maxiter) {
   .Deprecated("fitNLSbouts", package="diveMove",
                msg=paste("'bouts2.nls' is now deprecated in favor of",
                          "the new method 'fitNLSbouts'.",
                          "Please see help(fitNLSbouts)"))
   fitNLSbouts(lnfreq, start=start, maxiter=maxiter)
}

##' @rdname diveMove-deprecated
##' @section \code{bec2}:
##' For \code{bec2}, use \code{\link{bec}}.
"bec2" <- function(fit) {
   .Deprecated("bec", package="diveMove",
                msg=paste("'bec2' is now deprecated in favor of",
                          "the new generalized method 'bec'.",
                          "Please see help(bec)"))
   bec(fit)
}

##' @rdname diveMove-deprecated
##' @section \code{bec3}:
##' For \code{bec3}, use \code{\link{bec}}.
"bec3" <- function(fit) {
   .Deprecated("bec", package="diveMove",
                msg=paste("'bec3' is now deprecated in favor of",
                          "the new generalized method 'bec'.",
                          "Please see help(bec)"))
   bec(fit)
}

##' @rdname diveMove-deprecated
##' @section \code{bouts3.nlsFUN}:
##' For \code{bouts3.nlsFUN}, use \code{\link{boutsNLSll}}.
"bouts3.nlsFUN" <- function(x, a1, lambda1, a2, lambda2, a3, lambda3) {
    .Deprecated("boutsNLSll", package="diveMove",
                msg=paste("'bouts3.nlsFUN' is now deprecated in favor of",
                          "the new generalized 'boutsNLSll'.",
                          "Please see help(boutsNLSll)"))
    boutsNLSll(x, coefs=c(a1, lambda1, a2, lambda2, a3, lambda3))
}

##' @rdname diveMove-deprecated
##' @section \code{bouts3.nls}:
##' For \code{bouts3.nls}, use \code{\link{fitNLSbouts}}.
"bouts3.nls" <- function(lnfreq, start, maxiter) {
   .Deprecated("fitNLSbouts", package="diveMove",
                msg=paste("'bouts3.nls' is now deprecated in favor of",
                          "the new method 'fitNLSbouts'.",
                          "Please see help(fitNLSbouts)"))
   fitNLSbouts(lnfreq, start=start, maxiter=maxiter)
}

##' @rdname diveMove-deprecated
##' @section \code{bouts2.mleFUN}:
##' For \code{bouts2.mleFUN}, use \code{\link{.bouts2MLEll}}.
"bouts2.mleFUN" <- function(x, p, lambda1, lambda2) {
    msg <- paste("'bouts3.nlsFUN' is now deprecated in favor",
                 "the new generalized '.bouts2MLEll', which should not be",
                 "used directly, but rather via 'fitMLEbouts'.",
                 "Please see help(fitMLEbouts))")
    .Deprecated(".bouts2MLEll", package="diveMove", msg=msg)
    .bouts2MLEll(x, p=p, lambda0=lambda1, lambda1=lambda2)
}
