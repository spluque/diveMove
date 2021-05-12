
##' Defunct functions in package \sQuote{diveMove}
##'
##' These functions are defunct and no longer available.
##'
##' @name diveMove-defunct
##' @keywords internal
NULL

##' @rdname diveMove-defunct
##' @section \code{bouts2.ll} and \code{bouts2.LL}:
##' These functions have been superseded by the new function generator
##' \code{\link{boutsMLEll.chooser}}
"bouts2.ll" <- function() {
    .Defunct("boutsMLEll.chooser", package="diveMove",
             msg=paste("'bouts2.ll' is now defunct.",
                       "Please see help(boutsMLEll.chooser)"))
}

##' @rdname diveMove-defunct
"bouts2.LL" <- function() {
    .Defunct("boutsMLEll.chooser", package="diveMove",
             msg=paste("'bouts2.LL' is now defunct.",
                       "Please see help(boutsMLEll.chooser)"))
}

##' @rdname diveMove-defunct
"bouts.mle" <- function() {
    .Defunct("fitMLEbouts", package="diveMove",
             msg=paste("'bouts.mle' is now defunct.",
                       "Please see help(fitMLEbouts)"))
}
