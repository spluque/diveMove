## Copied and modified from caTools

##' Quantile of Moving Window
##'
##' Moving (aka running, rolling) Window Quantile calculated over a vector
##'
##' Apart from the end values, the result of y = runquantile(x, k) is the
##' same as \dQuote{\code{for(j=(1+k2):(n-k2))
##' y[j]=quintile(x[(j-k2):(j+k2)],na.rm = TRUE)}}. It can handle
##' non-finite numbers like NaN's and Inf's (like \code{\link{quantile}(x,
##' na.rm = TRUE)}).
##'
##' The main incentive to write this set of functions was relative slowness
##' of majority of moving window functions available in R and its packages.
##' All functions listed in "see also" section are slower than very
##' inefficient \dQuote{\code{\link{apply}(\link{embed}(x,k),1,FUN)}}
##' approach. Relative speeds of \code{runquantile} is O(n*k)
##'
##' Function \code{runquantile} uses insertion sort to sort the moving
##' window, but gain speed by remembering results of the previous
##' sort. Since each time the window is moved, only one point changes, all
##' but one points in the window are already sorted. Insertion sort can fix
##' that in O(k) time.
##'
##' @aliases .runquantile
##' @param x numeric vector of length n or matrix with n rows. If \code{x}
##'     is a matrix than each column will be processed separately.
##' @param k width of moving window; must be an integer between one and n.
##' @param endrule
##' character string indicating how the values at the beginning and the
##' end, of the array, should be treated. Only first and last \code{k2}
##' values at both ends are affected, where \code{k2} is the half-bandwidth
##' \code{k2 = k \%/\% 2}.
##'
##' * \code{"quantile"} Applies the \code{\link{quantile}} function to
##'   smaller and smaller sections of the array. Equivalent to: \code{for(i
##'   in 1:k2) out[i]=quantile(x[1:(i+k2)])}.
##' * \code{"trim"} Trim the ends; output array length is equal to
##'   \code{length(x)-2*k2 (out = out[(k2+1):(n-k2)])}. This option mimics
##'   output of \code{\link{apply}} \code{(\link{embed}(x,k),1,FUN)} and
##'   other related functions.
##' * \code{"keep"} Fill the ends with numbers from \code{x} vector
##'   \code{(out[1:k2] = x[1:k2])}
##' * \code{"constant"} Fill the ends with first and last calculated value
##'   in output array \code{(out[1:k2] = out[k2+1])}
##' * \code{"NA"} Fill the ends with NA's \code{(out[1:k2] = NA)}
##' * \code{"func"} Same as \code{"quantile"} but implimented in R. This
##'   option could be very slow, and is included mostly for testing
##'
##' @param probs numeric vector of probabilities with values in [0,1] range
##'     used by \code{runquantile}.
##' @param type an integer between 1 and 9 selecting one of the nine
##'     quantile algorithms, same as \code{type} in \code{\link{quantile}}
##'     function.  Another even more readable description of nine ways to
##'     calculate quantiles can be found at
##'     \url{http://mathworld.wolfram.com/Quantile.html}.
##' @param align specifies whether result should be centered (default),
##'     left-aligned or right-aligned.  If \code{endrule}="quantile" then
##'     setting \code{align} to "left" or "right" will fall back on slower
##'     implementation equivalent to \code{endrule}="func".
##' @return
##' If \code{x} is a matrix than function \code{runquantile} returns a
##' matrix of size [n \eqn{\times}{x} \code{\link{length}}(probs)]. If
##' \code{x} is vactor a than function \code{runquantile} returns a matrix
##' of size [\code{\link{dim}}(x) \eqn{\times}{x}
##' \code{\link{length}}(probs)].  If \code{endrule="trim"} the output will
##' have fewer rows.
##' @author Jarek Tuszynski (SAIC) \email{jaroslaw.w.tuszynski@@saic.com}
##' @references
##' About quantiles: Hyndman, R. J. and Fan, Y. (1996) \emph{Sample
##' quantiles in statistical packages, American Statistician}, 50, 361.
##'
##' About quantiles: Eric W. Weisstein. \emph{Quantile}. From MathWorld-- A
##' Wolfram Web Resource. \url{http://mathworld.wolfram.com/Quantile.html}
##'
##' About insertion sort used in \code{runmad} and \code{runquantile}: R.
##' Sedgewick (1988): \emph{Algorithms}. Addison-Wesley (page 99)
##'
##' @keywords ts smooth array utilities
##' @concept moving min
##' @concept rolling min
##' @concept running min
##' @concept moving max
##' @concept rolling max
##' @concept running max
##' @concept moving minimum
##' @concept rolling minimum
##' @concept running minimum
##' @concept moving maximum
##' @concept rolling maximum
##' @concept running maximum
##' @concept moving quantile
##' @concept rolling quantile
##' @concept running quantile
##' @concept moving percentile
##' @concept rolling percentile
##' @concept running percentile
##' @concept moving window
##' @concept rolling window
##' @concept running window
##' @examples
##' ## show plot using runquantile
##' k <- 31; n <- 200
##' x <- rnorm(n, sd=30) + abs(seq(n)-n/4)
##' y <- diveMove:::.runquantile(x, k, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
##' col <- c("black", "red", "green", "blue", "magenta", "cyan")
##' plot(x, col=col[1], main="Moving Window Quantiles")
##' lines(y[,1], col=col[2])
##' lines(y[,2], col=col[3])
##' lines(y[,3], col=col[4])
##' lines(y[,4], col=col[5])
##' lines(y[,5], col=col[6])
##' lab=c("data", "runquantile(.05)", "runquantile(.25)", "runquantile(0.5)",
##'       "runquantile(.75)", "runquantile(.95)")
##' legend(0,230, lab, col=col, lty=1)
##'
##' ## basic tests against apply/embed
##' a <- diveMove:::.runquantile(x, k, c(0.3, 0.7), endrule="trim")
##' b <- t(apply(embed(x, k), 1, quantile, probs=c(0.3, 0.7)))
##' eps <- .Machine$double.eps ^ 0.5
##' stopifnot(all(abs(a - b) < eps))
##'
##' ## Test against loop approach
##'
##' ## This test works fine at the R prompt but fails during package check -
##' ## need to investigate
##' k <- 25; n <- 200
##' x <- rnorm(n, sd=30) + abs(seq(n) - n / 4) # create random data
##' x[seq(1, n, 11)] <- NaN;                   # add NANs
##' k2 <- k %/% 2
##' k1 <- k - k2 - 1
##' a <- diveMove:::.runquantile(x, k, probs=c(0.3, 0.8))
##' b <- matrix(0, n, 2)
##' for(j in 1:n) {
##'     lo <- max(1, j - k1)
##'     hi <- min(n, j + k2)
##'     b[j, ] <- quantile(x[lo:hi], probs=c(0.3, 0.8), na.rm=TRUE)
##' }
##' ## stopifnot(all(abs(a-b)<eps));
##'
##' ## Compare calculation of array ends
##' a <- diveMove:::.runquantile(x, k, probs=0.4,
##'                              endrule="quantile") # fast C code
##' b <- diveMove:::.runquantile(x, k, probs=0.4,
##'                              endrule="func")     # slow R code
##' stopifnot(all(abs(a - b) < eps))
##'
##' ## Test if moving windows forward and backward gives the same results
##' k <- 51
##' a <- diveMove:::.runquantile(x, k, probs=0.4)
##' b <- diveMove:::.runquantile(x[n:1], k, probs=0.4)
##' stopifnot(all(a[n:1]==b, na.rm=TRUE))
##'
##' ## Test vector vs. matrix inputs, especially for the edge handling
##' nRow <- 200; k <- 25; nCol <- 10
##' x <- rnorm(nRow, sd=30) + abs(seq(nRow) - n / 4)
##' x[seq(1, nRow, 10)] <- NaN              # add NANs
##' X <- matrix(rep(x, nCol), nRow, nCol)   # replicate x in columns of X
##' a <- diveMove:::.runquantile(x, k, probs=0.6)
##' b <- diveMove:::.runquantile(X, k, probs=0.6)
##' stopifnot(all(abs(a - b[, 1]) < eps))    # vector vs. 2D array
##' stopifnot(all(abs(b[, 1] - b[, nCol]) < eps)) # compare rows within 2D array
##'
##' ## Exhaustive testing of runquantile to standard R approach
##' numeric.test <- function (x, k) {
##'   probs <- c(1, 25, 50, 75, 99) / 100
##'   a <- diveMove:::.runquantile(x, k, c(0.3, 0.7), endrule="trim")
##'   b <- t(apply(embed(x, k), 1, quantile, probs=c(0.3, 0.7), na.rm=TRUE))
##'   eps <- .Machine$double.eps ^ 0.5
##'   stopifnot(all(abs(a - b) < eps))
##' }
##' n <- 50
##' x <- rnorm(n,sd=30) + abs(seq(n) - n / 4) # nice behaving data
##' for(i in 2:5) numeric.test(x, i)          # test small window sizes
##' for(i in 1:5) numeric.test(x, n - i + 1)  # test large window size
##' x[seq(1, 50, 10)] <- NaN                  # add NANs and repet the test
##' for(i in 2:5) numeric.test(x, i)          # test small window sizes
##' for(i in 1:5) numeric.test(x, n - i + 1)  # test large window size
##'
##' ## Speed comparison
##' \dontrun{
##' x <- runif(1e6); k=1e3 + 1
##' system.time(diveMove:::.runquantile(x, k, 0.5)) # Speed O(n*k)
##' }
.runquantile <- function(x, k, probs, type=7,
                         endrule=c("quantile", "NA", "trim", "keep",
                                   "constant", "func"),
                         align=c("center", "left", "right")) {
    ## see http://mathworld.wolfram.com/Quantile.html for very clear
    ## definition of different quantile types
    endrule <- match.arg(endrule)
    align <- match.arg(align)
    ## Capture dimension of input array - to be used for formating y
    dimx <- dim(x)
    yIsVec <- is.null(dimx)             # original x was a vector
    x <- as.vector(x)
    n <- length(x)
    np <- length(probs)
    k <- as.integer(k)
    type <- as.integer(type)
    if (k <= 1) return (rep(x, n, np))
    if (k > n) k <- n
    if (is.na(type) || (type < 1 | type > 9))
        warning("'type' outside allowed range [1,9]; changing 'type' to ",
                type <- 7)

    y <- double(n * np)
    y <- .C("run_quantile", as.double(x), y=y, as.integer(n),
            as.integer(k), as.double(probs), as.integer(np),
            as.integer(type), NAOK=TRUE)$y
    dim(y) <- c(n, np)

    for (i in 1:np) {                   # for each percentile
        yTmp <- .EndRule(x, y[, i], k, dimx, endrule, align, quantile,
                         probs=probs[i], type=type, na.rm=TRUE)
        if (i == 1) {
            if (is.null(dimx))
                dimy <- length(yTmp)
            else
                dimy <- dim(yTmp)
            yy <- matrix(0, length(yTmp), np) # initialize output array
        }
        yy[, i] <- as.vector(yTmp)
    }
    if (np > 1)
        dim(yy) <- c(dimy, np)
    else
        dim(yy) <- dimy
    return(yy)
}


.EndRule <- function(x, y, k, dimx,
                     endrule=c("NA", "trim", "keep", "constant", "func"),
                     align=c("center", "left", "right"), Func, ...) {
    ## Function which postprocess results of running windows functions and
    ## cast them in to specified format. On input y is equivalent to y =
    ## runFUNC(as.vector(x), k, endrule="func", align="center")

    ## === Step 1: inspects inputs and unify format ===
    align <- match.arg(align)
    k <- as.integer(k)
    k2 <- k %/% 2
    if (k2 < 1) k2 <- 1
    ## original x was a vector -> returned y will be a vector
    yIsVec <- is.null(dimx)
    if (yIsVec) dimx <- c(length(y), 1) # x & y will become 2D arrays
    dim(x) <- dimx
    dim(y) <- dimx
    n <- nrow(x)
    m <- ncol(x)
    if (k > n) k2 <- (n - 1) %/% 2
    k1 <- k - k2 - 1
    if (align == "center" && k == 2) align <- 'right'

    ## === Step 2: Apply different endrules ===
    if (endrule == "trim") {
        y <- y[(k1 + 1):(n - k2), ] # change y dimensions
    } else if (align == "center") {
        idx1 <- 1:k1
        idx2 <- (n - k2 + 1):n
        ## endrule calculation in R will be skipped for most common case
        ## when endrule is default and array was a vector not a matrix
        if (endrule == "NA") {
            y[idx1, ] <- NA
            y[idx2, ] <- NA
        } else if (endrule == "keep") {
            y[idx1, ] <- x[idx1, ]
            y[idx2, ] <- x[idx2, ]
        } else if (endrule == "constant") {
            y[idx1, ] <- y[k1 + 1 + integer(m), ]
            y[idx2, ] <- y[n - k2 + integer(m), ]
        } else if (endrule == "func" || !yIsVec) {
            for (j in 1:m) {
                for (i in idx1) y[i, j] <- Func(x[1:(i + k2), j], ...)
                for (i in idx2) y[i, j] <- Func(x[(i - k1):n, j], ...)
            }
        }
    } else if (align == "left") {
        y[1:(n - k1), ] <- y[(k1 + 1):n, ]
        idx <- (n - k + 2):n
        if (endrule == "NA") {
            y[idx, ] <- NA
        } else if (endrule == "keep") {
            y[idx, ] <- x[idx, ]
        } else if (endrule == "constant") {
            y[idx, ] <- y[n - k + integer(m) + 1, ]
        } else {
            for (j in 1:m) for (i in idx) y[i, j] <- Func(x[i:n, j], ...)
        }
    } else if (align == "right") {
        y[(k2 + 1):n, ] <- y[1:(n - k2), ]
        idx <- 1:(k - 1)
        if (endrule == "NA") {
            y[idx, ] <- NA
        } else if (endrule == "keep") {
            y[idx, ] <- x[idx, ]
        } else if (endrule == "constant") {
            y[idx, ] <- y[k + integer(m), ]
        } else {
            for (j in 1:m) for (i in idx) y[i, j] <- Func(x[1:i, j], ...)
        }
    }

    ## === Step 4: final casting and return results ===
    if (yIsVec) y <- as.vector(y)
    return(y)
}
