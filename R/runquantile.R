## Copied and modified from caTools

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
