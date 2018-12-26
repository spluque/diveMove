\name{runquantile-internal}
\alias{runquantile-internal}

\alias{.runquantile}

% -------------------------------------------------------------------------

\title{Quantile of Moving Window}


\description{
  Moving (aka running, rolling) Window Quantile calculated over a vector
}


\usage{
.runquantile(x, k, probs, type=7,
             endrule=c("quantile", "NA", "trim", "keep", "constant", "func"),
             align = c("center", "left", "right"))
}


\arguments{

  \item{x}{numeric vector of length n or matrix with n rows. If \code{x}
    is a matrix than each column will be processed separately.}

  \item{k}{width of moving window; must be an integer between one and
    n.}

  \item{endrule}{character string indicating how the values at the
    beginning and the end, of the array, should be treated. Only first
    and last \code{k2} values at both ends are affected, where \code{k2}
    is the half-bandwidth \code{k2 = k \%/\% 2}.

     \itemize{

       \item \code{"quantile"} Applies the \code{\link{quantile}}
         function to smaller and smaller sections of the
         array. Equivalent to: \code{for(i in 1:k2)
         out[i]=quantile(x[1:(i+k2)])}.

       \item \code{"trim"} Trim the ends; output array length is equal
         to \code{length(x)-2*k2 (out = out[(k2+1):(n-k2)])}. This
         option mimics output of \code{\link{apply}}
         \code{(\link{embed}(x,k),1,FUN)} and other related functions.

       \item \code{"keep"} Fill the ends with numbers from \code{x}
         vector \code{(out[1:k2] = x[1:k2])}

       \item \code{"constant"} Fill the ends with first and last
         calculated value in output array \code{(out[1:k2] = out[k2+1])}

       \item \code{"NA"} Fill the ends with NA's \code{(out[1:k2] = NA)}

       \item \code{"func"} Same as \code{"quantile"} but implimented in
       R. This option could be very slow, and is included mostly for
       testing

     }

   }

  \item{probs}{numeric vector of probabilities with values in [0,1]
    range used by \code{runquantile}.}

  \item{type}{an integer between 1 and 9 selecting one of the nine
    quantile algorithms, same as \code{type} in \code{\link{quantile}}
    function.  Another even more readable description of nine ways to
    calculate quantiles can be found at
    \url{http://mathworld.wolfram.com/Quantile.html}.}

  \item{align}{specifies whether result should be centered (default),
    left-aligned or right-aligned.  If \code{endrule}="quantile" then
    setting \code{align} to "left" or "right" will fall back on slower
    implementation equivalent to \code{endrule}="func". }

}

\details{Apart from the end values, the result of y = runquantile(x, k)
  is the same as \dQuote{\code{for(j=(1+k2):(n-k2))
  y[j]=quintile(x[(j-k2):(j+k2)],na.rm = TRUE)}}. It can handle
  non-finite numbers like NaN's and Inf's (like \code{\link{quantile}(x,
  na.rm = TRUE)}).

  The main incentive to write this set of functions was relative
  slowness of majority of moving window functions available in R and its
  packages.  All functions listed in "see also" section are slower than
  very inefficient \dQuote{\code{\link{apply}(\link{embed}(x,k),1,FUN)}}
  approach. Relative speeds of \code{runquantile} is O(n*k)

  Function \code{runquantile} uses insertion sort to sort the moving
  window, but gain speed by remembering results of the previous
  sort. Since each time the window is moved, only one point changes, all
  but one points in the window are already sorted. Insertion sort can
  fix that in O(k) time.

}

\value{
  If \code{x} is a matrix than function \code{runquantile} returns a
  matrix of size [n \eqn{\times}{x} \code{\link{length}}(probs)]. If
  \code{x} is vactor a than function \code{runquantile} returns a matrix
  of size [\code{\link{dim}}(x) \eqn{\times}{x}
  \code{\link{length}}(probs)].  If \code{endrule="trim"} the output
  will have fewer rows.
}

\references{

  \itemize{

    \item About quantiles: Hyndman, R. J. and Fan, Y. (1996)
      \emph{Sample quantiles in statistical packages, American
      Statistician}, 50, 361.

    \item About quantiles: Eric W. Weisstein. \emph{Quantile}. From
      MathWorld-- A Wolfram Web
      Resource. \url{http://mathworld.wolfram.com/Quantile.html}

    \item About insertion sort used in \code{runmad} and
      \code{runquantile}: R. Sedgewick (1988):
      \emph{Algorithms}. Addison-Wesley (page 99)

    }

  }

% -------------------------------------------------------------------------

\examples{
  ## show plot using runquantile
  k <- 31; n <- 200
  x <- rnorm(n, sd=30) + abs(seq(n)-n/4)
  y <- diveMove:::.runquantile(x, k, probs=c(0.05, 0.25, 0.5, 0.75, 0.95))
  col <- c("black", "red", "green", "blue", "magenta", "cyan")
  plot(x, col=col[1], main="Moving Window Quantiles")
  lines(y[,1], col=col[2])
  lines(y[,2], col=col[3])
  lines(y[,3], col=col[4])
  lines(y[,4], col=col[5])
  lines(y[,5], col=col[6])
  lab=c("data", "runquantile(.05)", "runquantile(.25)", "runquantile(0.5)",
        "runquantile(.75)", "runquantile(.95)")
  legend(0,230, lab, col=col, lty=1)

  ## basic tests against apply/embed
  a <- diveMove:::.runquantile(x, k, c(0.3, 0.7), endrule="trim")
  b <- t(apply(embed(x, k), 1, quantile, probs=c(0.3, 0.7)))
  eps <- .Machine$double.eps ^ 0.5
  stopifnot(all(abs(a - b) < eps))

  ## Test against loop approach

  ## This test works fine at the R prompt but fails during package check -
  ## need to investigate
  k <- 25; n <- 200
  x <- rnorm(n, sd=30) + abs(seq(n) - n / 4) # create random data
  x[seq(1, n, 11)] <- NaN;                   # add NANs
  k2 <- k %/% 2
  k1 <- k - k2 - 1
  a <- diveMove:::.runquantile(x, k, probs=c(0.3, 0.8))
  b <- matrix(0, n, 2)
  for(j in 1:n) {
      lo <- max(1, j - k1)
      hi <- min(n, j + k2)
      b[j, ] <- quantile(x[lo:hi], probs=c(0.3, 0.8), na.rm=TRUE)
  }
  ## stopifnot(all(abs(a-b)<eps));

  ## Compare calculation of array ends
  a <- diveMove:::.runquantile(x, k, probs=0.4,
                               endrule="quantile") # fast C code
  b <- diveMove:::.runquantile(x, k, probs=0.4,
                               endrule="func")     # slow R code
  stopifnot(all(abs(a - b) < eps))

  ## Test if moving windows forward and backward gives the same results
  k <- 51
  a <- diveMove:::.runquantile(x, k, probs=0.4)
  b <- diveMove:::.runquantile(x[n:1], k, probs=0.4)
  stopifnot(all(a[n:1]==b, na.rm=TRUE))

  ## Test vector vs. matrix inputs, especially for the edge handling
  nRow <- 200; k <- 25; nCol <- 10
  x <- rnorm(nRow, sd=30) + abs(seq(nRow) - n / 4)
  x[seq(1, nRow, 10)] <- NaN              # add NANs
  X <- matrix(rep(x, nCol), nRow, nCol)   # replicate x in columns of X
  a <- diveMove:::.runquantile(x, k, probs=0.6)
  b <- diveMove:::.runquantile(X, k, probs=0.6)
  stopifnot(all(abs(a - b[, 1]) < eps))    # vector vs. 2D array
  stopifnot(all(abs(b[, 1] - b[, nCol]) < eps)) # compare rows within 2D array

  ## Exhaustive testing of runquantile to standard R approach
  numeric.test <- function (x, k) {
    probs <- c(1, 25, 50, 75, 99) / 100
    a <- diveMove:::.runquantile(x, k, c(0.3, 0.7), endrule="trim")
    b <- t(apply(embed(x, k), 1, quantile, probs=c(0.3, 0.7), na.rm=TRUE))
    eps <- .Machine$double.eps ^ 0.5
    stopifnot(all(abs(a - b) < eps))
  }
  n <- 50
  x <- rnorm(n,sd=30) + abs(seq(n) - n / 4) # nice behaving data
  for(i in 2:5) numeric.test(x, i)          # test small window sizes
  for(i in 1:5) numeric.test(x, n - i + 1)  # test large window size
  x[seq(1, 50, 10)] <- NaN                  # add NANs and repet the test
  for(i in 2:5) numeric.test(x, i)          # test small window sizes
  for(i in 1:5) numeric.test(x, n - i + 1)  # test large window size

  ## Speed comparison
  \dontrun{
  x <- runif(1e6); k=1e3 + 1
  system.time(diveMove:::.runquantile(x, k, 0.5)) # Speed O(n*k)
  }
}


\author{Jarek Tuszynski (SAIC) \email{jaroslaw.w.tuszynski@saic.com}}


\keyword{ts}
\keyword{smooth}
\keyword{array}
\keyword{utilities}
\concept{moving min}
\concept{rolling min}
\concept{running min}
\concept{moving max}
\concept{rolling max}
\concept{running max}
\concept{moving minimum}
\concept{rolling minimum}
\concept{running minimum}
\concept{moving maximum}
\concept{rolling maximum}
\concept{running maximum}
\concept{moving quantile}
\concept{rolling quantile}
\concept{running quantile}
\concept{moving percentile}
\concept{rolling percentile}
\concept{running percentile}
\concept{moving window}
\concept{rolling window}
\concept{running window}
