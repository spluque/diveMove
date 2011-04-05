% $Id: $
\name{plotZOC-methods}
\docType{methods}
\alias{plotZOC-methods}

\alias{plotZOC}

\alias{plotZOC,TDR,matrix-method}
\alias{plotZOC,TDR,TDRcalibrate-method}

% -------------------------------------------------------------------------

\title{Methods for visually assessing results of ZOC procedure}


\description{ Plots for comparing the zero-offset corrected depth from a
  \code{\link{TDRcalibrate}} object with the uncorrected data in a
  \code{\link{TDR}} object, or the progress in each of the filters
  during recursive filtering for ZOC (\code{\link{calibrateDepth}}). }


\usage{

\S4method{plotZOC}{TDR,matrix}(x, y, xlim, ylim, ylab="Depth (m)", \ldots)
\S4method{plotZOC}{TDR,TDRcalibrate}(x, y, xlim, ylim, ylab="Depth (m)", \ldots)

}


\arguments{

  \item{x}{\code{TDR} object.}

  \item{y}{matrix with the same number of rows as there are observations
    in \code{x}, or a \code{TDRcalibrate} object.}

  \item{xlim}{\code{POSIXct} or numeric vector of length 2, with lower
    and upper limits of time to be plotted.  Defaults to time range of
    input.}

  \item{ylim}{numeric vector of length 2 (upper, lower) with axis
    limits.  Defaults to range of input.}

  \item{ylab}{character strings to label the corresponding y-axis.}

  \item{\ldots}{Arguments passed to \code{\link{legend}}.}

}


\details{

  The \code{TDR},\code{matrix} method produces a plot like those shown
  in Luque and Fried (2011).

  The \code{TDR},\code{TDRcalibrate} method overlays the corrected depth
  from the second argument over that from the first.

}


\references{

  Luque, S.P. and Fried, R. (2011) Recursive filtering for zero offset
  correction of diving depth time series. PLoS ONE 6:e15850

}


\section{Methods}{
  \describe{

    \item{plotTDR}{\code{signature(x="TDR", y="matrix")}: This plot
      helps in finding appropriate parameters for
      \code{diveMove:::.depthFilter}, and consists of three panels.  The
      upper panel shows the original data, the middle panel shows the
      filters, and the last panel shows the corrected
      data. method=\dQuote{visual} in \code{\link{calibrateDepth}}.}

    \item{plotTDR}{\code{signature(x="TDR", y="TDRcalibrate")}: This
      plots depth from the \code{TDRcalibrate} object over the one from
      the \code{TDR} object.}

    }

}


\value{ Nothing; a plot as side effect. }


\seealso{ \code{\link{calibrateDepth}}, \code{\link{.zoc}} }

% -------------------------------------------------------------------------

\examples{

## Using the Example from '?diveStats':

utils::example("diveStats", package="diveMove",
               ask=FALSE, echo=FALSE)

## Plot filters for ZOC
## Work on first trip subset, to save processing time, since
## there's no drift nor shifts between trips
tdr <- divesTDR[1:15000]
## Try window widths (K), quantiles (P) and bound the search (db)
K <- c(3, 360); P <- c(0.5, 0.02); db <- c(0, 5)
d.filter <- diveMove:::.depthFilter(depth=getDepth(tdr),
                                    k=K, probs=P, depth.bounds=db,
                                    na.rm=TRUE)
old.par <- par(no.readonly=TRUE)
plotZOC(tdr, d.filter, ylim=c(0, 6))
par(old.par)

## Plot corrected and uncorrected depth, regardless of method
## Look at three different scales
xlim1 <- c(getTime(divesTDR)[7100], getTime(divesTDR)[11700])
xlim2 <- c(getTime(divesTDR)[7100], getTime(divesTDR)[7400])
xlim3 <- c(getTime(divesTDR)[7100], getTime(divesTDR)[7200])
par(mar=c(3, 4, 0, 1) + 0.1, cex=1.1, las=1)
layout(seq(3))
plotZOC(divesTDR, dcalib, xlim=xlim1, ylim=c(0, 6))
plotZOC(divesTDR, dcalib, xlim=xlim2, ylim=c(0, 70))
plotZOC(divesTDR, dcalib, xlim=xlim3, ylim=c(0, 70))
par(old.par)

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}

\keyword{methods}
\keyword{iplot}
