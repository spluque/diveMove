\name{austFilter}

\alias{grpSpeedFilter}
\alias{rmsDistFilter}
\alias{austFilter}

% -------------------------------------------------------------------------

\title{Filter satellite locations}



\description{Apply a three stage algorithm to eliminate erroneous
  locations, based on the procedure outlined in Austin et al. (2003).}


\usage{
austFilter(time, lon, lat, id=gl(1, 1, length(time)),
           speed.thr, dist.thr, window=5)
grpSpeedFilter(x, speed.thr, window=5)
rmsDistFilter(x, speed.thr, window=5, dist.thr)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{time}{\code{POSIXct} object with dates and times for each
    point.}

  \item{lon}{Numeric vectors of longitudes, in decimal degrees.}

  \item{lat}{Numeric vector of latitudes, in decimal degrees.}

  \item{id}{A factor grouping points in different categories
    (e.g. individuals).}

  \item{speed.thr}{Speed threshold (m/s) above which filter tests
    should fail any given point.}

  \item{dist.thr}{Distance threshold (km) above which the last filter
    test should fail any given point.}

  \item{window}{Integer indicating the size of the moving window over
    which tests should be carried out.}

  \item{x}{3-column matrix with column 1: \code{POSIXct} vector; column
    2: numeric longitude vector; column 3: numeric latitude vector.}

}


\details{These functions implement the location filtering procedure
  outlined in Austin et al. (2003).  \code{grpSpeedFilter} and
  \code{rmsDistFilter} can be used to perform only the first stage or
  the second and third stages of the algorithm on their own,
  respectively.  Alternatively, the three filters can be run
  sequentially using \code{austFilter}.

  The first stage of the filter is an iterative process which tests
  every point, except the first and last two, for travel velocity
  relative to the preceeding/following two points. If all these four
  speeds are greater than the specified threshold, the point is marked
  as failing the first stage. In this case, the next point is tested,
  removing the failing point from the set of test points.

  The second stage runs McConnell et al. (1992) algorithm, which tests
  all the points that passed the first stage, in the same manner as
  above. The root mean square of all four speeds is calculated, and if
  it is greater than the specified threshold, the point is marked as
  failing the second stage.

  The third stage is run simultaneously with the second stage, but if
  the mean distance of all four pairs of points is greater than the
  specified threshold, then the point is marked as failing the third
  stage.

  The speed and distance threshold should be obtained separately (see
  \code{\link{distSpeed}}).}


\section{Warning}{This function applies McConnell et al.'s filter as
described in Austin et al. (2003), but other authors may have used the
filter differently.  Austin et al. (2003) have apparently applied the
filter in a vectorized manner.  It is not clear from the original paper
whether the filter is applied iteratively or in a vectorized fashion, so
it seems to be used inconsistently.}


\value{

  \code{grpSpeedFilter} returns a logical vector indicating those lines
  that passed the test.

  \code{rmsDistFilter} and \code{austFilter} return a matrix with 2 or 3
  columns, respectively, of logical vectors with values TRUE for points
  that passed each stage.  For the latter, positions that fail the first
  stage fail the other stages too.  The second and third columns
  returned by \code{austFilter}, as well as those returned by
  \code{rmsDistFilter} are independent of one another; i.e. positions
  that fail stage 2 do not necessarily fail stage 3.

}


\seealso{\code{\link{distSpeed}}}


\references{McConnell BJ, Chambers C, Fedak MA. 1992. Foraging ecology
  of southern elephant seals in relation to bathymetry and productivity
  of the Southern Ocean. \emph{Antarctic Science} 4:393-398.

  Austin D, McMillan JI, Bowen D. 2003. A three-stage algorithm for
  filtering erroneous Argos satellite locations. \emph{Marine Mammal
  Science} 19: 371-383.}

% -------------------------------------------------------------------------

\examples{
locs <- readLocs(system.file(file.path("data", "sealLocs.csv"),
                             package="diveMove"), idCol=1, dateCol=2,
                  dtformat="\%Y-\%m-\%d \%H:\%M:\%S", classCol=3, lonCol=4,
                  latCol=5)
ringy <- subset(locs, id == "ringy" & !is.na(lon) & !is.na(lat))

## Austin et al.'s group filter alone
grp <- grpSpeedFilter(ringy[, 3:5], speed.thr=1.1)

## McConnell et al.'s filter (root mean square test), and distance test alone
rms <- rmsDistFilter(ringy[, 3:5], speed.thr=1.1, dist.thr=300)

## Show resulting tracks
n <- nrow(ringy)
plot.nofilter <- function(main) {
    plot(lat ~ lon, ringy, type="n", main=main)
    with(ringy, segments(lon[-n], lat[-n], lon[-1], lat[-1]))
}
layout(matrix(1:4, ncol=2, byrow=TRUE))
plot.nofilter(main="Unfiltered Track")
plot.nofilter(main="Group Filter")
n1 <- length(which(grp))
with(ringy[grp, ], segments(lon[-n1], lat[-n1], lon[-1], lat[-1],
                            col="blue"))
plot.nofilter(main="Root Mean Square Filter")
n2 <- length(which(rms[, 1]))
with(ringy[rms[, 1], ], segments(lon[-n2], lat[-n2], lon[-1], lat[-1],
                                 col="red"))
plot.nofilter(main="Distance Filter")
n3 <- length(which(rms[, 2]))
with(ringy[rms[, 2], ], segments(lon[-n3], lat[-n3], lon[-1], lat[-1],
                                 col="green"))


## All three tests in sequence (Austin et al. procedure)
austin <- with(ringy, austFilter(time, lon, lat, speed.thr=1.1,
                                 dist.thr=300))
layout(matrix(1:4, ncol=2, byrow=TRUE))
plot.nofilter(main="Unfiltered Track")
plot.nofilter(main="Stage 1")
n1 <- length(which(austin[, 1]))
with(ringy[austin[, 1], ], segments(lon[-n1], lat[-n1], lon[-1], lat[-1],
                                    col="blue"))
plot.nofilter(main="Stage 2")
n2 <- length(which(austin[, 2]))
with(ringy[austin[, 2], ], segments(lon[-n2], lat[-n2], lon[-1], lat[-1],
                                    col="red"))
plot.nofilter(main="Stage 3")
n3 <- length(which(austin[, 3]))
with(ringy[austin[, 3], ], segments(lon[-n3], lat[-n3], lon[-1], lat[-1],
                                    col="green"))

}


\author{Sebastian P. Luque \email{spluque@gmail.com} and Andy Liaw.}


\keyword{manip}% at least one, from doc/KEYWORDS
\keyword{iteration}
