\name{distSpeed}

\alias{distSpeed}

% -------------------------------------------------------------------------

\title{Calculate distance and speed between locations}



\description{Calculate distance, time difference, and speed between
  pairs of points defined by latitude and longitude, given the time at
  which all points were measured.}


\usage{
distSpeed(pt1, pt2)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{pt1}{A matrix or data frame with three columns; the first a
    \code{POSIXct} object with dates and times for all points, the
    second and third numeric vectors of longitude and latitude for all
    points, respectively, in decimal degrees.}

  \item{pt2}{A matrix with the same size and structure as \code{pt1}.}

}


\value{A matrix with three columns: distance (km), time difference (s),
  and speed (m/s).}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\examples{

locs <- readLocs(system.file(file.path("data", "sealLocs.csv"),
                             package="diveMove"), idCol=1, dateCol=2,
                  dtformat="\%Y-\%m-\%d \%H:\%M:\%S", classCol=3, lonCol=4,
                  latCol=5)

## Travel summary between successive standard locations
locs.std <- subset(locs, subset=class == "0" | class == "1" |
                   class == "2" | class == "3" &
                   !is.na(lon) & !is.na(lat))
locs.std.tr <- by(locs.std, locs.std$id, function(x) {
    distSpeed(x[-nrow(x), 3:5], x[-1, 3:5])
})
lapply(locs.std.tr, head)

## Particular quantiles from travel summaries
lapply(locs.std.tr, function(x) {
    quantile(x[, 3], seq(0.90, 0.99, 0.01), na.rm=TRUE) # speed
})
lapply(locs.std.tr, function(x) {
    quantile(x[, 1], seq(0.90, 0.99, 0.01), na.rm=TRUE) # distance
})

## Travel summary between arbitrary two sets of points
distSpeed(locs[c(1, 5, 10), 3:5], locs[c(25, 30, 35), 3:5])

}



\keyword{math}% at least one, from doc/KEYWORDS
\keyword{manip}
