##' Calculate distance and speed between locations
##'
##' Calculate distance, time difference, and speed between pairs of points
##' defined by latitude and longitude, given the time at which all points were
##' measured.
##'
##' @param pt1 A matrix or \code{\link{data.frame}} with three columns; the
##' first a \code{POSIXct} object with dates and times for all points, the
##' second and third numeric vectors of longitude and latitude for all points,
##' respectively, in decimal degrees.
##' @param pt2 A matrix with the same size and structure as \code{pt1}.
##' @param method character indicating which of the distance algorithms from
##' \code{\link[geosphere]{geosphere-package}} to use (only default parameters
##' used).  Only \code{Meeus} and \code{VincentyEllipsoid} are supported for
##' now.
##' @return A matrix with three columns: distance (km), time difference (s),
##' and speed (m/s).
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @keywords math manip
##' @examples
##' ## Using the Example from '?readLocs':
##' utils::example("readLocs", package="diveMove",
##'                ask=FALSE, echo=FALSE)
##'
##' ## Travel summary between successive standard locations
##' locs.std <- subset(locs, subset=class == "0" | class == "1" |
##'                    class == "2" | class == "3" &
##'                    !is.na(lon) & !is.na(lat))
##' ## Default Meeus method
##' locs.std.tr <- by(locs.std, locs.std$id, function(x) {
##'     distSpeed(x[-nrow(x), 3:5], x[-1, 3:5])
##' })
##' lapply(locs.std.tr, head)
##'
##' ## Particular quantiles from travel summaries
##' lapply(locs.std.tr, function(x) {
##'     quantile(x[, 3], seq(0.90, 0.99, 0.01), na.rm=TRUE) # speed
##' })
##' lapply(locs.std.tr, function(x) {
##'     quantile(x[, 1], seq(0.90, 0.99, 0.01), na.rm=TRUE) # distance
##' })
##'
##' ## Travel summary between two arbitrary sets of points
##' pts <- seq(10)
##' (meeus <- distSpeed(locs[pts, 3:5], locs[pts + 1, 3:5]))
##' (vincenty <- distSpeed(locs[pts, 3:5],
##'                        locs[pts + 1, 3:5],
##'                        method="VincentyEllipsoid"))
##' meeus - vincenty
"distSpeed" <- function(pt1, pt2, method=c("Meeus", "VincentyEllipsoid"))
{
    ## Value: A 3-column matrix with distance, time elapsed and speed
    ## between two points or set of points.
    ## --------------------------------------------------------------------
    ## Arguments: pt1 and pt2=matrices for each point, with three columns;
    ## the first for a POSIXct object with time for each point, the second
    ## for longitude, and the third for latitude.  method=character; which
    ## of the distance algorithms from geosphere package to use (only
    ## default parameters used).
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (nrow(pt1) != nrow(pt2)) {
        stop("pt1 and pt2 must have the same number of rows")
    } else if (ncol(pt1) != 3 || ncol(pt2) != 3) {
        stop("pt1 and pt2 must both have 3 columns")
    } else if (nrow(pt1) < 1 || ncol(pt2) < 1) {
        stop("pt1 and pt2 must each have at least 1 row")
    }
    method <- match.arg(method)
    switch(method,
           Meeus = {
               distance <- distMeeus(pt1[, 2:3], pt2[, 2:3])
           },
           VincentyEllipsoid = {
               distance <- distVincentyEllipsoid(pt1[, 2:3], pt2[, 2:3])
           })
    pt1[, 1] <- as.numeric(pt1[, 1])
    pt2[, 1] <- as.numeric(pt2[, 1])
    ## Distance (in Km)
    distance <- distance / 1000
    ## Calculate time difference (in seconds) between locations.
    timdiff <- abs(pt1[, 1] - pt2[, 1])
    ## Speed in m/s.
    speed <- ifelse(timdiff == 0, 0, (distance * 1000) / timdiff)
    cbind(distance, time.elapsed=timdiff, speed)
}
