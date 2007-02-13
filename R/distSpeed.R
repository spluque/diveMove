## $Id: distSpeed.R,v 1.1.1.1.2.1 2007-02-13 21:51:53 sluque Exp $

"distSpeed" <- function(pt1, pt2)
{
    ## Value: A 3-column matrix with distance, time elapsed and speed
    ## between two points or set of points.
    ## --------------------------------------------------------------------
    ## Arguments: pt1 and pt2=matrices for each point, with three columns;
    ## the first for a POSIXct object with time for each point, the second
    ## for longitude, and the third for latitude.  speed=logical; should
    ## speed and time diffs be calculated?
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (nrow(pt1) != nrow(pt2)) {
        stop("pt1 and pt2 must have the same number of rows")
    } else if (ncol(pt1) != 3 || ncol(pt2) != 3) {
        stop("pt1 and pt2 must both have 3 columns")
    }
    pt1[, 1] <- as.numeric(pt1[, 1])
    pt2[, 1] <- as.numeric(pt2[, 1])
    ## Eccentricity of the Earth (squared polar/equatorial radii); km
    ecc <- sqrt(1 - (6356.75528816^2) / (6378.14^2))
    ## We assume original decimal angles and convert to radians.
    lat1 <- pt1[, 3] * (pi/180)
    lat2 <- pt2[, 3] * (pi/180)
    lon1 <- pt1[, 2] * (pi/180)
    lon2 <- pt2[, 2] * (pi/180)
    ## Difference in latitude and longitude between the points.
    dlat <- abs(lat2 - lat1)
    dlon <- abs(lon2 - lon1)
    ## Mean latitude among points.
    meanlat <- (lat1 + lat2) / 2
    ## Intermediate calculations.
    sindlat <- sin(dlat/2)^2
    coslat1 <- cos(lat1)
    coslat2 <- cos(lat2)
    sindlon <- sin(dlon/2)^2
    a <- sindlat + coslat1 * coslat2 * sindlon
    fac <- 2 * asin(sqrt(pmin(1, a)))
    ## Weighted earth radius average between the two locations.
    r1 <- 6378.14 * (1 - (ecc^2)) / (1 - (ecc^2) * (sin(meanlat)^2))^(3/2)
    r2 <- 6378.14 / sqrt(1 - (ecc^2) * (sin(meanlat)^2))
    ravg <- r1 * (dlat / (dlat + dlon)) + r2 * (dlon / (dlat + dlon))
    ## Distance (in Km)
    distance <- ifelse(dlat == 0 & dlon == 0, 0, ravg * fac)
    ## Calculate time difference (in seconds) between locations.
    timdiff <- abs(pt1[, 1] - pt2[, 1])
    ## Speed in m/s.
    speed <- ifelse(timdiff == 0, 0, (distance * 1000) / timdiff)
    cbind(distance, time.elapsed=timdiff, speed)
}
