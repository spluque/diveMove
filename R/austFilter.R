## $Id: austFilter.R,v 1.3 2007-02-15 17:20:05 sluque Exp $

"grpSpeedFilter" <- function(x, speed.thr, window=5)
{
    ## Value: Do stage one on matrix x (assuming it's a single unit),
    ## return a logical; whether each observation in x failed the test
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with cols: POSIXct, lon, lat; speed.thr=speed
    ## threshold (m/s), window=size of window to test
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!window %% 2) stop ("window size must be an odd number")
    if (nrow(x) < window) stop ("there are fewer rows than window size")
    tpos <- window %/% 2                # test subscript - 1
    testfun <- function(k) {            # k=matrix with group to test
        mid <- tpos + 1                 # subscript of pt to test
        ref <- c(-seq(tpos), seq(tpos)) # subscripts of pts to test against
        mid.rep <- rep(mid, length(ref))
        speeds <- distSpeed(k[mid.rep, ], k[mid + ref, ])[, 3]
        all(speeds <= speed.thr, na.rm=TRUE) # FALSE if some speeds are > thr
    }
    pass <- !logical(nrow(x))         # all pass at start up
    ## define all test rows and subscript for forward movement of window
    testrows <- seq(1 + tpos, nrow(x) - tpos); i <- 1
    for (j in testrows) {
        test <- testfun(x[c(i:(i + tpos - 1), j:(j + tpos)), ])
        if(!test) {
            pass[j] <- FALSE
        } else {
            i <- i + 1
        }
    }
    pass
}


"rmsDistFilter" <- function(x, speed.thr, window=5, dist.thr)
{
    ## Value: Apply McConnell et al's filter and Austin et al's last
    ## stage, return 2-col matrix of logicals; whether each observation
    ## failed each test.  These 2 filters are independent of each other.
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with cols: POSIXct, lon, lat; speed.thr=speed
    ## threshold (m/s), window=size of window to test; dist.thr=distance
    ## threshold (km)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!window %% 2) stop ("window size must be an odd number")
    if (nrow(x) < window) stop ("there are fewer rows than window size")
    tpos <- window %/% 2                      # test subscript - 1
    testrows <- seq(1 + tpos, nrow(x) - tpos) # rel subscripts of pts to test
    ref <- c(-seq(tpos), seq(tpos))     # reference points for test
    ## Matrix of test subscripts
    testmtx <- cbind(testrows, sapply(ref, "+", testrows))
    dist.fun <- function(k) {           # k=subscripts
        xmid <- k[1]                    # 1st is the middle
        xmid.rep <- rep(xmid, length(k) - 1)
        others <- k[-1]                 # stats to all other positions
        tr <- distSpeed(x[xmid.rep, ], x[others, ])
        tr[, c(1, 3)]
    }
    travel <- apply(testmtx, 1, dist.fun)
    dists <- travel[seq(length(ref)), ]
    speeds <- travel[seq(length(ref) + 1, nrow(travel)), ]

    ## root mean square value (Mcconnell et al filter)
    rms <- apply(speeds, 2, function(k) { # do this for every test group
        sqrt(sum(k ^ 2, na.rm=TRUE) / length(k))
    })
    rmsPass <- rms <= speed.thr

    ## Distance filter
    distt <- apply(dists, 2, function(k) sum(k, na.rm=TRUE) / length(k))
    distPass <- distt <= dist.thr

    ## Top and bottom ends pass as these can't be tested
    untested <- matrix(!logical(tpos * 2), ncol=2)
    rbind(untested, cbind(rmsPass, distPass), untested)
}


"austFilter" <- function(time, lon, lat, id=gl(1, 1, length(time)),
                         speed.thr, dist.thr, window=5)
{
    ## Value: A matrix with logicals indicating whether each reading
    ## failed each filter.  This runs the filters in Austin et al. (2003).
    ## Results are presented from each filter, independently of the others
    ## for flexibility.
    ## --------------------------------------------------------------------
    ## Arguments: lat and lon=latitude and longitude vectors in degrees;
    ## time=POSIXct object with times for each point; id=factor
    ## identifying sections of the data to be treated separately;
    ## speed.thr=speed threshold (m/s); dist.thr=distance threshold (km);
    ## window=size of window to test
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## FIRST STAGE
    ## ********************************************************
    locs <- data.frame(time, lon, lat)

    ## Do first stage over each seal's data, returns vector as long as locs
    first <- unlist(by(locs, id, grpSpeedFilter, speed.thr, window),
                    use.names=FALSE)

    ## SECOND AND THIRD STAGES ********************************************
    good <- which(first)               # native subscripts that passed
    last <- do.call(rbind, by(locs[good, ], id[good], rmsDistFilter,
                              speed.thr, window, dist.thr))
    filter123 <- cbind(firstPass=first,
                       secondPass=first, # 2nd and 3rd start the same as 1st
                       thirdPass=first)
    filter123[good, 2:3] <- last
    filter123
}

## TEST ZONE --------------------------------------------------------------
