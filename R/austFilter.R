##' Filter satellite locations
##'
##' Apply a three stage algorithm to eliminate erroneous locations, based
##' on established procedures.
##'
##' These functions implement the location filtering procedure outlined in
##' Austin et al. (2003).  \code{grpSpeedFilter} and \code{rmsDistFilter}
##' can be used to perform only the first stage or the second and third
##' stages of the algorithm on their own, respectively.  Alternatively, the
##' three filters can be run in a single call using \code{austFilter}.
##'
##' The first stage of the filter is an iterative process which tests every
##' point, except the first and last (\var{w}/2) - 1 (where \var{w} is the
##' window size) points, for travel velocity relative to the
##' preceeding/following (\var{w}/2) - 1 points. If all \var{w} - 1 speeds
##' are greater than the specified threshold, the point is marked as
##' failing the first stage. In this case, the next point is tested,
##' removing the failing point from the set of test points.
##'
##' The second stage runs McConnell et al. (1992) algorithm, which tests
##' all the points that passed the first stage, in the same manner as
##' above. The root mean square of all \var{w} - 1 speeds is calculated,
##' and if it is greater than the specified threshold, the point is marked
##' as failing the second stage (see Warning section below).
##'
##' The third stage is run simultaneously with the second stage, but if the
##' mean distance of all \var{w} - 1 pairs of points is greater than the
##' specified threshold, then the point is marked as failing the third
##' stage.
##'
##' The speed and distance threshold should be obtained separately (see
##' \code{\link{distSpeed}}).
##'
##' @param time \code{POSIXct} object with dates and times for each point.
##' @param lon numeric vectors of longitudes, in decimal degrees.
##' @param lat numeric vector of latitudes, in decimal degrees.
##' @param id A factor grouping points in different categories
##'     (e.g. individuals).
##' @param speed.thr numeric scalar: speed threshold (m/s) above which
##'     filter tests should fail any given point.
##' @param dist.thr numeric scalar: distance threshold (km) above which the
##'     last filter test should fail any given point.
##' @param window integer: the size of the moving window over which tests
##'     should be carried out.
##' @param ... Arguments ultimately passed to \code{\link{distSpeed}}.
##' @section Warning:
##' This function applies McConnell et al.'s filter as described in Freitas
##' et al. (2008).  According to the original description of the algorithm
##' in McConnell et al. (1992), the filter makes a single pass through all
##' locations.  Austin et al. (2003) and other authors may have used the
##' filter this way.  However, as Freitas et al. (2008) noted, this causes
##' locations adjacent to those flagged as failing to fail also, thereby
##' rejecting too many locations.  In diveMove, the algorithm was modified
##' to reject only the \dQuote{peaks} in each series of consecutive
##' locations having root mean square speed higher than threshold.
##' @return
##' \code{rmsDistFilter} and \code{austFilter} return a matrix with 2 or 3
##' columns, respectively, of logical vectors with values TRUE for points
##' that passed each stage.  For the latter, positions that fail the first
##' stage fail the other stages too.  The second and third columns returned
##' by \code{austFilter}, as well as those returned by \code{rmsDistFilter}
##' are independent of one another; i.e. positions that fail stage 2 do not
##' necessarily fail stage 3.
##' @author Sebastian Luque \email{spluque@@gmail.com} and Andy Liaw.
##' @references
##' McConnell BJ, Chambers C, Fedak MA. 1992. Foraging ecology of southern
##' elephant seals in relation to bathymetry and productivity of the
##' Southern Ocean. \emph{Antarctic Science} 4:393-398.
##'
##' Austin D, McMillan JI, Bowen D. 2003. A three-stage algorithm for
##' filtering erroneous Argos satellite locations. \emph{Marine Mammal
##' Science} 19: 371-383.
##'
##' Freitas C, Lydersen, C, Fedak MA, Kovacs KM. 2008. A simple new
##' algorithm to filter marine mammal ARGOS locations. Marine Mammal
##' Science DOI: 10.1111/j.1748-7692.2007.00180.x
##' @keywords manip iteration
##' @seealso \code{\link{distSpeed}}
##' @examples
##' ## Using the Example from '?readLocs':
##' utils::example("readLocs", package="diveMove",
##'                ask=FALSE, echo=FALSE)
##' ringy <- subset(locs, id == "ringy" & !is.na(lon) & !is.na(lat))
##'
##' ## Examples below use default Meeus algorithm for computing distances.
##' ## See ?distSpeed for specifying other methods.
##' ## Austin et al.'s group filter alone
##' grp <- grpSpeedFilter(ringy[, 3:5], speed.thr=1.1)
##'
##' ## McConnell et al.'s filter (root mean square test), and distance test
##' ## alone
##' rms <- rmsDistFilter(ringy[, 3:5], speed.thr=1.1, dist.thr=300)
##'
##' ## Show resulting tracks
##' n <- nrow(ringy)
##' plot.nofilter <- function(main) {
##'     plot(lat ~ lon, ringy, type="n", main=main)
##'     with(ringy, segments(lon[-n], lat[-n], lon[-1], lat[-1]))
##' }
##' layout(matrix(1:4, ncol=2, byrow=TRUE))
##' plot.nofilter(main="Unfiltered Track")
##' plot.nofilter(main="Group Filter")
##' n1 <- length(which(grp))
##' with(ringy[grp, ], segments(lon[-n1], lat[-n1], lon[-1], lat[-1],
##'                             col="blue"))
##' plot.nofilter(main="Root Mean Square Filter")
##' n2 <- length(which(rms[, 1]))
##' with(ringy[rms[, 1], ], segments(lon[-n2], lat[-n2], lon[-1], lat[-1],
##'                                  col="red"))
##' plot.nofilter(main="Distance Filter")
##' n3 <- length(which(rms[, 2]))
##' with(ringy[rms[, 2], ], segments(lon[-n3], lat[-n3], lon[-1], lat[-1],
##'                                  col="green"))
##'
##' ## All three tests (Austin et al. procedure)
##' austin <- with(ringy, austFilter(time, lon, lat, speed.thr=1.1,
##'                                  dist.thr=300))
##' layout(matrix(1:4, ncol=2, byrow=TRUE))
##' plot.nofilter(main="Unfiltered Track")
##' plot.nofilter(main="Stage 1")
##' n1 <- length(which(austin[, 1]))
##' with(ringy[austin[, 1], ], segments(lon[-n1], lat[-n1], lon[-1], lat[-1],
##'                                     col="blue"))
##' plot.nofilter(main="Stage 2")
##' n2 <- length(which(austin[, 2]))
##' with(ringy[austin[, 2], ], segments(lon[-n2], lat[-n2], lon[-1], lat[-1],
##'                                     col="red"))
##' plot.nofilter(main="Stage 3")
##' n3 <- length(which(austin[, 3]))
##' with(ringy[austin[, 3], ], segments(lon[-n3], lat[-n3], lon[-1], lat[-1],
##'                                     col="green"))
"austFilter" <- function(time, lon, lat, id=gl(1, 1, length(time)),
                         speed.thr, dist.thr, window=5, ...)
{
    ## FIRST STAGE ********************************************************
    locs <- data.frame(time, lon, lat)

    ## Do first stage over each seal's data, returns vector as long as locs
    first <- unlist(by(locs, id, grpSpeedFilter, speed.thr, window, ...),
                    use.names=FALSE)

    ## SECOND AND THIRD STAGES ********************************************
    good <- which(first)               # native subscripts that passed
    last <- do.call(rbind, by(locs[good, ], id[good], rmsDistFilter,
                              speed.thr, window, dist.thr, ...))
    filter123 <- cbind(firstPass=first,
                       secondPass=first, # 2nd and 3rd start the same as 1st
                       thirdPass=first)
    filter123[good, 2:3] <- last
    filter123
}

##' @describeIn austFilter Do stage one on 3-column matrix \code{x}
##' @param x 3-column matrix with column 1: \code{POSIXct} vector; column
##'     2: numeric longitude vector; column 3: numeric latitude vector.
##' @return \code{grpSpeedFilter} logical vector indicating those lines
##'     that passed the test.
"grpSpeedFilter" <- function(x, speed.thr, window=5, ...)
{
    ## Value: Do stage one on matrix x (assuming it's a single unit),
    ## return a logical; whether each observation in x passed the test
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with cols: POSIXct, lon, lat; speed.thr=speed
    ## threshold (m/s), window=size of window to test; ...=arguments passed
    ## to distSpeed(), namely only 'method' for now.
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
        speeds <- distSpeed(k[mid.rep, ], k[mid + ref, ], ...)[, 3]
        all(speeds > speed.thr, na.rm=TRUE) # TRUE if all speeds > thr
    }
    pass <- !logical(nrow(x))         # all pass at start up
    ## define all test rows and subscript for forward movement of window
    testrows <- seq(1 + tpos, nrow(x) - tpos); i <- 1
    for (j in testrows) {
        test <- testfun(x[c(i:(i + tpos - 1), j:(j + tpos)), ])
        if (test) {
            pass[j] <- FALSE
        } else {
            i <- i + 1
        }
    }
    pass
}

##' @describeIn austFilter Apply McConnell et al's filter and Austin et
##'     al's last stage
"rmsDistFilter" <- function(x, speed.thr, window=5, dist.thr, ...)
{
    ## Value: Run McConnell et al's filter and Austin et al's last stage,
    ## return 2-col matrix of logicals; whether each observation passed
    ## each test.
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with cols: POSIXct, lon, lat; speed.thr=speed
    ## threshold (m/s), window=size of window to test; dist.thr=distance
    ## threshold (km); ...=arguments passed to distSpeed(), namely only
    ## 'method' for now.
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!window %% 2) stop ("window size must be an odd number")
    wdw.errmess <- "there are fewer rows than window size"
    if (nrow(x) < window) stop (wdw.errmess)
    tpos <- window %/% 2                      # test subscript - 1
    ref <- c(-seq(tpos), seq(tpos))           # reference points for test
    travel.fun <- function(k) {           # k=subscripts
        xmid <- k[1]                      # 1st is the middle
        xmid.rep <- rep(xmid, length(k) - 1)
        others <- k[-1]                 # 1st against all other positions
        tr <- distSpeed(x[xmid.rep, ], x[others, ], ...)
        tr[, c(1, 3)]
    }
    testrows <- seq(tpos + 1, nrow(x) - tpos) # subscripts of locs to test
    rmsSwitch <- distPass <- !logical(length(testrows)) # to begin looping

    while (any(rmsSwitch)) {           # stop when switch is all FALSE
        switchidx <- which(rmsSwitch)
        if ((length(switchidx) + (tpos * 2)) < window) stop (wdw.errmess)
        testrows.new <- testrows[rmsSwitch]
        idx <- seq_along(testrows.new)  # index the above
        testidx.mtx <- testidx <- c(idx, sapply(ref, "+", idx))
        testidx.inner <- testidx >= 1 & testidx <= length(idx)
        testidx.mtx[testidx.inner] <- testrows.new[testidx[testidx.inner]]
        testidx.low <- testidx < 1
        testidx.mtx[testidx.low] <- testrows[1] - (1 - testidx[testidx.low])
        testidx.high <- testidx > length(idx)
        testidx.mtx[testidx.high] <- testrows[length(testrows)] +
            (testidx[testidx.high] - length(idx))
        testidx.mtx <- matrix(testidx.mtx, nrow=length(idx))
        travel <- apply(testidx.mtx, 1, travel.fun)
        if (dim(travel)[1] > 2) {
            dist.refs <- seq(length(ref))
            speed.refs <- seq(length(ref) + 1, nrow(travel))
        } else {
            dist.refs <- 1
            speed.refs <- 2
        }
        dists <- as.matrix(travel[dist.refs, ])
        speeds <- as.matrix(travel[speed.refs, ])
        ## root mean square value
        rms <- apply(speeds, 2, function(k) { # do this for every test group
            sqrt(sum(k ^ 2, na.rm=TRUE) / length(k))
        })
        ## prelim passing locs in current set
        rmsPass <- rms <= speed.thr
        ## find series of adj prelim failing locs in current set
        rmsPass.rle <- rle(rmsPass)
        bad <- which(!rmsPass.rle$values & rmsPass.rle$lengths > 1)
        if (length(bad) < 1) break  # stop looping if no adjc failing locs
        beg <- rev(length(rmsPass) + 1 - cumsum(rev(rmsPass.rle$lengths)))
        end <- cumsum(rmsPass.rle$lengths)
        for (j in bad) {
            bads <- seq(beg[j], end[j])
            Vi <- rms[bads]
            maxVi.idx <- which(Vi == max(Vi))
            peaks <- bads[maxVi.idx]
            ok <- bads[!bads %in% peaks]
            rmsPass[ok] <- TRUE
        }
        rmsSwitch[switchidx[!rmsPass]] <- FALSE # set failing locs in orig set
        ## Distance filter
        distt <- apply(dists, 2, function(k) sum(k, na.rm=TRUE) / length(k))
        distFail <- distt > dist.thr
        distPass[switchidx[distFail]] <- FALSE
    }

    rmsPass <- rmsSwitch
    ## Top and bottom ends pass as these can't be tested
    untested <- matrix(!logical(tpos * 2), ncol=2)
    rbind(untested, cbind(rmsPass, distPass), untested)
}


## TEST ZONE --------------------------------------------------------------
