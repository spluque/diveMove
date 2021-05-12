
".derivStats" <- function(x, diveNo)
{
    ## Value: Matrix with one row per dive, keeping order in 'x'
    ## --------------------------------------------------------------------
    ## Arguments: x=TDRcalibrate object; diveNo=numeric vector specifying
    ## which dives to obtain derivative statistics.
    ## --------------------------------------------------------------------
    ## Purpose: Provide summary statistics (summary() and sd()) of
    ## derivatives for descent, bottom, and ascent phases for each dive.
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    if (missing(diveNo)) {
        diveNo <- seq(max(getDAct(x, "dive.id")))
    } else {
        diveNo <- sort(unique(diveNo))
    }
    summarize.phase <- function(diveNo, phase, label) {
        der <- getDiveDeriv(x, diveNo=diveNo, phase=phase)
        y.nona <- der$y[!is.na(der$y)]
        der.summ <- summary(y.nona)
        names(der.summ) <- gsub("[ \\.]", "", tolower(names(der.summ)))
        der.sd <- sd(y.nona)
        der.m <- c(der.summ, sd=der.sd)
        names(der.m) <- paste(label, names(der.m), sep=".")
        der.m
    }
    d <- do.call(rbind,
                 lapply(diveNo, summarize.phase, "descent", "descD"))
    b <- do.call(rbind,
                 lapply(diveNo, summarize.phase, "bottom", "bottD"))
    a <- do.call(rbind,
                 lapply(diveNo, summarize.phase, "ascent", "ascD"))
    cbind(d, b, a)
}

##' Per-dive statistics
##'
##' Calculate dive statistics in \acronym{TDR} records.
##'
##' \code{diveStats} calculates various dive statistics based on time and
##' depth for an entire \acronym{TDR} record.  \code{oneDiveStats} obtains
##' these statistics from a single dive, and \code{stampDive} stamps each
##' dive with associated phase information.
##'
##' @aliases diveStats
##' @param x A \code{\link{TDRcalibrate-class}} object for \code{diveStats}
##'     and \code{stampDive}, and a \code{\link{data.frame}} containing a
##'     single dive's data (a factor identifying the dive phases, a POSIXct
##'     object with the time for each reading, a numeric depth vector, and
##'     a numeric speed vector) for \code{oneDiveStats}.
##' @param depth.deriv logical: should depth derivative statistics be
##'     calculated?
##' @param interval numeric scalar: sampling interval for interpreting
##'     \code{x}.
##' @param speed logical: should speed statistics be calculated?
##' @param ignoreZ logical: whether phases should be numbered considering
##'     all aquatic activities (\dQuote{W} and \dQuote{Z}) or ignoring
##'     \dQuote{Z} activities.
##' @return A \code{\link{data.frame}} with one row per dive detected
##'     (durations are in s, and linear variables in m):
##'
##' \item{begdesc}{A \code{POSIXct} object, specifying the start time of
##' each dive.}
##'
##' \item{enddesc}{A \code{POSIXct} object, as \code{begdesc} indicating
##' descent's end time.}
##'
##' \item{begasc}{A \code{POSIXct} object, as \code{begdesc} indicating the
##' time ascent began.}
##'
##' \item{desctim}{Descent duration of each dive.}
##'
##' \item{botttim}{Bottom duration of each dive.}
##'
##' \item{asctim}{Ascent duration of each dive.}
##'
##' \item{divetim}{Dive duration.}
##'
##' \item{descdist}{Numeric vector with last descent depth.}
##'
##' \item{bottdist}{Numeric vector with the sum of absolute depth
##' differences while at the bottom of each dive; measure of amount of
##' \dQuote{wiggling} while at bottom.}
##'
##' \item{ascdist}{Numeric vector with first ascent depth.}
##'
##' \item{bottdep.mean}{Mean bottom depth.}
##'
##' \item{bottdep.median}{Median bottom depth.}
##'
##' \item{bottdep.sd}{Standard deviation of bottom depths.}
##'
##' \item{maxdep}{Numeric vector with maximum depth.}
##'
##' \item{desc.tdist}{Numeric vector with descent total distance, estimated
##' from speed.}
##'
##' \item{desc.mean.speed}{Numeric vector with descent mean speed.}
##'
##' \item{desc.angle}{Numeric vector with descent angle, from the surface
##' plane.}
##'
##' \item{bott.tdist}{Numeric vector with bottom total distance, estimated
##' from speed.}
##'
##' \item{bott.mean.speed}{Numeric vector with bottom mean speed.}
##'
##' \item{asc.tdist}{Numeric vector with ascent total distance, estimated
##' from speed.}
##'
##' \item{asc.mean.speed}{Numeric vector with ascent mean speed.}
##'
##' \item{asc.angle}{Numeric vector with ascent angle, from the bottom plane.}
##'
##' \item{postdive.dur}{Postdive duration.}
##'
##' \item{postdive.tdist}{Numeric vector with postdive total distance,
##' estimated from speed.}
##'
##' \item{postdive.mean.speed}{Numeric vector with postdive mean speed.}
##'
##' If \code{depth.deriv=TRUE}, 21 additional columns with the minimum,
##' first quartile, median, mean, third quartile, maximum, and standard
##' deviation of the depth derivative for each phase of the dive.  The
##' number of columns also depends on argument \code{speed}.
##'
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{calibrateDepth}}, \code{\link{.detPhase}},
##'     \code{\link{TDRcalibrate-class}}
##' @keywords arith math
##' @examples
##' \donttest{## Too long for checks
##' ## Continuing the Example from '?calibrateDepth':
##' utils::example("calibrateDepth", package="diveMove",
##'                ask=FALSE, echo=FALSE, run.donttest=TRUE)
##' dcalib		# the 'TDRcalibrate' that was created
##'
##' tdrX <- diveStats(dcalib)
##' stamps <- stampDive(dcalib, ignoreZ=TRUE)
##' tdrX.tab <- data.frame(stamps, tdrX)
##' summary(tdrX.tab)
##'
##' }
"diveStats" <- function(x, depth.deriv=TRUE)
{
    if (!is(x, "TDRcalibrate")) stop("x must be a TDRcalibrate object")
    zvtdr <- getTDR(x)                     # fully calibrated object
    interval <- getDtime(zvtdr)            # sampling interval
    diveid <- getDAct(x, "dive.id")        # dive IDs
    postdiveid <- getDAct(x, "postdive.id")          # postdive IDs
    ok <- which(diveid > 0 & diveid %in% postdiveid) # diving subscripts
    dtimes <- getTime(zvtdr)[ok]                     # diving times
    ddepths <- getDepth(zvtdr)[ok]                   # diving depths
    dids <- diveid[ok]                               # dive IDs
    dphases <- getDPhaseLab(x)[ok]                   # dive phase labels
    okpd <- which(postdiveid %in% unique(dids)) # postdive subscripts
    pdtimes <- getTime(zvtdr)[okpd]          # required postdive times
    pddepths <- getDepth(zvtdr)[okpd]        # required postdive depths
    pdids <- postdiveid[okpd]                # required postdive IDs

    postdive.dur <- tapply(pdtimes, pdids, function(k) {
        difftime(k[length(k)], k[1], units="secs")
    })

    dtimestz <- attr(dtimes, "tzone")
    if (!is(zvtdr, "TDRspeed")) {
        td <- data.frame(dphases, dtimes, ddepths)
        perdive <- do.call(rbind, by(td, dids, oneDiveStats, interval=interval))
        res <- data.frame(perdive, postdive.dur)
        for (i in 1:3) res[, i] <- .POSIXct(res[, i], dtimestz)
    } else {
        dspeeds <- getSpeed(zvtdr)[ok]  # diving speeds
        td <- data.frame(dphases, dtimes, ddepths, dspeeds)
        perdive <- do.call(rbind, by(td, dids, oneDiveStats, interval=interval,
                                     speed=TRUE))
        ## for postdive total distance and mean speed
        ptd <- data.frame(pdtimes, getSpeed(zvtdr)[okpd])
        pdv <- do.call(rbind, by(ptd, pdids, .speedStats))
        res <- data.frame(perdive, postdive.dur, postdive.tdist=pdv[, 1],
                          postdive.mean.speed=pdv[, 2], row.names=NULL)
        for (i in 1:3) res[, i] <- .POSIXct(res[, i], dtimestz)
    }

    if (depth.deriv) {
        data.frame(res, .derivStats(x, diveNo=dids))
    } else res
}
