## $Id: detPhase.R,v 1.2 2007-02-13 17:22:53 sluque Exp $

".rleActivity" <- function(time, act, interval)
{
    ## Value: list with factor breaking activity phases, duration of each,
    ## and beginning and end times of each
    ## --------------------------------------------------------------------
    ## Arguments: time=POSIXct date and time; act=factor representing
    ## activity for each row interval=sampling interval (s) in POSIXct
    ## units
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    runs <- rle(as.vector(act))
    cuttimbr <- factor(rep(seq(along=runs$lengths), runs$lengths))
    timsplit <- split(time, cuttimbr)
    begtim <- structure(sapply(timsplit, "[", 1), class=c("POSIXt", "POSIXct"),
                        tzone="GMT", names=NULL)
    endtim <- structure(sapply(timsplit, function(x) x[length(x)]),
                        class=c("POSIXt", "POSIXct"), tzone="GMT", names=NULL)
    duration <- difftime(endtim, begtim, units="secs") + interval
    list(time.br=cuttimbr,
         time.peract=duration,
         beg.time=begtim,
         end.time=endtim)
}

".detPhase" <- function(time, depth, dry.thr, wet.thr, ...)
{
    ## Value: list with index of per-row activities, the activity code,
    ## and start and end of each activity phase
    ## --------------------------------------------------------------------
    ## Arguments: time=chron vector with date/time depth=numeric vector
    ## with depth readings (m) ...=sampling interval in POSIXct units (s),
    ## to pass to rleActivity dry.thr=duration (in s) of on-land readings
    ## that should be at-sea aquaerr=duration (in s) of at-sea readings to
    ## be taken as leisure
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## Factor with default "land" values to code activity levels: L=land,
    ## W=wet (at-sea), U=underwater (0 - dive threshold), D=diving, Z=wet
    ## (leisure)
    act <- factor(rep("L", length(time)), levels=c("L", "W", "U", "D", "Z"))
    ## 10's when animal is wet; i.e. when depth is being recorded
    act[!is.na(depth)] <- "W"
    ## First run calculates times in each activity phase from the raw data
    rawacts <- diveMove:::.rleActivity(time, act, ...)
    ## On-land activity < 'dry.thr' should be considered still at-sea
    land <- levels(rawacts[[1]])[rawacts[[2]] < dry.thr]
    act[rawacts[[1]] %in% land & act == "L"] <- "W"
    ## Second run; at-sea phases < wet.thr should be leisure
    leiacts <- diveMove:::.rleActivity(time, act, ...)
    leisure <- levels(leiacts[[1]])[leiacts[[2]] < wet.thr]
    act[leiacts[[1]] %in% leisure & act == "W"] <- "Z"
    ## Final run to determine times with all corrected activities
    finacts <- diveMove:::.rleActivity(time, act, ...)
    nphase <- length(levels(finacts[[1]]))
    if(act[1] == "L" & act[length(act)] == "L") {
        message("Record is complete\n", nphase, " phases detected")
    } else {
        if(act[1] != "L" & act[length(act)] != "L") {
            message("Record is truncated at the beginning and at the end\n",
                    nphase, " phases detected")
        } else {
            if(act[1] != "L") {
                message("Record is truncated at the beginning\n", nphase,
                        " phases detected")
            } else {
                message("Record is truncated at the end\n", nphase,
                        " phases detected")
            }
        }
    }
    indphases <- as.numeric(finacts[[1]])
    names(finacts[[3]]) <- seq(length(finacts[[3]]))
    names(finacts[[4]]) <- seq(length(finacts[[4]]))
    list(phase.id=indphases,            # index of per-row activities
         activity=act,                  # activities themselves
         begin=finacts[[3]],            # start of activity phase
         end=finacts[[4]])              # end of activity phase
}
