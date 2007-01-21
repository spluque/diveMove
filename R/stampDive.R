"stampDive" <- function(x, ignoreZ=TRUE)
{
    ## Value: A data frame; stamping each dive with trip number, trip
    ## type, and trip start and end time
    ## --------------------------------------------------------------------
    ## Arguments: x=TDRcalibrate object, ignoreZ=ignore Z phases?
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDRcalibrate")) stop ("x needs to be a TDRcalibrate object")
    act <- getGAct(x, "activity")
    diveid <- getDAct(x, "dive.id")

    if (ignoreZ) {
        tt <- getTime(getTDR(x))
        interval <- getDtime(getTDR(x))
        act[act == "Z"] <- "L"
        attlist <- diveMove:::.rleActivity(tt, act, interval) # recalculate
        phaseid <- as.numeric(attlist[[1]])  # what phase.id is now
    } else {
        attlist <- getGAct(x)
        phaseid <- getGAct(x, "phase.id")
    }

    beg <- rep(attlist[[3]], table(phaseid))
    end <- rep(attlist[[4]], table(phaseid))
    trip.no <- numeric(length(act))      # vector of 0s
    phaseid[act == "L"] <- 0             # phase.id on land should be 0
    ## make a sequence for phase.id > 0 from 1:number of such phases
    trip.no[act != "L"] <- rep(seq(along=table(phaseid[phaseid > 0])),
                table(phaseid[phaseid > 0]))
    ok <- match(unique(diveid[diveid > 0]), diveid) # required subscripts
    trip.no <-  trip.no[ok]
    trip.type <- act[ok]

    data.frame(trip.no, trip.type, beg=beg[ok], end=end[ok])
}
