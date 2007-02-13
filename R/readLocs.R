## $Id: readLocs.R,v 1.2 2007-02-13 17:22:53 sluque Exp $

"readLocs" <- function(file, loc.idCol, idCol, dateCol, timeCol=NULL,
                       dtformat="%m/%d/%Y %H:%M:%S", tz="GMT", classCol,
                       lonCol, latCol, alt.lonCol=NULL, alt.latCol=NULL, ...)
{
    ## Value: A data frame with ARGOS locations.
    ## --------------------------------------------------------------------
    ## Arguments: file=quoted file name, including path, of file to read,
    ## loc.idCol=column number containing the location id, idCol=column
    ## number identifying locations belonging to different groups,
    ## dateCol=column number containing dates and, optionally, times,
    ## timeCol=optional column number containing times, latCol and
    ## lonCol=latitude and longitude column numbers, respectively,
    ## alt.latCol and alt.lonCol=alternative latitude and longitude
    ## columns, respectively, classCol=ARGOS classification; ...= passed
    ## to read.csv()
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    srcfile <- basename(file)
    inLocs <- read.csv(file, ...)
    if (missing(loc.idCol)) {
        loc.id <- seq(nrow(inLocs))
    } else loc.id <- inLocs[, loc.idCol]
    if (missing(idCol)) {
        id <- rep(1, nrow(inLocs))
    } else id <- inLocs[, idCol]
    dtpasted <- paste(inLocs[, dateCol], inLocs[, timeCol])
    datetime <- as.POSIXct(strptime(dtpasted, format=dtformat), tz=tz)
    ## Set up data frame with loc id, animal id, time, year, doy, period,
    ## pttid, class, newclass, lat, lon, latalt, lonalt
    locs <- data.frame(loc.id=loc.id, id=id, time=datetime,
                       lon=inLocs[, lonCol], lat=inLocs[, latCol],
                       class=inLocs[, classCol])
    if (!is.null(alt.lonCol)) locs$alt.lon <- inLocs[, alt.lonCol]
    if (!is.null(alt.latCol)) locs$alt.lat <- inLocs[, alt.latCol]
    comment(locs) <- srcfile

    locs[order(locs[, 2], locs[, 3]), ] # sort by seal id and time
}

## TEST ZONE --------------------------------------------------------------
