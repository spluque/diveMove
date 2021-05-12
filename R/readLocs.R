
##' Read comma-delimited file with location data
##'
##' Read a delimited (*.csv) file with (at least) time, latitude, longitude
##' readings.
##'
##' The file must have a header row identifying each field, and all rows
##' must be complete (i.e. have the same number of fields). Field names
##' need not follow any convention.
##'
##' @param locations character: a string indicating the path to the file to
##'     read, or a \code{\link{data.frame}} available in the search
##'     list. Provide the entire path if the file is not on the current
##'     directory.  This can also be a text-mode connection, as allowed in
##'     \code{\link{read.csv}}.
##' @param loc.idCol integer: column number containing location ID.  If
##'     missing, a \code{loc.id} column is generated with sequential
##'     integers as long as the input.
##' @param idCol integer: column number containing an identifier for
##'     locations belonging to different groups.  If missing, an id column
##'     is generated with number one repeated as many times as the input.
##' @param dateCol integer: column number containing dates, and,
##'     optionally, times.
##' @param timeCol integer: column number containing times.
##' @param dtformat character: a string specifying the format in which the
##'     date and time columns, when pasted together, should be interpreted
##'     (see \code{\link{strptime}}) in \code{file}.
##' @param tz character: a string indicating the time zone for the date and
##'     time readings.
##' @param lonCol integer: column number containing longitude readings.
##' @param latCol integer: column number containing latitude readings.
##' @param classCol integer: column number containing the ARGOS rating for
##'     each location.
##' @param alt.lonCol integer: column number containing alternative
##'     longitude readings.
##' @param alt.latCol integer: Column number containing alternative
##'     latitude readings.
##' @param ... Passed to \code{\link{read.csv}}
##' @return A data frame.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @keywords manip
##' @examples
##' ## Do example to define object zz with location of dataset
##' utils::example("sealLocs", package="diveMove",
##'                ask=FALSE, echo=FALSE)
##' locs <- readLocs(zz, idCol=1, dateCol=2,
##'                  dtformat="%Y-%m-%d %H:%M:%S", classCol=3,
##'                  lonCol=4, latCol=5, sep=";")
##'
##' summary(locs)
"readLocs" <- function(locations, loc.idCol, idCol, dateCol, timeCol=NULL,
                       dtformat="%m/%d/%Y %H:%M:%S", tz="GMT", classCol,
                       lonCol, latCol, alt.lonCol=NULL, alt.latCol=NULL, ...)
{
    if (inherits(locations, "connection") ||
        (is.character(locations) && file.exists(locations))) {
        srcfile.name <- ifelse(inherits(locations, "connection"),
                               basename(summary(locations)$description),
                               basename(locations))
        inLocs <- read.csv(locations, ...)
    } else {
        if (! is.data.frame(locations)) {
            stop ("'locations' must be a data.frame, path to a file, or a connection")
        } else {inLocs <- locations}
    }
    if (missing(loc.idCol)) {
        loc.id <- seq(nrow(inLocs))
    } else loc.id <- inLocs[, loc.idCol]
    if (missing(idCol)) {
        id <- rep(1, nrow(inLocs))
    } else id <- inLocs[, idCol]
    dtpasted <- paste(inLocs[, dateCol], inLocs[, timeCol])
    datetime <- as.POSIXct(strptime(dtpasted, format=dtformat), tz=tz)
    locs <- data.frame(loc.id=loc.id, id=id, time=datetime,
                       lon=inLocs[, lonCol], lat=inLocs[, latCol],
                       class=inLocs[, classCol])
    if (!is.null(alt.lonCol)) locs$alt.lon <- inLocs[, alt.lonCol]
    if (!is.null(alt.latCol)) locs$alt.lat <- inLocs[, alt.latCol]
    comment(locs) <- ifelse(exists("srcfile.name"), srcfile.name,
                            paste(deparse(match.call()), collapse=""))

    locs[order(locs[, 2], locs[, 3]), ] # sort by seal id and time
}

## TEST ZONE --------------------------------------------------------------
