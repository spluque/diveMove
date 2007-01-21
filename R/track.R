"track" <- function(txy, id=gl(1, nrow(txy)), subset)
{
    ## Value: A data frame with a per point-pair travel summary.
    ## --------------------------------------------------------------------
    ## Arguments: txy=data.frame whose first col represents POSIXct
    ## date/time, second and third col represent lon and lat,
    ## respectively, id=a factor dividing the data into sections,
    ## subset=logical expression indicating which rows should be analyzed
    ## (optional).
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (missing(subset)) {
        r <- TRUE
    } else {
        e <- substitute(subset)
        r <- eval(e, txy, parent.frame())
        if (!is.logical(r)) stop("'subset' must evaluate to logical")
        r <- r & !is.na(r)
    }
    locs <- txy[r, ]
    splitlocs <- split(locs, id[r])
    ## function to apply to each subset
    perid <- function(x) distSpeed(x[-nrow(x), ], x[-1, ])
    track <- lapply(splitlocs, perid)
    tracktab <- do.call("rbind", track)
    id <- rep(as.numeric(names(track)), sapply(track, nrow))
    data.frame(id=id, tracktab)
}
