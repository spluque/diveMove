## $Id: zoc.R,v 1.2 2007-02-13 17:22:53 sluque Exp $

"zoc" <- function(time, depth, offset)
{
    ## Value: Vector with all depths with corrected values for time
    ## windows selected with plotTDR(). Make depths < 0 equal 0
    ## --------------------------------------------------------------------
    ## Arguments: time=POSIXct, depth=uncalibrated depth, offset=offset to
    ## use if already known
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (missing(offset)) {
        zoclims <- plotTD(time, depth)
        dev.off()
        if (length(zoclims) == 0) {
            message("No ZOC performed.")
            depth[depth < 0 & !is.na(depth)] <- 0
            depth
        } else {
            ## Subtract the ZOC from depths in each window
            ## If there's any overlap, the latest window prevails
            zocdives <- lapply(zoclims, function(x) {
                newdep <- depth[time >= x[[1]][1] & time < x[[1]][2]] - x[[2]][1]
                indsnewdep <- which(time >= x[[1]][1] & time < x[[1]][2])
                cbind(indsnewdep, newdep)
            })
            message(paste("ZOC procedure completed\n", length(zocdives),
                          " time windows have been corrected", sep=""))
            zocdives <- do.call("rbind", zocdives)
            ## Correct depths within chosen time windows
            depth[zocdives[, 1]] <- zocdives[, 2]
            ## Turn all negative depths into zeroes
            depth[depth < 0 & !is.na(depth)] <- 0
            depth
        }
    } else {
        depth <- depth - offset
        depth[depth < 0 & !is.na(depth)] <- 0
        depth
    }
}
