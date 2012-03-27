library(diveMove)

###_ + Reading and as.data.frame ------------------------------------------
zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove")
sealX <- readTDR(zz, speed=TRUE, sep=";", na.strings="", as.is=TRUE)
is(sealX, "TDR")
is(sealX, "TDRspeed")
zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove")
sealX <- readTDR(zz, sep=";", na.strings="", as.is=TRUE)
is(sealX, "TDR")
is(sealX, "TDRspeed")

sealDat <- as.data.frame(sealX)
zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove")
sealX <- readTDR(zz, concurrentCols=NULL, sep=";",
                 na.strings="", as.is=TRUE)
is(sealX, "TDR")
is(sealX, "TDRspeed")
zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove")
sealX <- readTDR(zz, subsamp=10, speed=TRUE, concurrentCols=6,
                 sep=";", na.strings="", as.is=TRUE)
is(sealX, "TDR")
is(sealX, "TDRspeed")
zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove")
sealX <- readTDR(zz, subsamp=10, speed=TRUE, concurrentCols=5:6,
                 sep=";", na.strings="", as.is=TRUE)
is(sealX, "TDR")
is(sealX, "TDRspeed")

sealDat <- as.data.frame(sealX)
sealX <- createTDR(time=sealDat$time, depth=sealDat$depth,
                   concurrentData=sealDat[, 3:ncol(sealDat)],
                   dtime=sealX@dtime, file=sealX@file)
sealX <- createTDR(time=sealDat$time, depth=sealDat$depth,
                   concurrentData=sealDat[, 3:ncol(sealDat)], speed=TRUE,
                   dtime=sealX@dtime, file=sealX@file)

###_ + Accessors ----------------------------------------------------------
head(tt <- getTime(sealX))
head(dd <- getDepth(sealX))
head(ss <- getSpeed(sealX))
head(cc <- getCCData(sealX))
head(cc <- getCCData(sealX, "speed"))
getFileName(sealX)
getDtime(sealX)

###_ + Replacements -------------------------------------------------------
sll <- length(getSpeed(sealX))
speed(sealX) <- rnorm(sll)
head(getSpeed(sealX))
depth(sealX) <- rnorm(getDepth(sealX))
sealX <- createTDR(time=sealDat$time, depth=sealDat$depth,
                   concurrentData=sealDat[, 3:ncol(sealDat)],
                   dtime=sealX@dtime, file=sealX@file)
depth(sealX) <- rnorm(length(getDepth(sealX)))
head(getDepth(sealX))


###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
