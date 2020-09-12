###_ + Reading and as.data.frame ------------------------------------------

zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove", mustWork=TRUE)
sealX <- readTDR(zz, speed=TRUE, sep=";", na.strings="", as.is=TRUE)

expect_true(is(sealX, "TDR"))
sealX <- readTDR(zz, sep=";", na.strings="", as.is=TRUE)
expect_true(!is(sealX, "TDRspeed")) # speed=FALSE default

sealDat <- as.data.frame(sealX)
stopifnot(is.data.frame(sealDat))
sealX <- readTDR(zz, concurrentCols=NULL, sep=";",
                 na.strings="", as.is=TRUE)
## concurrentData should be a 0-column data.frame
expect_true(ncol(getCCData(sealX)) == 0)

sealX <- readTDR(zz, subsamp=10, speed=TRUE, concurrentCols=6,
                 sep=";", na.strings="", as.is=TRUE)
expect_true(is(sealX, "TDR"))
expect_true(is(sealX, "TDRspeed"))
expect_true(getDtime(sealX) == 10)

sealX <- readTDR(zz, subsamp=10, speed=TRUE, concurrentCols=5:6,
                 sep=";", na.strings="", as.is=TRUE)
expect_true(ncol(getCCData(sealX)) == 2)

sealDat <- as.data.frame(sealX)
sealX <- createTDR(time=sealDat$time, depth=sealDat$depth,
                   concurrentData=sealDat[, 3:ncol(sealDat)],
                   dtime=sealX@dtime, file=sealX@file)
sealX.speed <- createTDR(time=sealDat$time, depth=sealDat$depth,
                         concurrentData=sealDat[, 3:ncol(sealDat)],
                         speed=TRUE, dtime=sealX@dtime, file=sealX@file)
expect_true(is(sealX, "TDR"))
expect_true(is(sealX.speed, "TDRspeed"))

###_ + Accessors ----------------------------------------------------------

tt <- getTime(sealX)
dd <- getDepth(sealX)
ss <- getSpeed(sealX.speed)
cc <- getCCData(sealX)
expect_equal(length(dd), length(tt))
expect_equal(length(ss), length(tt))
expect_equal(nrow(cc), length(tt))
cc <- getCCData(sealX, "speed")
expect_equal(ncol(cc), 1)

expect_equal(getFileName(sealX), "dives.csv")
expect_equal(getDtime(sealX), 10)

###_ + Replacements -------------------------------------------------------

sll <- length(getSpeed(sealX.speed))
speed.new <- rnorm(sll)
speed(sealX.speed) <- speed.new
expect_identical(speed.new, getSpeed(sealX.speed))

depth.new <- rnorm(getDepth(sealX))
depth(sealX) <- depth.new
expect_identical(depth.new, getDepth(sealX))

sealX <- createTDR(time=sealDat$time, depth=sealDat$depth,
                   concurrentData=sealDat[, 3:ncol(sealDat)],
                   dtime=sealX@dtime, file=sealX@file)
depth.new <- rnorm(length(getDepth(sealX)))
depth(sealX) <- depth.new
expect_identical(depth.new, getDepth(sealX))


###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
