
zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove", mustWork=TRUE)
sealX <- readTDR(zz, concurrentCols=4:6, sep=";",
                  na.strings="", as.is=TRUE)
dcalib <- calibrateDepth(sealX, zoc.method="offset", offset=3)
tdrstats <- diveStats(dcalib)
dim.ok <- c(323, 36)
expect_equal(dim(tdrstats), dim.ok)

stamps <- stampDive(dcalib)
expect_equal(dim(stamps), c(dim.ok[1], 4))
att <- timeBudget(dcalib, FALSE)
expect_equal(dim(att), c(7, 4))

sealX <- readTDR(zz, speed=TRUE, concurrentCols=4:6,
                  sep=";", na.strings="", as.is=TRUE)
dcalib <- calibrateDepth(sealX, zoc.method="offset", offset=3)
vcalib <- calibrateSpeed(dcalib, z=1, plot=FALSE)
tdrstats <- diveStats(vcalib)
expect_equal(dim(tdrstats), c(dim.ok[1], dim.ok[2] + 10))


###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
