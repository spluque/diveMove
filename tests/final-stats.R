library(diveMove)

zz <- gzfile(system.file(file.path("data", "dives.csv.gz"),
                         package="diveMove"), open="r")
(sealX <- readTDR(zz, concurrentCols=4:6, sep=";",
                  na.strings="", as.is=TRUE))
(dcalib <- calibrateDepth(sealX, zoc.method="offset", offset=3))
tdrstats <- diveStats(dcalib)
head(tdrstats)
head(stamps <- stampDive(dcalib))
(att <- timeBudget(dcalib, FALSE))
(att <- timeBudget(dcalib, TRUE))

zz <- gzfile(system.file(file.path("data", "dives.csv.gz"),
                         package="diveMove"), open="r")
(sealX <- readTDR(zz, speed=TRUE, concurrentCols=4:6,
                  sep=";", na.strings="", as.is=TRUE))
(dcalib <- calibrateDepth(sealX, zoc.method="offset", offset=3))
(vcalib <- calibrateSpeed(dcalib, z=1))
tdrstats <- diveStats(vcalib)
head(tdrstats)
head(stamps <- stampDive(vcalib))
(att <- timeBudget(vcalib, FALSE))
(att <- timeBudget(vcalib, TRUE))


###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
