###_+ Check calibrateDepth() overall

## Load data to compare against
data(divesTDR)

zz <- system.file(file.path("data", "dives.csv"),
                  package="diveMove", mustWork=TRUE)
sealX <- readTDR(zz, concurrentCols=4:6, speed=TRUE,
                 sep=";", na.strings="", as.is=TRUE)

## Compare against TDR object provided as data
expect_equivalent(divesTDR, sealX)

dcalib <- calibrateDepth(sealX, dry.thr=3610, zoc.method="offset", offset=3)
ndives <- max(getDAct(dcalib, "dive.id"))
dive.stats <- diveStats(dcalib)
maxdep <- max(dive.stats$maxdep)

## Check that we get expected number of dives and maximum depth
expect_true(ndives == 323)
expect_true(maxdep == 88)

###_+ Check through calibrateDepth() procedure

###_ : Check phase detection
nphases <- 7
detp <- diveMove:::.detPhase(getTime(sealX), getDepth(sealX), dry.thr=70,
                             wet.thr=3610, interval=getDtime(sealX))

expect_equal(max(detp$phase.id), nphases)
expect_equal(length(detp$beg), nphases)
expect_equal(length(detp$end), nphases)

###_ : Check zoc
zd <- diveMove:::.zoc(getTime(sealX), getDepth(sealX),
                      method="offset", control=list(offset=3))
expect_equal(min(zd, na.rm=TRUE), 0)
expect_equal(max(zd, na.rm=TRUE), 88)

###_ : Check dive detection
detd <- diveMove:::.detDive(getDepth(sealX), detp[[2]], 4)
ndives <- 816
nL <- 9530
nW <- 137
nU <- 20964
nD <- 3568
nZ <- 0
dive.act <- as.integer(c(nL, nW, nU, nD, nZ))
names(dive.act) <- c("L", "W", "U", "D", "Z")
expect_equal(max(detd$dive.id), ndives)
expect_equal(max(detd$postdive.id), ndives)
detd.dact.summ <- summary(detd$dive.activity)
expect_identical(detd.dact.summ[order(factor(names(detd.dact.summ)))],
                 dive.act[order(factor(names(dive.act)))])

###_ : Check labelling of dive phases
phaselabs <- diveMove:::.labDivePhase(sealX, detd[, 1], smooth.par=0.1,
                                      dive.model="smooth.spline",
                                      knot.factor=3, descent.crit.q=0,
                                      ascent.crit.q=0.1)
nDA <- 308
nD <- 722
nDB <- 506
nB <- 1063
nBA <- 508
nA <- 461
nX <- 30631
nphase.labs <- as.integer(c(nDA, nD, nDB, nB, nBA, nA, nX))
names(nphase.labs) <- c("DA", "D", "DB", "B", "BA", "A", "X")
phaselabs.summ <- summary(phaselabs$phase.labels)
expect_identical(phaselabs.summ[order(factor(names(phaselabs.summ)))],
                 nphase.labs[order(factor(names(nphase.labs)))])

## phaselabs <- diveMove:::.labDivePhase(sealX, detd[, 1], smooth.par=0.1,
##                                       dive.model="unimodal",
##                                       knot.factor=3, descent.crit.q=0,
##                                       ascent.crit.q=0.1)
## nDA <- 312
## ## nD <- 971
## nD <- 967  # R-devel
## ## nDB <- 503
## nDB <- 504  # R-devel
## nBA <- 504
## nB <- 579
## ## nA <- 699
## nA <- 702  # R-devel
## nX <- 30631
## nphase.labs <- as.integer(c(nDA, nD, nDB, nBA, nB, nA, nX))
## names(nphase.labs) <- c("DA", "D", "DB", "BA", "B", "A", "X")
## phaselabs.summ <- summary(phaselabs$phase.labels)
## expect_identical(phaselabs.summ[order(factor(names(phaselabs.summ)))],
##                  nphase.labs[order(factor(names(nphase.labs)))])

###_ : Test speed calibration effect

vcalib <- calibrateSpeed(dcalib, z=0, plot=FALSE)
expect_false(identical(getSpeed(getTDR(vcalib)),
                       getCCData(getTDR(dcalib), "speed")[[1]]))



###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
