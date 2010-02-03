library(diveMove)

(sealX <- readTDR(system.file(file.path("data", "dives.csv"),
                              package="diveMove"),
                  concurrentCols=4:6, speed=TRUE,
                  sep=";", na.strings="", as.is=TRUE))
(dcalib <- calibrateDepth(sealX, dry.thr=3610, offset=3))
(dcalib <- calibrateDepth(sealX, offset=3, ascent.crit=0.5,
                          descent.crit=0.5, wiggle=0.75))

###_+ Check all calibrateDepth() procedure

###_ : Check phase detection
detp <- diveMove:::.detPhase(getTime(sealX), getDepth(sealX), dry.thr=70,
                             wet.thr=3610, getDtime(sealX))
###_ : Check zoc
zd <- zoc(getTime(sealX), getDepth(sealX), offset=3)
if (!is.null(zd)) sealX@depth <- zd
###_ : Check dive detection
detd <- diveMove:::.detDive(getDepth(sealX), detp[[2]], 4, getDtime(sealX))
###_ : Check labelling of dive phases
phaselabs <- diveMove:::.labDivePhase(sealX, detd[, 1], descent.crit.q=0.1,
                                      ascent.crit.q=0.5, wiggle.tol=0.85)

vcalib <- calibrateSpeed(dcalib, z=0, cex.pts=0.2)


###_ + Emacs local variables
## Local variables:
## allout-layout: (+ : 0)
## End:
