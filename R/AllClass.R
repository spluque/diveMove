##' Classes "TDR" and "TDRspeed" for representing TDR information
##'
##' These classes store information gathered by time-depth recorders.
##'
##' Since the data to store in objects of these clases usually come from a
##' file, the easiest way to construct such objects is with the function
##' \code{\link{readTDR}} to retrieve all the necessary information.
##'
##' @aliases TDR
##' @slot file Object of class \sQuote{character}, string indicating the
##'     file where the data comes from.
##' @slot dtime Object of class \sQuote{numeric}, sampling interval in
##'     seconds.
##' @slot time Object of class \code{\link{POSIXct}}, time stamp for every
##'     reading.
##' @slot depth Object of class \sQuote{numeric}, depth (m) readings.
##' @slot concurrentData Object of class \code{\link{data.frame}}, optional
##'     data collected concurrently.
##' @section Objects from the class:
##' Objects can be created by calls of the form \code{new("TDR", \dots)}
##' and \code{new("TDRspeed", \dots)}.
##'
##' \sQuote{TDR} objects contain concurrent time and depth readings, as
##' well as a string indicating the file the data originates from, and a
##' number indicating the sampling interval for these data.
##' \sQuote{TDRspeed} extends \sQuote{TDR} objects containing additional
##' concurrent speed readings.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{readTDR}}, \code{\link{TDRcalibrate}}.
##' @keywords classes
setClass("TDR",
         slots=c(file="character", dtime="numeric", time="POSIXct",
                 depth="numeric", concurrentData="data.frame"),
         prototype=prototype(concurrentData=data.frame()),
         validity=function(object) {
             if (length(object@time) != length(object@depth)) {
                 return("depth and time must have equal lengths")
             }
             time.diffs <- diff(unclass(object@time))
             if (any(time.diffs < 0)) {
                 return("time stamps must be in increasing order")
             }
             if (any(time.diffs == 0)) {
                 return("time stamps must not contain duplicate values")
             }
             ccDataN <- nrow(object@concurrentData)
             if (ccDataN > 0 && ccDataN != length(object@time)) {
                 mes <- paste("concurrentData must have the same number of rows",
                              "as there are time stamps")
                 return(mes)
             }
             if (!slot(object, "dtime")) return("dtime cannot be missing")
             return(TRUE)
         })

.speedNames <- c("velocity", "speed")

##' @describeIn TDR Class \code{TDRspeed}
##' @aliases TDRspeed
setClass("TDRspeed", contains="TDR",
         validity=function(object) {
             ccData <- object@concurrentData
             ccDataNames <- names(ccData)
             speedCol <- ccDataNames %in% .speedNames
             if (length(ccDataNames[speedCol]) != 1) {
                 return("speed is not available in concurrentData slot")
             } else if (!is.numeric(ccData[, speedCol])) {
                 return("speed must be of class numeric")
             }
             return(TRUE)
         })

##' Class "TDRcalibrate" for dive analysis
##'
##' This class holds information produced at various stages of dive
##' analysis.  Methods are provided for extracting data from each slot.
##'
##' This is perhaps the most important class in diveMove, as it holds all
##' the information necessary for calculating requested summaries for a
##' TDR.
##'
##' @aliases TDRcalibrate
##' @slot call Object of class \code{\link{call}}.  The matched call to the
##'     function that created the object.
##' @slot tdr Object of class \code{\link{TDR}}.  This slot contains the
##'     time, zero-offset corrected depth, and possibly a data frame.  If
##'     the object is also of class "TDRspeed", then the data frame might
##'     contain calibrated or uncalibrated speed.  See
##'     \code{\link{readTDR}} and the accessor function
##'     \code{\link{getTDR}} for this slot.
##' @slot gross.activity Object of class \sQuote{list}. This slot holds a
##'     list of the form returned by \code{\link{.detPhase}}, composed of 4
##'     elements.  It contains a vector (named \code{phase.id}) numbering
##'     each major activity phase found in the record, a factor (named
##'     \code{activity}) labelling each row as being dry, wet, or trivial
##'     wet activity.  These two elements are as long as there are rows in
##'     \code{tdr}.  This list also contains two more vectors, named
##'     \code{begin} and \code{end}: one with the beginning time of each
##'     phase, and another with the ending time; both represented as
##'     \code{\link{POSIXct}} objects. See \code{\link{.detPhase}}.
##' @slot dive.activity Object of class \code{\link{data.frame}}.  This
##'     slot contains a \code{\link{data.frame}} of the form returned by
##'     \code{\link{.detDive}}, with as many rows as those in \code{tdr},
##'     consisting of three vectors named: \code{dive.id}, which is an
##'     integer vector, sequentially numbering each dive (rows that are not
##'     part of a dive are labelled 0), dive.activity is a factor which
##'     completes that in \code{activity} above, further identifying rows
##'     in the record belonging to a dive.  The third vector in
##'     \code{dive.activity} is an integer vector sequentially numbering
##'     each postdive interval (all rows that belong to a dive are labelled
##'     0).  See \code{\link{.detDive}}, and \code{\link{getDAct}} to
##'     access all or any one of these vectors.
##' @slot dive.phases Object of class \sQuote{factor}.  This slot is a
##'     factor that labels each row in the record as belonging to a
##'     particular phase of a dive.  It has the same form as the
##'     \dQuote{phase.labels} component of the list returned by
##'     \code{\link{.labDivePhase}}.
##' @slot dive.models Object of class \sQuote{list}.  This slot contains
##'     the details of the process of dive phase identification for each
##'     dive.  It has the same form as the \code{dive.models} component of
##'     the list returned by \code{\link{.labDivePhase}}.  It has as many
##'     components as there are dives in the \code{\link{TDR}} object, each
##'     of them of class \code{\link{diveModel}}.
##' @slot dry.thr Object of class \sQuote{numeric}.  The temporal criteria
##'     used for detecting dry periods that should be considered as wet.
##' @slot wet.thr Object of class \sQuote{numeric} the temporal criteria
##'     used for detecting periods wet that should not be considered as
##'     foraging time.
##' @slot dive.thr Object of class \sQuote{numeric}.  The temporal criteria
##'     used for detecting periods wet that should not be considered as
##'     foraging time.
##' @slot speed.calib.coefs Object of class \sQuote{numeric}.  The
##'     intercept and slope derived from the speed calibration procedure.
##'     Defaults to c(0, 1) meaning uncalibrated speeds.
##' @section Objects from the Class:
##'
##' Objects can be created by calls of the form \code{new("TDRcalibrate",
##' \dots{})}.  The objects of this class contain information necessary to
##' divide the record into sections (e.g.  dry/water), dive/surface, and
##' different sections within dives.  They also contain the parameters used
##' to calibrate speed and criteria to divide the record into phases.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{TDR}} for links to other classes in the package.
##'     \code{\link{TDRcalibrate-methods}} for the various methods
##'     available.
##' @keywords classes
setClass("TDRcalibrate",
         slots=c(call="call", tdr="TDR", gross.activity="list",
                 dive.activity="data.frame", dive.phases="factor",
                 dive.models="list", dry.thr="numeric", wet.thr="numeric",
                 dive.thr="numeric", speed.calib.coefs="numeric"),
         prototype=prototype(speed.calib.coefs=c(0, 1)),
         validity=function(object) {
             ndives <- max(object@dive.activity$dive.id, na.rm=TRUE)
             dml <- slot(object, "dive.models")
             if (length(slot(object, "dry.thr")) > 1) {
                 return("dry.thr must be a single number")
             }
             if (length(slot(object, "wet.thr")) > 1) {
                 return("wet.thr must be a single number")
             }
             if (length(slot(object, "dive.thr")) > 1) {
                 return("dive.thr must be a single number")
             }
             if (length(slot(object, "speed.calib.coefs")) != 2) {
                 return("speed.calib.coefs must be a length-2 vector")
             }
             if (length(dml) != ndives) {
                 return("All dives must have a corresponding dive model")
             }
             if (! all(sapply(dml, is, "diveModel"))) {
                 return("All elements of dive.models must be class diveModel")
             }
             return(TRUE)
         })

setOldClass("smooth.spline")
setOldClass("bSpline")
setClassUnion("dive.spline", c("smooth.spline", "bSpline"))
setOldClass("xyVector")

##' Class "diveModel" for representing a model for identifying dive phases
##'
##' Details of model used to identify the different phases of a dive.
##' @aliases diveModel
##' @slot label.matrix Object of class \code{"matrix"}.  A 2-column
##'     character matrix with row numbers matching each observation to the
##'     full \code{\link{TDR}} object, and a vector labelling the phases of
##'     each dive.
##' @slot model Object of class \code{"character"}.  A string identifying
##'     the specific model fit to dives for the purpose of dive phase
##'     identification.  It should be one of \sQuote{smooth.spline} or
##'     \sQuote{unimodal}.
##' @slot dive.spline Object of class \code{"smooth.spline"}. Details of
##'     cubic smoothing spline fit (see
##'     \code{\link[stats]{smooth.spline}}).
##' @slot spline.deriv Object of class \code{"list"}.  A list with the
##'     first derivative of the smoothing spline (see
##'     \code{\link[stats]{predict.smooth.spline}}).
##' @slot descent.crit Object of class \code{"numeric"}.  The index of the
##'     observation at which the descent was deemed to have ended (from
##'     initial surface observation).
##' @slot ascent.crit Object of class \code{"numeric"}.  the index of the
##'     observation at which the ascent was deemed to have ended (from
##'     initial surface observation).
##' @slot descent.crit.rate Object of class \code{"numeric"}. The rate of
##'     descent corresponding to the critical quantile used.
##' @slot ascent.crit.rate Object of class \code{"numeric"}. The rate of
##'     ascent corresponding to the critical quantile used.
##' @section Objects from the Class:
##' Objects can be created by calls of the form \code{new("diveModel",
##'     ...)}.
##'
##' \sQuote{diveModel} objects contain all relevant details of the process to
##' identify phases of a dive.  Objects of this class are typically generated
##' during depth calibration, using \code{\link{calibrateDepth}}, more
##' specifically \code{\link{.cutDive}}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{getDiveDeriv}}, \code{\link{plotDiveModel}}
##' @keywords classes
##' @examples
##' showClass("diveModel")
##'
##' \donttest{## Too long for checks
##' ## Continuing the Example from '?calibrateDepth':
##' utils::example("calibrateDepth", package="diveMove",
##'                ask=FALSE, echo=FALSE, run.donttest=TRUE)
##' dcalib		# the 'TDRcalibrate' that was created
##'
##' ## Compare dive models for dive phase detection
##' diveNo <- 255
##' diveX <- as.data.frame(extractDive(dcalib, diveNo=diveNo))
##' diveX.m <- cbind(as.numeric(row.names(diveX[-c(1, nrow(diveX)), ])),
##'                  diveX$depth[-c(1, nrow(diveX))],
##'                  diveX$time[-c(1, nrow(diveX))])
##'
##' ## calibrateDepth() default unimodal regression. Number of inner knots is
##' ## either 10 or the number of samples in the dive, whichever is larger.
##' (phases.uni <- diveMove:::.cutDive(diveX.m, smooth.par=0.2, knot.factor=20,
##'                                    dive.model="unimodal",
##'                                    descent.crit.q=0.01, ascent.crit.q=0))
##' ## Smoothing spline model, using default smoothing parameter.
##' (phases.spl <- diveMove:::.cutDive(diveX.m, smooth.par=0.2, knot.factor=20,
##'                                    dive.model="smooth.spline",
##'                                    descent.crit.q=0.01, ascent.crit.q=0))
##' plotDiveModel(phases.spl,
##'               diveNo=paste(diveNo, ", smooth.par=", 0.2, sep=""))
##' plotDiveModel(phases.uni, diveNo=paste(diveNo))
##'
##' }
setClass("diveModel",
         slots=c(label.matrix="matrix", model="character",
                 dive.spline="dive.spline", spline.deriv="xyVector",
                 descent.crit="numeric", ascent.crit="numeric",
                 descent.crit.rate="numeric", ascent.crit.rate="numeric"),
         validity=function(object) {
             if (! object@model %in% c("smooth.spline", "unimodal")) {
                 return("Model must be 'smooth.spline' or 'unimodal'")
             }
             if (length(slot(object, "descent.crit")) > 1) {
                 return("descent.crit must be a single number")
             }
             if (length(slot(object, "ascent.crit")) > 1) {
                 return("ascent.crit must be a single number")
             }
             if (length(slot(object, "descent.crit.rate")) > 1) {
                 return("descent.crit.rate must be a single number")
             }
             if (length(slot(object, "ascent.crit.rate")) > 1) {
                 return("ascent.crit.rate must be a single number")
             }
             return(TRUE)
         })

setOldClass("nls")                      # For bout methods

##' Class "Bouts" for representing Poisson mixtures for identification of
##' behavioural bouts
##'
##' Base class for storing key information for modelling and detecting
##' bouts in behavioural data.
##' @aliases Bouts
##' @slot x Object of class \code{"numeric"}.  Data to be modelled.
##' @slot method Object of class \code{"character"}.  A string indicating
##'     the type of frequency to calculate from \code{x}: "standard" or
##'     "seq.diff".  If "standard", frequencies are calculated directly
##'     from \code{x}, and from the sequential differences in \code{x}
##'     otherwise.
##' @slot lnfreq Object of class \code{\link{data.frame}}.  Columns named
##'     \var{lnfreq} (log frequencies) and \var{x} (mid points of histogram
##'     bins).
##' @section Objects from the class:
##' Objects can be created most conveniently via the
##' \code{\link{boutfreqs}} function, which sets the \code{lnfreq} slot,
##' but can also be created via \code{new("Bouts")}.
##' @author Sebastian P. Luque \email{spluque@@gmail.com}
##' @seealso \code{\link{boutfreqs}}
##' @keywords classes
setClass("Bouts",
         slots=c(x="numeric", method="character", lnfreq="data.frame"),
         prototype=prototype(method="standard", lnfreq=data.frame()),
         validity=function(object) {
             meths <- c("standard", "seq.diff")
             meths.msg = paste(meths, collapse=", ")
             if (! object@method %in% meths)
                 return(paste("method must be one of:", meths.msg))
             return(TRUE)
         })
