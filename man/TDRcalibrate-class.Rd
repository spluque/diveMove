% $Id: TDRcalibrate-class.Rd,v 1.5 2007-12-08 21:50:54 sluque Exp $
\name{TDRcalibrate-class}
\docType{class}

\alias{TDRcalibrate-class}
\alias{TDRcalibrate}

% -------------------------------------------------------------------------

\title{Class "TDRcalibrate" for dive analysis}



\description{This class holds information produced at various stages of
  dive analysis.  Methods are provided for extracting data from each
  slot.}


\section{Objects from the Class}{ Objects can be created by calls of the
  form \code{new("TDRcalibrate", \dots)}.  The objects of this class
  contain information necessary to divide the record into sections
  (e.g. dry/water), dive/surface, and different sections within dives.
  They also contain the parameters used to calibrate speed and criteria
  to divide the record into phases.}


\section{Slots}{
  \describe{

    \item{\code{tdr}:}{Object of class \dQuote{TDR}.

      This slot contains the time, zero-offset corrected depth, and
      possibly a data frame.  If the object is also of class "TDRspeed",
      then the data frame might contain calibrated or uncalibrated
      speed.  See \code{\link{readTDR}} and the accessor function
      \code{\link{getTDR}} for this slot.}

    \item{\code{gross.activity}:}{Object of class \dQuote{list}.

      This slot holds a list of the form returned by
      \code{\link{.detPhase}}, composed of 4 elements.  It contains a
      vector (named \code{phase.id}) numbering each major activity phase
      found in the record, a factor (named \code{activity}) labelling
      each row as being dry, wet, or trivial wet activity.  These two
      elements are as long as there are rows in \code{tdr}.  This list
      also contains two more vectors, named \code{begin} and \code{end}:
      one with the beginning time of each phase, and another with the
      ending time; both represented as \code{POSIXct} objects.  See
      \code{\link{.detPhase}}.}

    \item{\code{dive.activity}:}{Object of class \dQuote{data.frame}.

      This slot contains a data.frame of the form returned by
      \code{\link{.detDive}}, with as many rows as those in \code{tdr},
      consisting of three vectors named: \code{dive.id}, which is an
      integer vector, sequentially numbering each dive (rows that are
      not part of a dive are labelled 0), dive.activity is a factor
      which completes that in \code{activity} above, further identifying
      rows in the record belonging to a dive.  The third vector in
      \code{dive.activity} is an integer vector sequentially numbering
      each postdive interval (all rows that belong to a dive are
      labelled 0).  See \code{\link{.detDive}}, and
      \code{\link{getDAct}} to access all or any one of these vectors.}

    \item{\code{dive.phases}:}{Object of class \dQuote{factor}. must be
      the same as value returned by \code{\link{.labDivePhase}}.

      This slot is a factor that labels each row in the record as
      belonging to a particular phase of a dive.  It has the same form
      as objects returned by \code{\link{.labDivePhase}}.}

    \item{\code{dry.thr}:}{Object of class \dQuote{numeric} the temporal
      criteria used for detecting dry periods that should be considered
      as wet.}

    \item{\code{wet.thr}:}{Object of class \dQuote{numeric} the temporal
      criteria used for detecting periods wet that should not be
      considered as foraging time.}

    \item{\code{dive.thr}:}{Object of class \dQuote{numeric} the
      criteria used for defining a dive.}

    \item{\code{speed.calib.coefs}:}{Object of class \dQuote{numeric}
      the intercept and slope derived from the speed calibration
      procedure.  Defaults to c(0, 1) meaning uncalibrated speeds.}

  }
}


\details{This is perhaps the most important class in diveMove, as it
  holds all the information necessary for calculating requested
  summaries for a TDR.}

% -------------------------------------------------------------------------

\author{Sebastian P. Luque \email{spluque@gmail.com}}


\seealso{\code{\link{TDR}} for links to other classes in the package.
  \code{\link{TDRcalibrate-methods}} for the various methods available.}


\keyword{classes}
