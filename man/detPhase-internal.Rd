\name{detPhase-internal}
\alias{detPhase-internal}

\alias{.detPhase}

% -------------------------------------------------------------------------

\title{Detect phases of activity from depth readings}



\description{Functions to identify sections of a \acronym{TDR} record
  displaying one of three possible activities: dry, wet, and trivial
  wet.}


\usage{
.detPhase(time, depth, dry.thr, wet.cond, wet.thr, interval)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{time}{\code{POSIXct} object with date and time for all depths.}

  \item{depth}{numeric vector with depth readings.}

  \item{dry.thr, wet.cond, wet.thr}{As passed from
    \code{\link{calibrateDepth}}.}

  \item{interval}{As passed from \code{\link{calibrateDepth}}; sampling
    interval in seconds.}

}


\details{See \code{\link{calibrateDepth}}.}


\value{A list with components:

  \item{phase.id}{Numeric vector identifying each activity phase,
    starting from 1 for every input record.}

  \item{activity}{Factor with levels \dQuote{L} indicating dry,
    \dQuote{W} indicating wet, \dQuote{U} for underwater (above dive
    criterion), \dQuote{D} for diving, \dQuote{Z} for trivial wet animal
    activities.  Only \dQuote{L}, \dQuote{W}, and \dQuote{Z} are
    actually represented.}

  \item{begin}{A \code{\link{POSIXct}} object as long as the number of
    unique activity phases identified, indicating the start times for
    each activity phase.}

  \item{end}{A \code{\link{POSIXct}} object as long as the number of
    unique activity phases identified, indicating the end times for each
    activity phase.}

}


\seealso{\code{\link{.detDive}}, \code{\link{calibrateDepth}}}

% -------------------------------------------------------------------------

\examples{

data(divesTDR)
depths <- getDepth(divesTDR)
times <- getTime(divesTDR)

detp <- diveMove:::.detPhase(times, depths, dry.thr=70, wet.thr=3610,
                             interval=getDtime(divesTDR))
## Plot detected phases
plotTDR(times, depths, interact=FALSE)
rect(xleft=detp$begin, xright=detp$end, ybottom=0, ytop=-4,
     col=seq_along(detp$begin))

}

\author{Sebastian P. Luque \email{spluque@gmail.com} and Andy Liaw.}


\keyword{internal}% at least one, from doc/KEYWORDS
