% $Id$
\name{readTDR}

\alias{readTDR}
\alias{createTDR}

% -------------------------------------------------------------------------

\title{Read comma-delimited file with "TDR" data}



\description{Read a delimited (*.csv) file containing time-depth
  recorder (\dfn{TDR}) data from various \acronym{TDR} models.  Return a
  \code{TDR} or \code{TDRspeed} object. \code{createTDR} creates an
  object of one of these classes from other objects.}


\usage{
readTDR(file, dateCol=1, timeCol=2, depthCol=3, speed=FALSE,
        subsamp=5, concurrentCols=4:6,
        dtformat="\%d/\%m/\%Y \%H:\%M:\%S", tz="GMT", \ldots)
createTDR(time, depth, concurrentData=data.frame(), speed=FALSE, dtime, file)
}


\arguments{

  \item{file}{A string indicating the path to the file to read.}

  \item{dateCol}{Column number containing dates, and optionally, times.}

  \item{timeCol}{Column number with times.}

  \item{depthCol}{Column number containing depth readings.}

  \item{speed}{For \code{readTDR}: Logical indicating whether speed is
    included in one of the columns of concurrentCols.}

  \item{subsamp}{Subsample rows in \code{file} with \code{subsamp}
    interval, in s.}

  \item{concurrentCols}{Column numbers to include as concurrent data
    collected.}

  \item{dtformat}{A string, specifying the format in which the date and
    time columns, when pasted together, should be interpreted (see
    \code{\link{strptime}}).}

  \item{tz}{A string indicating the time zone assumed for the date and
    time readings.}

  \item{\ldots}{Passed to \code{\link{read.csv}}}

  \item{time}{A \code{POSIXct} object with date and time readings for
    each reading.}

  \item{depth}{Numeric vector with depth readings.}

  \item{concurrentData}{Data frame with additional, concurrent data
    collected.}

  \item{dtime}{Sampling interval used in seconds.  If missing, it is
    calculated from the \code{time} argument.}

}


\details{The input file is assumed to have a header row identifying each
  field, and all rows must be complete (i.e. have the same number of
  fields).  Field names need not follow any convention.  However, depth
  and speed are assumed to be in m, and \eqn{m \cdot s^{-1}}{m/s},
  respectively, for further analyses.

  If \var{speed} is TRUE and concurrentCols contains a column named
  speed or velocity, then an object of class \code{\link{TDRspeed}} is
  created, where speed is considered to be the column matching this
  name.}


\note{Although \code{\link{TDR}} and \code{\link{TDRspeed}} classes
  check that time stamps are in increasing order, the integrity of the
  input must be thoroughly verified for common errors present in text
  output from \acronym{TDR} devices such as duplicate records, missing
  time stamps and non-numeric characters in numeric fields.  These
  errors are much more efficiently dealt with outside of \acronym{GNU}
  \R using tools like \code{GNU awk} or \code{GNU sed}, so
  \code{\link{diveMove}} does not currently attempt to fix these
  errors.}


\value{An object of class \code{\link{TDR}} or \code{\link{TDRspeed}}.}

% -------------------------------------------------------------------------

\examples{
readTDR(system.file(file.path("data", "dives.csv"),
                    package="diveMove"),
        speed=TRUE, sep=";", na.strings="", as.is=TRUE)

## Or more pedestrian
tdrX <- read.csv(system.file(file.path("data", "dives.csv"),
                             package="diveMove"),
                 sep=";", na.strings="", as.is=TRUE)
date.time <- paste(tdrX$date, tdrX$time)
tdr.time <- as.POSIXct(strptime(date.time, format="\%d/\%m/\%Y \%H:\%M:\%S"),
                       tz="GMT")
createTDR(tdr.time, tdrX$depth, concurrentData=data.frame(speed=tdrX$speed),
          file="dives.csv", speed=TRUE)

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}% at least one, from doc/KEYWORDS
