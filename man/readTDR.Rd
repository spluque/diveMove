\name{readTDR}
\encoding{latin1}

\alias{readTDR}
\alias{createTDR}

% -------------------------------------------------------------------------

\title{Read comma-delimited file with TDR data}



\description{Read a comma delimited (*.csv) file containing time-depth
  recorder (TDR) data from various TDR models.  Return a \code{TDR} or
  \code{TDRspeed} object. \code{createTDR} creates an object of one of
  these classes from other objects.}


\usage{
readTDR(file, dateCol=1, timeCol=2, depthCol=3, speed=FALSE,
        subsamp=5, concurrentCols=4:6,
        dtformat="\%d/\%m/\%Y \%H:\%M:\%S", tz="GMT")
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
  speed or velocity, then an object of class TDRspeed is created, where
  speed is considered the column matching this name.}


\value{An object of class \sQuote{TDR} or \sQuote{TDRspeed}.}

% -------------------------------------------------------------------------

\author{\enc{Sebastián}{Sebastian} P. Luque \email{spluque@gmail.com}}


\examples{
readTDR(system.file(file.path("data", "dives.csv"),
                    package="diveMove"), speed=TRUE)

## Or more pedestrian
tdrX <- read.csv(system.file(file.path("data", "dives.csv"),
                             package="diveMove"), na.strings="", as.is=TRUE)
date.time <- paste(tdrX$date, tdrX$time)
tdr.time <- as.POSIXct(strptime(date.time, format="\%d/\%m/\%Y \%H:\%M:\%S"),
                       tz="GMT")
createTDR(tdr.time, tdrX$depth, concurrentData=data.frame(speed=tdrX$speed),
          file="dives.csv", speed=TRUE)

}


\keyword{manip}% at least one, from doc/KEYWORDS
