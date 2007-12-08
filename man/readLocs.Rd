% $Id: readLocs.Rd,v 1.5 2007-12-08 21:50:54 sluque Exp $
\name{readLocs}
\alias{readLocs}

% -------------------------------------------------------------------------

\title{Read comma-delimited file with location data}


\description{Read a comma delimited (*.csv) file with (at least) time,
  latitude, longitude readings.}


\usage{
readLocs(file, loc.idCol, idCol, dateCol, timeCol=NULL,
         dtformat="\%m/\%d/\%Y \%H:\%M:\%S", tz="GMT",
         classCol, lonCol, latCol, alt.lonCol=NULL, alt.latCol=NULL, \ldots)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{file}{A string indicating the name of the file to read. Provide
    the entire path if the file is not on the current directory.}

  \item{loc.idCol}{Column number containing location ID.  If missing, a
    \code{loc.id} column is generated with sequential integers as long
    as the input.}

  \item{idCol}{Column number containing an identifier for locations
    belonging to different groups.  If missing, an id column is
    generated with number one repeated as many times as the input.}

  \item{dateCol}{Column number containing dates, and, optionally,
    times.}

  \item{timeCol}{Column number containing times.}

  \item{dtformat}{A string, specifying the format in which the date and
    time columns, when pasted together, should be interpreted (see
    \code{\link{strptime}}) in \code{file}.}

  \item{tz}{A string indicating the time zone for the date and time
    readings.}

  \item{lonCol}{Column number containing longitude readings.}

  \item{latCol}{Column number containing latitude readings.}

  \item{classCol}{Column number containing the ARGOS rating for each
    location.}

  \item{alt.lonCol}{Column number containing alternative longitude
    readings.}

  \item{alt.latCol}{Column number containing alternative latitude
    readings.}

  \item{\ldots}{Passed to \code{\link{read.csv}}}

}


\details{The file must have a header row identifying each field, and all
  rows must be complete (i.e. have the same number of fields). Field
  names need not follow any convention.}


\value{A data frame.}

% -------------------------------------------------------------------------

\examples{

locs <- readLocs(system.file(file.path("data", "sealLocs.csv"),
                             package="diveMove"), idCol=1, dateCol=2,
                 dtformat="\%Y-\%m-\%d \%H:\%M:\%S",
                 classCol=3, lonCol=4, latCol=5)

summary(locs)

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}
