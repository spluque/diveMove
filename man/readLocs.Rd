\name{readLocs}
\alias{readLocs}

% -------------------------------------------------------------------------

\title{Read comma-delimited file with location data}


\description{Read a delimited (*.csv) file with (at least) time,
  latitude, longitude readings.}


\usage{
readLocs(locations, loc.idCol, idCol, dateCol, timeCol=NULL,
         dtformat="\%m/\%d/\%Y \%H:\%M:\%S", tz="GMT",
         classCol, lonCol, latCol, alt.lonCol=NULL, alt.latCol=NULL, \ldots)
}
%- maybe also 'usage' for other objects documented here.


\arguments{

  \item{locations}{character: a string indicating the path to the file
    to read, or a \code{\link{data.frame}} available in the search
    list. Provide the entire path if the file is not on the current
    directory.  This can also be a text-mode connection, as allowed in
    \code{\link{read.csv}}.}

  \item{loc.idCol}{integer: column number containing location ID.  If
    missing, a \code{loc.id} column is generated with sequential
    integers as long as the input.}

  \item{idCol}{integer: column number containing an identifier for
    locations belonging to different groups.  If missing, an id column
    is generated with number one repeated as many times as the input.}

  \item{dateCol}{integer: column number containing dates, and,
    optionally, times.}

  \item{timeCol}{integer: column number containing times.}

  \item{dtformat}{character: a string specifying the format in which the
    date and time columns, when pasted together, should be interpreted
    (see \code{\link{strptime}}) in \code{file}.}

  \item{tz}{character: a string indicating the time zone for the date
    and time readings.}

  \item{lonCol}{integer: column number containing longitude readings.}

  \item{latCol}{integer: column number containing latitude readings.}

  \item{classCol}{integer: column number containing the ARGOS rating for
    each location.}

  \item{alt.lonCol}{integer: column number containing alternative
    longitude readings.}

  \item{alt.latCol}{integer: Column number containing alternative
    latitude readings.}

  \item{\ldots}{Passed to \code{\link{read.csv}}}

}


\details{The file must have a header row identifying each field, and all
  rows must be complete (i.e. have the same number of fields). Field
  names need not follow any convention.}


\value{A data frame.}

% -------------------------------------------------------------------------

\examples{

## Do example to define object zz with location of dataset
utils::example("sealLocs", package="diveMove",
               ask=FALSE, echo=FALSE)
locs <- readLocs(zz, idCol=1, dateCol=2,
                 dtformat="\%Y-\%m-\%d \%H:\%M:\%S", classCol=3,
                 lonCol=4, latCol=5, sep=";")

summary(locs)

}


\author{Sebastian P. Luque \email{spluque@gmail.com}}


\keyword{manip}
