\name{ISOweek2date}
\alias{ISOweek2date}
\title{Converts date from week notation according to ISO 8601 to standard notation}
\usage{
  ISOweek2date(weekdate)
}
\arguments{
  \item{weekdate}{A character vector of year, week, and
  weekday in format "\code{\%Y-W\%V-\%u}"}
}
\value{
  A vector of class \code{Date}
}
\description{
  This function returns the date of a given weekdate (year,
  week of the year, day of week according to ISO 8601). It
  is the inverse function to \code{date2ISOweek}.
}
\details{
  According to ISO 8601, the year of the week can differ
  from the calendar year (see the examples).
}
\examples{
x <- paste(1999:2011, "-12-31", sep="")
w <- date2ISOweek(x)
d <- ISOweek2date(w)
data.frame(date = x, weekdate = w, date2 = d)
}
\author{
  Uwe Block <\email{u.block.mz@gmail.com}>
}
\seealso{
  \code{\link{strptime}} for a description of the date
  formats and references on ISO 8601.
}

