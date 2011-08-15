\name{ISOweek}
\alias{ISOweek}
\title{Week of the year according to ISO 8601}
\usage{
  ISOweek(date)
}
\arguments{
  \item{date}{Vector which can be coerced to class
  \code{Date}}
}
\value{
  A character vector of year and week in format
  "\code{\%Y-W\%V}"
}
\description{
  This function returns the year and the week of the year
  of a given date according to ISO 8601. It is an
  substitute for the \code{\%Y-W\%V} format which is not
  implemented on Windows.
}
\details{
  According to ISO 8601, the year of the week can differ
  from the calendar year (see the examples).
}
\examples{
x <- paste(1999:2011, "-12-31", sep="")
y <- as.Date(x)
data.frame(date = format(y), week = ISOweek(y))
data.frame(date = x, week = ISOweekday(x))
}
\author{
  Hatto von Hatzfeld <hatto@salesianer.de>, adopted to R by
  Uwe Block <u.block.mz@gmail.com>
}
\references{
  \url{http://www.salesianer.de/util/kalwoch.html}
}
\seealso{
  \code{\link{strptime}}
}

