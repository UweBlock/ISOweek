#' Week of the year according to ISO 8601
#'
#' Returns the year and the week of the year of a given date according to ISO 8601.
#' It is an alternative to \code{strftime(x, "\%G-W\%V")}.  
#' 
#' The conversion specifications \code{\%g}, \code{\%G}, \code{\%V}, \code{\%u} 
#' where not available on Windows and MacOS before \code{R 3.1.0}.
#' According to ISO 8601, the year of the week can differ from the calendar year (see the examples).
#'
#' 
#' @param date Vector which can be coerced to class \code{Date}.
#' @return A character vector of year and week in format "\code{\%G-W\%V}".
#' @seealso \code{\link{strftime}} for a description of conversion specifications and references 
#' on ISO 8601. 
#' @seealso \code{isoWeekYear} in the \code{surveillance} package for an alternative implementation.
#' @export
#' @author Hatto von Hatzfeld \email{hatto@@salesianer.de}, 
#'   adopted to \R by Uwe Block \email{u.block.mz@@gmail.com}
#' @references \url{http://www.salesianer.de/util/kalwoch.html}
#' @examples
#' x <- paste(1999:2011, "-12-31", sep = "")
#' y <- as.Date(x)
#' data.frame(date = format(y), week = ISOweek(y))
#' data.frame(date = x, week = ISOweek(x))
ISOweek <- function(date) {
  date <- as.Date(date)
	nearest_thursday <- thursday0(date)
	year <- year0(nearest_thursday)   # year of the week can differ from year of the date
  # first week of the year includes always the 4th of January,
  # take care of NA dates
  january04 <- as.Date(ifelse(is.na(year), NA, paste(year, "01", "04", sep="-")))
  # first thursday of the year
	first_thursday <- thursday0(january04)
  # difference in days, use difftime explicitly to avoid problems with package "lubridate"
  # lubridate overrides + and - methods for POSIXt, Date and difftime
  diffdays <- difftime(nearest_thursday, first_thursday, units = "days")
  stopifnot(class(diffdays) == "difftime")
  # as.integer is required to convert difftime object
	week <- as.integer(diffdays) %/% 7 + 1
  # use sprintf to produce leading zeros for the week number
	return(ifelse(is.na(week), NA_character_, sprintf("%04.0f-W%02.0f", year, week)))
}

