#' Weekday as integer number (0-6, Monday is 0) 
#'
#' This internal function returns the weekday of a given date.
#' 
#' The week starts on Monday and ends on Sunday.
#'
#' @param date Vector which can be coerced to class \code{Date}
#' @return An integer vector of weekdays (0-6, Monday is 0)
#' @seealso \code{\link{ISOweekday}}
weekday0 <- function(date) {
  return(ISOweekday(date) - 1L)
}

#' Date of the nearest Thursday of a given date
#'
#' This internal function returns the date of the Thursday of the week in which the given date is located.
#' 
#' The week starts on Monday and ends on Sunday.
#'
#' @param date Vector which can be coerced to class \code{Date}
#' @return A vector of dates of the nearest Thursdays
thursday0 <- function(date) {
  date <- as.Date(date)
  return(date - weekday0(date) + 3)
}

#' Calendar year of a given date
#'
#' This internal function returns the year with century as integer.
#'
#' @param date Vector which can be coerced to class \code{Date}
#' @return An integer vector of years
year0 <- function(date) {
  date <- as.Date(date)
	return(as.integer(format(date, "%Y")))
}

#' Week of the year according to ISO 8601
#'
#' This function returns the year and the week of the year of a given date according to ISO 8601.
#' It is an substitute for the \code{\%Y-W\%V} format which is not implemented on Windows.
#'
#' According to ISO 8601, the year of the week can differ from the calendar year (see the examples).
#'
#' @param date Vector which can be coerced to class \code{Date}
#' @return A character vector of year and week in format "\code{\%Y-W\%V}"
#' @seealso \code{\link{strptime}} for a description of the date formats and references on ISO 8601. 
#'   \code{\link[surveillance]{isoWeekYear}} for an alternative implementation.
#' @export
#' @author Hatto von Hatzfeld <\email{hatto@@salesianer.de}>, adopted to \R by Uwe Block 
#'   <\email{u.block.mz@@gmail.com}>
#' @references \url{http://www.salesianer.de/util/kalwoch.html}
#' @examples
#' x <- paste(1999:2011, "-12-31", sep="")
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
  # as.numeric is required to convert difftime object
	week <- as.numeric(nearest_thursday - first_thursday) %/% 7 + 1
  # use sprintf to produce leading zeros for the week number
	return(ifelse(is.na(week), NA_character_, sprintf("%04.0f-W%02.0f", year, week)))
}

