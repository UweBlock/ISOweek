context("Internal functions")

test_that("weekday0 returns proper values", {
  input_chr <- paste(1999:2011, "-12-31", sep="")
  input_date <- as.Date(input_chr)
  result_int <- as.integer(c(5, 7, 1, 2, 3, 5, 6, 7, 1, 3, 4, 5, 6)) - 1L
  expect_that(weekday0(input_date), equals(result_int))
  expect_that(weekday0(input_chr), equals(result_int))
})

test_that("thursday0 returns proper values", {
  expect_that(thursday0(Sys.Date()), is_a("Date"))
  expect_that(weekday0(thursday0("1999-12-31")), equals(3L))
  expect_that(abs(thursday0(Sys.Date()) - Sys.Date()) <= 3, is_true())
})

test_that("year0 returns proper values", {
  expect_that(year0(Sys.Date()), is_a("integer"))
  expect_that(year0(c("1999-12-31", "2000-01-01")), equals(c(1999, 2000)))
})

context("ISOweek")

test_that("ISOweek returns proper format", {
  expect_that(ISOweek("1999-12-31"), is_a("character"))
  expect_that(ISOweek("1999-12-31"), matches("[0-9]{4}-W[0-9]{2}"))
})
test_that("ISOweek returns proper values", {
  input_chr <- paste(1999:2011, "-12-31", sep="")
  input_date <- as.Date(input_chr)
  result_chr <- c("1999-W52", "2000-W52", "2002-W01", "2003-W01", "2004-W01", 
                  "2004-W53", "2005-W52", "2006-W52", "2008-W01", "2009-W01",
                  "2009-W53", "2010-W52", "2011-W52")
  expect_that(ISOweek(input_date), equals(result_chr))
  expect_that(ISOweek(input_chr), equals(result_chr))
})
test_that("ISOweek handles NAs", {
  expect_that(ISOweekday(NA), equals(NA_integer_))
  expect_that(ISOweekday(NA_character_), equals(NA_integer_))
  input_chr <- paste(1999:2011, "-12-31", sep="")
  result_chr <- c("1999-W52", "2000-W52", "2002-W01", "2003-W01", "2004-W01", 
                  "2004-W53", "2005-W52", "2006-W52", "2008-W01", "2009-W01",
                  "2009-W53", "2010-W52", "2011-W52")
  idx_NA <- seq(from =2, to = length(input_chr), by = 2)
  input_chr_NA <- input_chr
  input_chr_NA[idx_NA] <- NA_character_
  input_date_NA <- as.Date(input_chr_NA)
  result_chr_NA <- result_chr
  result_chr_NA[idx_NA] <- NA
  expect_that(ISOweek(input_date_NA), equals(result_chr_NA))
  expect_that(ISOweek(input_chr_NA), equals(result_chr_NA))
})
test_that("ISOweekday stops on invalid parameters", {
  expect_that(ISOweek(31), throws_error())
  expect_that(ISOweek(31.1), throws_error())
  expect_that(ISOweek(FALSE), throws_error())
  expect_that(ISOweek("31"), throws_error())
  expect_that(ISOweek("12/31/1999"), throws_error())
})
