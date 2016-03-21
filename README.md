
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/ISOweek)](http://cran.r-project.org/package=ISOweek)  

ISOweek
=======

Helper functions to deal with week of the year and weekday according to ISO 8601.

This is an easy-to-use alternative to `strftime(x, "%G-W%V")` and a substitute for 
`strptime(x, "%G-W%V-%u")` on input. In addition, the package offers functions 
to convert from standard calender format `yyyy-mm-dd` to and from ISO 8601 week 
format `yyyy-Www-d`.
