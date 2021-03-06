ISOweek 0.6.4.9000
-------------

MINOR CHANGES

* Add CRAN download badge to `README.md` file.
* Add CRAN badge to `README.md` file.
* Rename `README` to `README.md`, reformat contents with `markdown`, and 
  update description of package.
* Udate `DOCUMENTATION` file.
* Move tests from `inst` to `tests/testthat`.
* Replace `test_package` by `test_check` to pass CRAN checks.
* Rename `NEWS` to `NEWS.md` and reformat contents with `markdown`.


ISOweek 0.6-4
-------------

MINOR CHANGES

* Updated documentation to reflect enhancements in `strftime` introduced with R.3.1.0.


ISOweek 0.6-3
-------------

BUG FIX

* Changed reference to package `surveillance which` caused problems on `MacOS`.


ISOweek 0.6-2
-------------

BUG FIX

* Fixed problems when package `lubridate` is loaded.
  `lubridate` overrides `+` and `-` methods for `POSIXt`, `Date`, and `difftime`.


ISOweek 0.6-1
-------------

MINOR CHANGES

* Minor corrections to the documentation.

* Added one more example to `ISOweek2date`.

* Moved internal functions to separate file, added keyword internal.

* Corrected email address of maintainer.


ISOweek 0.6.0
-------------

MAJOR CHANGES

* Added the functions `date2ISOweek` and `ISOweek2date`.


ISOweek 0.5
-----------

MAJOR CHANGES

* Code was restructured and rewritten. Comments and variable names were translated 
  from German to English.

* Documentation added to R sources using `roxygen2`.

* Test cases added for use by `test_that`.
