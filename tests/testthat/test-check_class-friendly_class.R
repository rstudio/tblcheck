test_that("friendly_class() default", {
	expect_equal(
		friendly_class(structure(1, class = "test")),
		"an object with class `test`"
	)
	expect_equal(
		friendly_class(structure(1, class = c("test_1", "test_2"))),
		"an object with classes `test_1` and `test_2`"
	)
	expect_equal(
		friendly_class(structure(1, class = c("test_1", "test_2", "test_3"))),
		"an object with classes `test_1`, `test_2`, and `test_3`"
	)
})

test_that("friendly_class() character", {
  expect_equal(
  	friendly_class("a"),
  	"a text string (class `character`)"
  )
	expect_equal(
		friendly_class(letters),
		"a vector of text (class `character`)"
	)
})

test_that("friendly_class() numeric", {
	expect_equal(
		friendly_class(1),
		"a number (class `numeric`)"
	)
	expect_equal(
		friendly_class(c(1, 2)),
		"a vector of numbers (class `numeric`)"
	)
})

test_that("friendly_class() integer", {
	expect_equal(
		friendly_class(1L),
		"an integer (class `integer`)"
	)
	expect_equal(
		friendly_class(1:2),
		"a vector of integers (class `integer`)"
	)
})

test_that("friendly_class() logical", {
	expect_equal(
		friendly_class(TRUE),
		"a `TRUE`/`FALSE` value (class `logical`)"
	)
	expect_equal(
		friendly_class(c(TRUE, FALSE)),
		"a vector of `TRUE`/`FALSE` values (class `logical`)"
	)
	expect_equal(
		friendly_class(NA),
		"a `TRUE`/`FALSE`/`NA` value (class `logical`)"
	)
	expect_equal(
		friendly_class(c(TRUE, NA)),
		"a vector of `TRUE`/`FALSE`/`NA` values (class `logical`)"
	)
})

test_that("friendly_class() complex", {
	expect_equal(
		friendly_class(1i),
		"a complex number (class `complex`)"
	)
	expect_equal(
		friendly_class(c(1, 1i)),
		"a vector of complex numbers (class `complex`)"
	)
})

test_that("friendly_class() complex", {
	expect_equal(
		friendly_class(1i),
		"a complex number (class `complex`)"
	)
	expect_equal(
		friendly_class(c(1, 1i)),
		"a vector of complex numbers (class `complex`)"
	)
})

test_that("friendly_class() raw", {
	expect_equal(
		friendly_class(raw(1)),
		"a raw byte value (class `raw`)"
	)
	expect_equal(
		friendly_class(raw(2)),
		"a vector of raw byte values (class `raw`)"
	)
})

test_that("friendly_class() factor", {
	expect_equal(
		friendly_class(factor(1)),
		"a factor (class `factor`)"
	)
	expect_equal(
		friendly_class(factor(1:2)),
		"a vector of factors (class `factor`)"
	)
})

test_that("friendly_class() Date", {
	expect_equal(
		friendly_class(lubridate::ymd("2022-12-01")),
		"a date (class `Date`)"
	)
	expect_equal(
		friendly_class(lubridate::ymd(c("2022-12-01", "2022-12-02"))),
		"a vector of dates (class `Date`)"
	)
})

test_that("friendly_class() POSIXt", {
	expect_equal(
		friendly_class(Sys.time()),
		"a date-time (class `POSIXct`)"
	)
	expect_equal(
		friendly_class(c(Sys.time(), Sys.time())),
		"a vector of date-times (class `POSIXct`)"
	)
	expect_equal(
		friendly_class(as.POSIXlt(Sys.time())),
		"a date-time (class `POSIXlt`)"
	)
	expect_equal(
		friendly_class(as.POSIXlt(c(Sys.time(), Sys.time()))),
		"a vector of date-times (class `POSIXlt`)"
	)
})

test_that("friendly_class() Period", {
	expect_equal(
		friendly_class(lubridate::hms("1:01:02")),
		"a time period (class `Period`)"
	)
	expect_equal(
		friendly_class(lubridate::hms(c("3:05:08", "13:21:34"))),
		"a vector of time periods (class `Period`)"
	)
})

test_that("friendly_class() data.frame", {
	expect_equal(
		friendly_class(mtcars),
		"a data frame (class `data.frame`)"
	)
})

test_that("friendly_class() tbl_df", {
	expect_equal(
		friendly_class(tibble::as_tibble(mtcars)),
		"a tibble (class `tbl_df`)"
	)
})

test_that("friendly_class() grouped_df", {
	expect_equal(
		friendly_class(dplyr::group_by(mtcars, cyl)),
		"a grouped tibble (class `grouped_df`)"
	)
})

test_that("friendly_class() rowwise_df", {
	expect_equal(
		friendly_class(dplyr::rowwise(mtcars)),
		"a rowwise tibble (class `rowwise_df`)"
	)
})


test_that("friendly_class() list", {
	expect_equal(
		friendly_class(list()),
		"a list (class `list`)"
	)
})

test_that("friendly_class() matrix", {
	expect_equal(
		friendly_class(matrix(1)),
		"a matrix (class `matrix`)"
	)
})

test_that("friendly_class() array", {
	expect_equal(
		friendly_class(array(1)),
		"an array (class `array`)"
	)
})
