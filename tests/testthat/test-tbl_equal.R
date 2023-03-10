# A data frame with all major types
df_all <- data.frame(
	a = c(1, 2.5),
	b = 1:2,
	c = c(T, F),
	d = c("a", "b"),
	e = factor(c("a", "b")),
	f = Sys.Date() + 1:2,
	g = Sys.time() + 1:2,
	stringsAsFactors = FALSE
)

test_that("data frames equal to themselves", {
	expect_true(tbl_equal(mtcars, mtcars))
	expect_true(tbl_equal(iris, iris))
	expect_true(tbl_equal(df_all, df_all))
})

test_that("data frames not equal if missing row", {
	expect_false(tbl_equal(mtcars, mtcars[-1, ]))
	expect_false(tbl_equal(iris, iris[-1, ]))
	expect_false(tbl_equal(df_all, df_all[-1, ]))
})

test_that("data frames not equal if missing col", {
	expect_false(tbl_equal(mtcars, mtcars[, -1]))
	expect_false(tbl_equal(iris, iris[, -1]))
	expect_false(tbl_equal(df_all, df_all[, -1]))
})

test_that("factors equal only if levels equal", {
	df1 <- tibble::tibble(x = factor(c("a", "b")))
	df2 <- tibble::tibble(x = factor(c("a", "d")))
	expect_false(tbl_equal(df1, df2))
	expect_false(tbl_equal(df2, df1))
})

test_that("factor comparison requires strict equality of levels (#2440)", {
	df1 <- tibble::tibble(x = factor("a"))
	df2 <- tibble::tibble(x = factor("a", levels = c("a", "b")))
	expect_true(tbl_equal(df1, df2, check_column_levels = FALSE))
	expect_true(tbl_equal(df2, df1, check_column_levels = FALSE))

	expect_false(tbl_equal(df1, df2))
	expect_false(tbl_equal(df2, df1))
})

test_that("all.equal.data.frame handles data.frames with NULL names", {
	x <- data.frame(LETTERS[1:3], rnorm(3))
	names(x) <- NULL
	expect_true(tbl_equal(x, x))
})

test_that("all.equal handles NA_character_ correctly. #1095", {
	d1 <- tibble::tibble(x = c(NA_character_))
	expect_true(tbl_equal(d1, d1))

	d2 <- tibble::tibble(x = c(NA_character_, "foo", "bar"))
	expect_true(tbl_equal(d2, d2))
})

test_that("handle Date columns of different types, integer and numeric (#1204)", {
	a <- data.frame(date = as.Date("2015-06-07"))
	b <- data.frame(date = structure(as.integer(a$date), class = "Date"))
	expect_true(tbl_equal(a, b))
})

test_that("equality test fails when convert is FALSE and types don't match (#1484)", {
	df1 <- tibble::tibble(x = "a")
	df2 <- tibble::tibble(x = factor("a"))
	expect_true(tbl_equal(df1, df2, check_column_class = FALSE))
	expect_false(tbl_equal(df1, df2, check_column_class = TRUE))
})

test_that("equality handle raw columns", {
	df <- tibble::tibble(a = 1:3, b = as.raw(1:3))
	expect_true(tbl_equal(df, df))
})

test_that("numeric and integer can be compared if convert = TRUE", {
	df1 <- tibble::tibble(x = 1:3)
	df2 <- tibble::tibble(x = as.numeric(1:3))
	expect_true(tbl_equal(df1, df2, check_column_class = FALSE))
	expect_false(tbl_equal(df1, df2))
})

test_that("ignore column order", {
	expect_false(
		tbl_equal(tibble::tibble(a = 1, b = 2), tibble::tibble(b = 2, a = 1), check_column_order = TRUE)
	)
	expect_false(
		tbl_equal(tibble::tibble(a = 1, b = 2), tibble::tibble(a = 1), check_column_order = TRUE)
	)
})
