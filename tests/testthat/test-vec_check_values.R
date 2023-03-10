test_that("value differences", {
	grade_default <- tblcheck_test_grade({
		.result <- 1:10
		.solution <- 11:20
		vec_grade_values()
	})

	expect_snapshot(grade_default)

	expect_problem(
		grade_default$problem,
		type = "values",
		expected = 11:20,
		actual = 1:10
	)

	grade_1 <- tblcheck_test_grade({
		.result <- 1:10
		.solution <- 11:20
		vec_grade_values(max_diffs = 1)
	})

	expect_snapshot(grade_1)
	expect_equal(grade_1$problem, grade_default$problem)

	grade_5 <- tblcheck_test_grade({
		.result <- 1:10
		.solution <- 11:20
		vec_grade_values(max_diffs = 5)
	})

	expect_snapshot(grade_5)
	expect_equal(grade_5$problem, grade_default$problem)

	grade_Inf <- tblcheck_test_grade({
		.result <- 1:10
		.solution <- 11:20
		vec_grade_values(max_diffs = Inf)
	})

	expect_snapshot(grade_Inf)
	expect_equal(grade_Inf$problem, grade_default$problem)
})

test_that("NA values", {
	grade_na <- tblcheck_test_grade({
		.result <- c(TRUE, TRUE, NA)
		.solution <- c(TRUE, TRUE, TRUE)
		vec_grade_values()
	})

	expect_snapshot(grade_na)

	expect_problem(
		grade_na$problem,
		type = "values",
		expected = c(TRUE, TRUE, TRUE),
		actual = c(TRUE, TRUE, NA)
	)

	grade_na_match <- tblcheck_test_grade({
		.result <- c(TRUE, TRUE, NA)
		.solution <- c(TRUE, TRUE, NA)
		vec_grade_values()
	})

	expect_null(grade_na_match)
})

test_that("vec_grade_values() failures", {
	grade_length <- tblcheck_test_grade({
		.result <- 1:9
		.solution <- 1:10
		vec_grade_values()
	})

	expect_equal(grade_length, vec_grade_dimensions(1:9, 1:10))

	grade_class <- tblcheck_test_grade({
		.result <- 1:10
		.solution <- letters[1:10]
		vec_grade_values()
	})

	expect_equal(grade_class, vec_grade_class(1:10, letters[1:10]))

	grade_attr <- tblcheck_test_grade({
		.result <- structure(1, class = "test", attr = "not a")
		.solution <- structure(1, class = "test", attr = "match")
		vec_grade_values()
	})

	expect_snapshot(grade_attr)
	expect_problem(grade_attr$problem, type = "values")
})

test_that("column values problem messages are created correctly", {
	grade_column <- tblcheck_test_grade({
		set.seed(4231)
		.result <- tibble::tibble(x = c(1:5, runif(5)))
		.solution <- tibble::tibble(x = c(1:5, runif(5)))
		tbl_grade_column("x")
	})

	expect_snapshot(grade_column)

	expect_match(grade_column$message, "your `x` column")

	grade_tbl <- tblcheck_test_grade({
		set.seed(4231)
		.result <- tibble::tibble(x = c(1:5, runif(5)))
		.solution <- tibble::tibble(x = c(1:5, runif(5)))
		tbl_grade()
	})

	expect_snapshot(grade_tbl)

	expect_match(grade_tbl$message, "your `x` column")
})

test_that("floating point differences are ignored by default", {
	grade_tolerant <- tblcheck_test_grade({
		.result <- sqrt(2) ^ 2
		.solution <- 2
		vec_grade_values()
	})

	expect_null(grade_tolerant)

	grade_intolerant <- tblcheck_test_grade({
		.result <- sqrt(2) ^ 2
		.solution <- 2
		vec_grade_values(tolerance = 0)
	})

	expect_snapshot(grade_intolerant)

	expect_equal(
		grade_intolerant$problem,
		problem("values", 2, sqrt(2) ^ 2),
		ignore_attr = "class"
	)
})
