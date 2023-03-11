test_that("tbl_grade_column() checks classes", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters)
		.solution <- tibble::tibble(a = 1:3)
		tbl_grade_column("a", .result, .solution)
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "class",
		expected = 1:3,
		actual = letters,
		location = "column",
		column = "a"
	)

	grade_int <- tblcheck_test_grade({
		.result <- tibble::tibble(a = c(1, 2, 3))
		.solution <- tibble::tibble(a = 1:3)
		tbl_grade_column("a", .result, .solution)
	})

	expect_snapshot(grade_int)

	expect_problem(
		grade_int$problem,
		type = "class",
		expected = as.integer(c(1, 2, 3)),
		actual = as.numeric(c(1, 2, 3)),
		location = "column",
		column = "a"
	)

	grade_int_ignore <- tblcheck_test_grade({
		.result <- tibble::tibble(a = c(1, 2, 3))
		.solution <- tibble::tibble(a = 1:3)
		tbl_grade_column(
			"a", .result, .solution, ignore_class = c("integer" = "numeric")
		)
	})

	expect_null(grade_int_ignore)
})

test_that("tbl_grade_column() checks the first three values", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = rev(letters))
		.solution <- tibble::tibble(a = letters)
		tbl_grade_column("a")
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "values",
		expected = letters,
		actual = rev(letters),
		location = "column",
		column = "a"
	)
})

test_that("tbl_grade_column() checks multiple classes", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = data.frame(x = 1))
		.solution <- tibble::tibble(a = tibble::tibble(x = 1))
		tbl_grade_column("a")
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "class",
		expected = tibble::tibble(x = 1),
		actual = data.frame(x = 1),
		location = "column",
		column = "a"
	)
})

test_that("tbl_grade_column() checks for value differences beyond the first 3", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = c(rep(1, 3), 5:10))
		.solution <- tibble::tibble(a = c(rep(1, 3), 10:15))
		tbl_grade_column("a")
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "values",
		expected = c(rep(1, 3), 10:15),
		actual = c(rep(1, 3), 5:10),
		location = "column",
		column = "a"
	)
})

test_that("max_diffs modifies the number of values to print", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters)
		.solution <- tibble::tibble(a = rev(letters))
		tbl_grade_column("a", max_diffs = 5)
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "values",
		expected = rev(letters),
		actual = letters,
		location = "column",
		column = "a"
	)
})

test_that("max_diffs doesn't overflow", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:2])
		.solution <- tibble::tibble(a = letters[2:1])
		tbl_grade_column("a", max_diffs = 3)
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "values",
		expected = letters[2:1],
		actual = letters[1:2],
		location = "column",
		column = "a"
	)
})

test_that("checks that columns have the same length", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:3])
		.solution <- tibble::tibble(a = letters[1:4])
		tbl_grade_column("a")
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "length",
		expected = letters[1:4],
		actual = letters[1:3],
		expected_length = 4,
		actual_length = 3,
		location = "column",
		column = "a"
	)
})

test_that("checks that the column is present in object", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(b = letters[1:3])
		.solution <- tibble::tibble(a = letters[1:3])
		tbl_grade_column("a")
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "names",
		missing = "a",
		location = "table"
	)
})

test_that("checks that the column is present in expected", {
	expect_warning(
		{
			grade <- tblcheck_test_grade({
				.result <- tibble::tibble(b = letters[1:3])
				.solution <- tibble::tibble(a = letters[1:3])
				tbl_grade_column("b")
			})
		},
		"`b` is not a column in `expected`"
	)

	expect_null(grade)
})

test_that("tbl_grade_column() with no problems returns invisible()", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:3])
		.solution <- tibble::tibble(a = letters[1:3])
		tbl_grade_column("a")
	})

	expect_null(grade)

	expect_invisible(
		tbl_grade_column("a", tibble::tibble(a = 1), tibble::tibble(a = 1))
	)

	expect_null(grade)
})

test_that("number of levels", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = as.factor(c("a", "b", "b")))
		.solution <- tibble::tibble(a = as.factor(c("a", "b", "c")))
		tbl_grade_column("a")
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "levels_n",
		expected = 3,
		actual = 2,
		location = "column",
		column = "a"
	)
})

test_that("level labels", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = as.factor(c("a", "b", "c")))
		.solution <- tibble::tibble(a = as.factor(c("x", "y", "z")))
		tbl_grade_column("a")
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "levels",
		missing = c("x", "y", "z"),
		unexpected = c("a", "b", "c"),
		location = "column",
		column = "a"
	)
})

test_that("level order", {
	grade_reverse <- tblcheck_test_grade({
		.result <- tibble::tibble(a = as.factor(c("a", "b", "c")))
		.solution <- tibble::tibble(a = factor(c("a", "b", "c"), levels = c("c", "b", "a")))
		tbl_grade_column("a")
	})

	expect_snapshot(grade_reverse)

	expect_problem(
		grade_reverse$problem,
		type = "levels_reversed",
		location = "column",
		column = "a"
	)

	grade_diffs <- tblcheck_test_grade({
		.result <- tibble::tibble(a = factor(1:3, c("a", "b", "c")))
		.solution <- tibble::tibble(a = factor(1:3, c("c", "a", "b")))
		tbl_grade_column("a")
	})

	expect_snapshot(grade_diffs)

	expect_problem(
		grade_diffs$problem,
		type = "levels_order",
		expected = c("c", "a", "b"),
		actual = c("a", "b", "c"),
		location = "column",
		column = "a"
	)

	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = as.factor(c("a", "b", "c", "d", "e")))
		.solution <- tibble::tibble(a = factor(c("a", "b", "c", "d", "e"), levels = c("a", "b", "c", "e", "d")))
		problem <- tbl_check_column("a")
		tbl_grade_column("a")
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "levels_order",
		expected = c("a", "b", "c", "e", "d"),
		actual = c("a", "b", "c", "d", "e"),
		location = "column",
		column = "a"
	)
})

test_that("tbl_grade_column() handles bad user input", {
	expect_internal_problem(
		tblcheck_test_grade({
			result <- solution <- tibble::tibble(b = letters[1:3])
			tbl_grade_column(3, object = result, expected = solution)
		}),
		"column"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			result <- solution <- tibble::tibble(b = letters[1:3])
			tbl_grade_column(c("a", "b"), object = result, expected = solution)
		}),
		"column"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			result <- solution <- tibble::tibble(b = letters[1:3])
			tbl_grade_column("b", object = result, expected = solution, check_class = "yes")
		}),
		"check_class"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			result <- solution <- tibble::tibble(b = letters[1:3])
			tbl_grade_column("b", object = result, expected = solution, check_length = c(TRUE, TRUE))
		}),
		"check_length"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			result <- solution <- tibble::tibble(b = letters[1:3])
			tbl_grade_column("b", object = result, expected = solution, check_values = "yes")
		}),
		"check_values"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			result <- tibble::tibble(b = letters[1:3])
			solution <- tibble::tibble(b = letters[4:6])
			tbl_grade_column("b", object = result, expected = solution, max_diffs = 1:3)
		}),
		"max_diffs"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			result <- solution <- tibble::tibble(b = letters[1:3])
			tbl_grade_column("b", object = 12, expected = solution)
		}),
		"object"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			result <- solution <- tibble::tibble(b = letters[1:3])
			tbl_grade_column("b", object = result, expected = list(a = 1))
		}),
		"expected"
	)
})

test_that("tbl_check_column() handles bad user input", {
	.result <- .solution <- tibble::tibble(b = letters[1:3])

	expect_internal_problem(
		tbl_check_column(3),
		message = "column"
	)

	expect_internal_problem(
		tbl_check_column(c("a", "b")),
		message = "column"
	)

	expect_internal_problem(
		tbl_check_column("b", check_class = "yes"),
		message = "check_class"
	)

	expect_internal_problem(
		tbl_check_column("b", check_values = "yes"),
		"check_values"
	)

	expect_internal_problem(
		tbl_check_column("b", object = 12),
		"object"
	)

	expect_internal_problem(
		tbl_check_column("b", check_length = c(TRUE, TRUE)),
		message = "check_length"
	)

	expect_internal_problem(
		tbl_check_column("b", check_values = NULL),
		message = "check_values"
	)
})
