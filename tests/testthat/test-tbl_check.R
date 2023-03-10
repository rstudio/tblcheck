test_that("tbl_grade() class", {
	grade_tbl_class_df <-
		tblcheck_test_grade({
			.result <- data.frame(a = 1:10, b = 1:10)
			.solution <- tibble::tibble(a = 1:10, b = 1:10)
			tbl_grade()
		})

	expect_snapshot(grade_tbl_class_df)

	expect_problem(
		grade_tbl_class_df$problem,
		type = "class",
		expected = tibble::tibble(a = 1:10, b = 1:10),
		actual = data.frame(a = 1:10, b = 1:10),
		location = "table"
	)

	grade_tbl_class_df_ignore <-
		tblcheck_test_grade({
			.result <- data.frame(a = 1:10, b = 1:10)
			.solution <- tibble::tibble(a = 1:10, b = 1:10)
			tbl_grade(ignore_class = c("tbl_df", "tbl"))
		})

	expect_null(grade_tbl_class_df_ignore)

	grade_tbl_class_grouped <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = 1:10, b = a)
			.solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
			tbl_grade()
		})

	expect_snapshot(grade_tbl_class_grouped)

	expect_problem(
		grade_tbl_class_grouped$problem,
		type = "class",
		expected = dplyr::group_by(tibble::tibble(a = 1:10, b = a), a),
		actual = tibble::tibble(a = 1:10, b = a),
		location = "table"
	)

	grade_tbl_class_grouped_ignore <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = 1:10, b = a)
			.solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
			tbl_grade(check_groups = FALSE)
		})

	expect_null(grade_tbl_class_grouped_ignore)

	grade_tbl_class_rowwise <-
		tblcheck_test_grade({
			.result <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
			.solution <- tibble::tibble(a = 1:10, b = a)
			tbl_grade()
		})

	expect_snapshot(grade_tbl_class_rowwise)

	expect_problem(
		grade_tbl_class_rowwise$problem,
		type = "class",
		expected = tibble::tibble(a = 1:10, b = a),
		actual = dplyr::rowwise(tibble::tibble(a = 1:10, b = a)),
		location = "table"
	)

	grade_tbl_class_grouped_rowwise <-
		tblcheck_test_grade({
			.result <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
			.solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
			tbl_grade()
		})

	expect_snapshot(grade_tbl_class_grouped_rowwise)

	expect_problem(
		grade_tbl_class_grouped_rowwise$problem,
		type = "class",
		expected = dplyr::group_by(tibble::tibble(a = 1:10, b = a), a),
		actual = dplyr::rowwise(tibble::tibble(a = 1:10, b = a)),
		location = "table"
	)
})

test_that("tbl_grade() rows", {
	grade_tbl_rows_missing_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a)
			.solution <- tibble::tibble(a = letters[-1], b = a)
			tbl_grade()
		})

	expect_snapshot(grade_tbl_rows_missing_1)

	expect_problem(
		grade_tbl_rows_missing_1$problem,
		type = "nrow",
		expected = 25,
		actual = 26,
		location = "table"
	)

	grade_tbl_rows_extra_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a)
			.solution <- tibble::tibble(a = letters[1], b = a)
			tbl_grade()
		})

	expect_snapshot(grade_tbl_rows_extra_1)

	expect_problem(
		grade_tbl_rows_extra_1$problem,
		type = "nrow",
		expected = 1,
		actual = 26,
		location = "table"
	)
})

test_that("tbl_grade() ncol", {
	grade_tbl_cols_extra_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a, c = a)
			.solution <- tibble::tibble(a = letters, b = a)
			tbl_grade(check_names = FALSE)
		})

	expect_snapshot(grade_tbl_cols_extra_1)

	expect_problem(
		grade_tbl_cols_extra_1$problem,
		type = "ncol",
		expected = 2,
		actual = 3,
		location = "table"
	)

	grade_tbl_cols_extra_2 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a, c = a)
			.solution <- tibble::tibble(a = letters)
			tbl_grade(check_names = FALSE)
		})

	expect_snapshot(grade_tbl_cols_extra_2)

	expect_problem(
		grade_tbl_cols_extra_2$problem,
		type = "ncol",
		expected = 1,
		actual = 3,
		location = "table"
	)

	grade_tbl_cols_missing_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters)
			.solution <- tibble::tibble(a = letters, b = a)
			tbl_grade(check_names = FALSE)
		})

	expect_snapshot(grade_tbl_cols_missing_1)

	expect_problem(
		grade_tbl_cols_missing_1$problem,
		type = "ncol",
		expected = 2,
		actual = 1,
		location = "table"
	)
})

test_that("tbl_grade() names", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:3], b = a)
		.solution <- tibble::tibble(x = letters[1:3], y = x)
		tbl_grade()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "names",
		missing = c("x", "y"),
		unexpected = c("a", "b"),
		location = "table"
	)
})

test_that("tbl_grade() columns", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:3])
		.solution <- tibble::tibble(a = letters[24:26])
		tbl_grade()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "values",
		expected = letters[24:26],
		actual = letters[1:3],
		location = "column",
		column = "a"
	)

	grade_int <- tblcheck_test_grade({
		.result <- tibble::tibble(a = c(1, 2, 3))
		.solution <- tibble::tibble(a = 1:3)
		tbl_grade()
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
		tbl_grade(ignore_class = c("integer" = "numeric"))
	})

	expect_null(grade_int_ignore)

	grade_tolerant <- tblcheck_test_grade({
		.result <- tibble::tibble(a = sqrt(2) ^ 2)
		.solution <- tibble::tibble(a = 2)
		tbl_grade()
	})

	expect_null(grade_tolerant)

	grade_intolerant <- tblcheck_test_grade({
		.result <- tibble::tibble(a = sqrt(2) ^ 2)
		.solution <- tibble::tibble(a = 2)
		tbl_grade(tolerance = 0)
	})

	expect_snapshot(grade_intolerant)

	expect_problem(
		grade_intolerant$problem,
		type = "values",
		expected = 2,
		actual = sqrt(2) ^ 2,
		location = "column",
		column = "a"
	)
})

test_that("tbl_grade() cols", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = 1:10, intermediate = 6:15, b = 11:20)
		.solution <- tibble::tibble(a = 1:10, b = 11:20)
		tbl_grade()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "names",
		missing = character(0),
		unexpected = "intermediate",
		location = "table"
	)

	check <- tblcheck_test_grade({
		.result <- tibble::tibble(a = 1:10, intermediate = 6:15, b = 11:20)
		.solution <- tibble::tibble(a = 1:10, b = 11:20)
		tbl_check()
	})

	expect_problem(
		check,
		type = "names",
		missing = character(0),
		unexpected = "intermediate",
		location = "table"
	)

	grade_cols <- tblcheck_test_grade({
		.result <- tibble::tibble(a = 1:10, intermediate = 6:15, b = 11:20)
		.solution <- tibble::tibble(a = 1:10, b = 11:20)
		tbl_grade(cols = any_of(names(.solution)))
	})

	expect_null(grade_cols)

	check_cols <- tblcheck_test_grade({
		.result <- tibble::tibble(a = 1:10, intermediate = 6:15, b = 11:20)
		.solution <- tibble::tibble(a = 1:10, b = 11:20)
		tbl_check(cols = any_of(names(.solution)))
	})

	expect_null(grade_cols)
})

test_that("tbl_grade() with no problems returns invisible()", {
	.solution <- tibble::tibble(a = letters[1:3], b = a, c = a)

	problem <- expect_invisible(
		tbl_check(object = .solution, expected = .solution)
	)

	grade <- expect_invisible(
		tbl_grade(object = .solution, expected = .solution)
	)

	expect_null(problem)
	expect_null(grade)
})

test_that("tbl_grade() returns grades with row problems", {
	grade_rows_extra <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters)
			.solution <- tibble::tibble(a = letters[1:25])
			tbl_grade()
		})

	expect_snapshot(grade_rows_extra)

	expect_problem(
		grade_rows_extra$problem,
		type = "nrow",
		expected = 25,
		actual = 26,
		location = "table"
	)

	grade_rows_missing <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters)
			.solution <- tibble::tibble(a = letters[1])
			problem <- tbl_check()
			tbl_grade()
		})

	expect_snapshot(grade_rows_missing)

	expect_problem(
		grade_rows_missing$problem,
		type = "nrow",
		expected = 1,
		actual = 26,
		location = "table"
	)
})

test_that("tbl_grade() returns names feedback to learnr", {
	grade_tbl_names_3 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a, c = a, d = a)
			.solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
			tbl_grade(max_diffs = 3)
		})

	expect_snapshot(grade_tbl_names_3)

	expect_problem(
		grade_tbl_names_3$problem,
		type = "names",
		missing = c("x", "y", "z", "w"),
		unexpected = c("a", "b", "c", "d"),
		location = "table"
	)

	# ---- with all diffs ---
	grade_tbl_names_inf <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a, c = a, d = a)
			.solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
			tbl_grade(max_diffs = Inf)
		})

	expect_snapshot(grade_tbl_names_inf)

	expect_problem(
		grade_tbl_names_inf$problem,
		type = "names",
		missing = c("x", "y", "z", "w"),
		unexpected = c("a", "b", "c", "d"),
		location = "table"
	)

	# ---- with one diff ---
	grade_tbl_names_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a, c = a, d = a)
			.solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
			tbl_grade(max_diffs = 1)
		})

	expect_snapshot(grade_tbl_names_1)

	expect_problem(
		grade_tbl_names_1$problem,
		type = "names",
		missing = c("x", "y", "z", "w"),
		unexpected = c("a", "b", "c", "d"),
		location = "table"
	)
})

test_that("number of levels", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = as.factor(c("a", "b", "b")))
		.solution <- tibble::tibble(a = as.factor(c("a", "b", "c")))
		tbl_grade()
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
		tbl_grade()
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
		tbl_grade()
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
		tbl_grade()
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
		problem <- tbl_check()
		tbl_grade()
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

test_that("tbl_grade() groups", {
	grade <- tblcheck_test_grade({
		.result <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), a)
		.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
		tbl_grade()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "groups",
		missing = "b",
		unexpected = "a",
		location = "table"
	)
})

test_that("tbl_grade() handles bad user input", {
	expect_internal_problem(
		tblcheck_test_grade({
			solution <- result <- tibble::tibble(a = 1:3)
			tbl_grade(object = result, expected = solution, check_dimensions = "yes")
		}),
		"check_dimensions"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			solution <- result <- tibble::tibble(a = 1:3)
			tbl_grade(object = result, expected = solution, check_names = 5)
		}),
		"check_names"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			solution <- result <- tibble::tibble(a = 1:3)
			tbl_grade(object = result, expected = solution, check_columns = NULL)
		}),
		"check_columns"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			solution <- result <- tibble::tibble(a = 1:3)
			tbl_grade(object = result, expected = solution, check_class = NA)
		}),
		"check_class"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			solution <- result <- tibble::tibble(a = 1:3)
			tbl_grade(object = result, expected = solution, check_column_values = c(TRUE, TRUE))
		}),
		"check_column_values"
	)
})

test_that("tbl_check() handles bad user input", {
	.result <- .solution <- tibble::tibble(a = 1:3)

	expect_internal_problem(
		tbl_check(check_dimensions = "yes"),
		message = "check_dimensions"
	)

	expect_internal_problem(
		tbl_check(check_names = 5),
		message = "check_names"
	)

	expect_internal_problem(
		tbl_check(check_columns = NULL),
		message = "check_columns"
	)

	expect_internal_problem(
		tbl_check(check_class = NA),
		message = "check_class"
	)

	expect_internal_problem(
		tbl_check(check_column_values = c(TRUE, TRUE)),
		message = "check_column_values"
	)
})
