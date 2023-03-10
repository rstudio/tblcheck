test_that("grade missing groups", {
	grade_ungrouped <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:3], b = a)
		.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
		tbl_grade_groups()
	})

	expect_snapshot(grade_ungrouped)

	expect_problem(
		grade_ungrouped$problem,
		type = "groups",
		missing = "b",
		unexpected = character(0),
		location = "table"
	)

	grade_grouped <- tblcheck_test_grade({
		.result <- dplyr::group_by(
			tibble::tibble(a = letters[1:3], b = a, c = a), a
		)
		.solution <- dplyr::group_by(
			tibble::tibble(a = letters[1:3], b = a, c = a), a, b, c
		)
		tbl_grade_groups()
	})

	expect_snapshot(grade_grouped)

	expect_problem(
		grade_grouped$problem,
		type = "groups",
		missing = c("b", "c"),
		unexpected = character(0),
		location = "table"
	)
})

test_that("grade unexpected groups", {
	grade_single <- tblcheck_test_grade({
		.result <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
		.solution <- tibble::tibble(a = letters[1:3], b = a)
		tbl_grade_groups()
	})

	expect_snapshot(grade_single)

	expect_problem(
		grade_single$problem,
		type = "groups",
		missing = character(0),
		unexpected = "b",
		location = "table"
	)

	grade_multiple <- tblcheck_test_grade({
		.result <- dplyr::group_by(
			tibble::tibble(a = letters[1:3], b = a, c = a), a, b, c
		)
		.solution <- dplyr::group_by(
			tibble::tibble(a = letters[1:3], b = a, c = a), a
		)
		tbl_grade_groups()
	})

	expect_snapshot(grade_multiple)

	expect_problem(
		grade_multiple$problem,
		type = "groups",
		missing = character(0),
		unexpected = c("b", "c"),
		location = "table"
	)
})

test_that("grade missing and unexpected groups", {
	grade <- tblcheck_test_grade({
		.result <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), a)
		.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
		tbl_grade_groups()
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

test_that("grade groups max_diffs()", {
	grade <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
		.solution <- dplyr::group_by(
			tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
		)
		tbl_grade_groups()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "groups",
		missing = c("a", "b", "c", "d"),
		unexpected = character(0),
		location = "table"
	)

	grade_inf <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
		.solution <- dplyr::group_by(
			tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
		)
		tbl_grade_groups(max_diffs = Inf)
	})

	expect_snapshot(grade_inf)

	expect_problem(
		grade_inf$problem,
		type = "groups",
		missing = c("a", "b", "c", "d"),
		unexpected = character(0),
		location = "table"
	)

	grade_one <- tblcheck_test_grade({
		.result <- tibble::tibble(a = letters[1:3], b = a, c = a, d = a)
		.solution <- dplyr::group_by(
			tibble::tibble(a = letters[1:3], b = a, c = a, d = a), a, b, c, d
		)
		tbl_grade_groups(max_diffs = 1)
	})

	expect_snapshot(grade_one)

	expect_problem(
		grade_one$problem,
		type = "groups",
		missing = c("a", "b", "c", "d"),
		unexpected = character(0),
		location = "table"
	)
})

test_that("tbl_grade_groups() with no problems returns invisible()", {
	.result <- tibble::tibble(a = letters[1:3], b = a)
	.solution <- tibble::tibble(a = letters[1:3], b = a)

	grade <- expect_invisible(tbl_grade_groups())
	expect_null(grade)

	problem <- expect_invisible(tbl_check_groups())
	expect_null(problem)

	.result <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
	.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)

	grade <- expect_invisible(tbl_grade_groups())
	expect_null(grade)

	problem <- expect_invisible(tbl_check_groups())
	expect_null(problem)
})

test_that("tbl_grade_groups() handles bad user input", {
	expect_internal_problem(
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters[1:3], b = a)
			.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
			tbl_grade_groups(max_diffs = "a")
		}),
		"max_diffs"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters[1:3], b = a)
			.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
			tbl_grade_groups(max_diffs = -1)
		}),
		"max_diffs"
	)

	expect_internal_problem(
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters[1:3], b = a)
			.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
			tbl_grade_groups(max_diffs = 1:2)
		}),
		"max_diffs"
	)
})
