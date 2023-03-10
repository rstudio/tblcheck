test_that("tbl_grade_class() with hinted messages", {
	grade_ungrouped <- tblcheck_test_grade({
		.result   <- tibble::tibble(a = letters[1:3], b = a)
		.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
		tbl_grade_class()
	})

	expect_snapshot(grade_ungrouped)

	expect_equal(
		grade_ungrouped$problem,
		problem(
			"class",
			expected = dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b),
			actual = tibble::tibble(a = letters[1:3], b = a)
		),
		ignore_attr = "class"
	)

	grade_grouped <- tblcheck_test_grade({
		.result   <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
		.solution <- tibble::tibble(a = letters[1:3], b = a)
		tbl_grade_class()
	})

	expect_snapshot(grade_grouped)

	expect_equal(
		grade_grouped$problem,
		problem(
			"class",
			expected = tibble::tibble(a = letters[1:3], b = a),
			actual = dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
		),
		ignore_attr = "class"
	)

	grade_ungrouped_int <- tblcheck_test_grade({
		.result   <- 1:2
		.solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
		tbl_grade_class()
	})

	expect_snapshot(grade_ungrouped_int)

	expect_equal(
		grade_ungrouped_int$problem,
		problem(
			"class",
			expected = dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b),
			actual = 1:2
		),
		ignore_attr = "class"
	)

	grade_unrowwise_int <- tblcheck_test_grade({
		.result   <- 1:2
		.solution <- dplyr::rowwise(tibble::tibble(a = letters[1:3], b = a))
		tbl_grade_class()
	})

	expect_snapshot(grade_unrowwise_int)

	expect_equal(
		grade_unrowwise_int$problem,
		problem(
			"class",
			expected = dplyr::rowwise(tibble::tibble(a = letters[1:3], b = a)),
			actual = 1:2
		),
		ignore_attr = "class"
	)
})
