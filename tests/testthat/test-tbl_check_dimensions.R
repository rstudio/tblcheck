test_that("vector length", {
	grade <- tblcheck_test_grade({
		.result <- letters[1:3]
		.solution <- letters[1:6]
		vec_grade_dimensions()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "length",
		expected = letters[1:6],
		actual = letters[1:3],
		expected_length = 6,
		actual_length = 3
	)

	grade <- tblcheck_test_grade({
		.result <- letters[1:3]
		.solution <- letters[1:5]
		vec_grade_dimensions()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "length",
		expected = letters[1:5],
		actual = letters[1:3],
		expected_length = 5,
		actual_length = 3
	)

	grade <- tblcheck_test_grade({
		.result <- letters[1:3]
		.solution <- letters[1:4]
		vec_grade_dimensions()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "length",
		expected = letters[1:4],
		actual = letters[1:3],
		expected_length = 4,
		actual_length = 3
	)

	grade <- tblcheck_test_grade({
		.result <- letters[1:3]
		.solution <- letters[4:7]
		vec_grade_dimensions()
	})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "length",
		expected = letters[4:7],
		actual = letters[1:3],
		expected_length = 4,
		actual_length = 3
	)

	grade_no_unique <- tblcheck_test_grade({
		.result <- rep("a", 3)
		.solution <- rep("a", 4)
		vec_grade_dimensions()
	})

	expect_snapshot(grade_no_unique)

	expect_problem(
		grade_no_unique$problem,
		type = "length",
		expected = rep("a", 4),
		actual = rep("a", 3),
		expected_length = 4,
		actual_length = 3
	)
})

test_that("table rows", {
	grade_tbl_rows_missing_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a)
			.solution <- tibble::tibble(a = letters[-1], b = a)
			tbl_grade_dimensions()
		})

	expect_snapshot(grade_tbl_rows_missing_1)

	expect_problem(
		grade_tbl_rows_missing_1$problem,
		type = "nrow",
		expected = 25,
		actual = 26
	)

	grade_tbl_rows_extra_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a)
			.solution <- tibble::tibble(a = letters[1], b = a)
			tbl_grade_dimensions()
		})

	expect_snapshot(grade_tbl_rows_extra_1)

	expect_problem(
		grade_tbl_rows_extra_1$problem,
		type = "nrow",
		expected = 1,
		actual = 26
	)
})

test_that("tbl_grade_dimensions() ncol", {
	grade_tbl_cols_extra_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a, c = a)
			.solution <- tibble::tibble(a = letters, b = a)
			tbl_grade_dimensions()
		})

	expect_snapshot(grade_tbl_cols_extra_1)

	expect_problem(
		grade_tbl_cols_extra_1$problem,
		type = "ncol",
		expected = 2,
		actual = 3
	)

	grade_tbl_cols_extra_2 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters, b = a, c = a)
			.solution <- tibble::tibble(a = letters)
			tbl_grade_dimensions()
		})

	expect_snapshot(grade_tbl_cols_extra_2)

	expect_problem(
		grade_tbl_cols_extra_2$problem,
		type = "ncol",
		expected = 1,
		actual = 3
	)

	grade_tbl_cols_missing_1 <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = letters)
			.solution <- tibble::tibble(a = letters, b = a)
			tbl_grade_dimensions()
		})

	expect_snapshot(grade_tbl_cols_missing_1)

	expect_problem(
		grade_tbl_cols_missing_1$problem,
		type = "ncol",
		expected = 2,
		actual = 1
	)
})

test_that("mismatched dimensions", {
	grade <-
		tblcheck_test_grade({
			.result <- 1:12
			.solution <- matrix(1:12, 3)
			tbl_grade_dimensions()
		})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "dimensions_n",
		expected = 2,
		actual = 1
	)
})

test_that("multidimensional array", {
	grade <-
		tblcheck_test_grade({
			.result <- array(1:12, c(1, 3, 4))
			.solution <- array(1:12, c(2, 2, 3))
			tbl_grade_dimensions()
		})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "dimensions",
		expected = c(2, 2, 3),
		actual = c(1, 3, 4)
	)
})

test_that("matrices", {
	grade <-
		tblcheck_test_grade({
			.result <- matrix(1:12, 3)
			.solution <- matrix(1:12, 4)
			tbl_grade_dimensions()
		})

	expect_snapshot(grade)

	expect_problem(
		grade$problem,
		type = "ncol",
		expected = 3,
		actual = 4
	)

	grade_n <-
		tblcheck_test_grade({
			.result <- 1:12
			.solution <- matrix(1:12, 4)
			tbl_grade_dimensions()
		})

	expect_snapshot(grade_n)

	expect_problem(
		grade_n$problem,
		type = "dimensions_n",
		expected = 2,
		actual = 1
	)
})
