test_that("rlang_call_match() with {rlang} > 0.4.12", {
	use_call_match <- function(a = 1, b = 3) {
		rlang_call_match(n = 1)
	}

	expect_equal(
		rlang::expr_text(use_call_match(a = 12)),
		"use_call_match(a = 12, b = 3)"
	)

	expect_equal(
		rlang::expr_text(use_call_match(b = 4)),
		"use_call_match(a = 1, b = 4)"
	)

	expect_equal(
		rlang::expr_text(use_call_match()),
		"use_call_match(a = 1, b = 3)"
	)
})

test_that("rlang_call_match() with {rlang} <= 0.4.12", {
	use_call_match <- function(a = 1, b = 3) {
		rlang_call_match(n = 1)
	}

	# mock usage of older version of rlang
	mockery::stub(rlang_call_match, "has_rlang_version", FALSE)

	expect_equal(
		rlang::expr_text(use_call_match(a = 12)),
		"use_call_match(a = 12, b = 3)"
	)

	expect_equal(
		rlang::expr_text(use_call_match(b = 4)),
		"use_call_match(a = 1, b = 4)"
	)

	expect_equal(
		rlang::expr_text(use_call_match()),
		"use_call_match(a = 1, b = 3)"
	)
})

test_that("grade_this_table() arguments overlap tbl_grade()", {
	tbl_grade_args_exclude <- c("object", "expected", "env", "...")
	tbl_grade_args <- setdiff(names(formals(tbl_grade)), tbl_grade_args_exclude)
	expect_equal(
		intersect(tbl_grade_args, names(formals(grade_this_table))),
		tbl_grade_args
	)
})

test_that("grade_this_vector() arguments overlap vec_grade()", {
	vec_grade_args_exclude <- c("object", "expected", "env", "...")
	vec_grade_args <- setdiff(names(formals(vec_grade)), vec_grade_args_exclude)
	expect_equal(
		intersect(vec_grade_args, names(formals(grade_this_vector))),
		vec_grade_args
	)
})

test_that("grade_this_table()", {
	grade_pass <-
		tblcheck_test_grade({
			grade_this_table(correct = "Correct")(
				gradethis::mock_this_exercise(
					.user_code = "tibble::tibble(a = 1:10, b = 1:10)",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_pass)
	expect_true(grade_pass$correct)
	expect_null(grade_pass$problem)

	grade_class <-
		tblcheck_test_grade({
			grade_this_table()(
				gradethis::mock_this_exercise(
					.user_code = "data.frame(a = 1:10, b = 1:10)",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_class)

	expect_equal(
		grade_class$problem,
		problem(
			"class",
			expected = tibble::tibble(a = 1:10, b = 1:10),
			actual = data.frame(a = 1:10, b = 1:10),
			location = "table"
		),
		ignore_attr = "class"
	)

	grade_pass_default <-
		tblcheck_test_grade({
			grade_this_table(check_class = FALSE, correct = "PASS")(
				gradethis::mock_this_exercise(
					.user_code = "data.frame(a = 1:10, b = 1:10)",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_pass_default)
	expect_true(grade_pass_default$correct)
	expect_match(grade_pass_default$message, "PASS", fixed = TRUE)
	expect_null(grade_pass_default$problem)
})

test_that("grade_this_vector()", {
	grade_pass <-
		tblcheck_test_grade({
			grade_this_vector(correct = "Correct")(
				gradethis::mock_this_exercise(
					.user_code = "1:10", .solution_code = "1:10"
				)
			)
		})

	expect_snapshot(grade_pass)
	expect_true(grade_pass$correct)
	expect_null(grade_pass$problem)

	grade_class <-
		tblcheck_test_grade({
			grade_this_vector()(
				gradethis::mock_this_exercise(
					.user_code = "as.numeric(1:10)", .solution_code = "1:10"
				)
			)
		})

	expect_snapshot(grade_class)

	expect_equal(
		grade_class$problem,
		problem(
			"class",
			expected = 1:10,
			actual = as.numeric(1:10),
			location = "vector"
		),
		ignore_attr = "class"
	)

	grade_pass_default <-
		tblcheck_test_grade({
			grade_this_vector(check_class = FALSE, check_values = FALSE, correct = "PASS")(
				gradethis::mock_this_exercise(
					.user_code = "as.numeric(1:10)", .solution_code = "1:10"
				)
			)
		})

	expect_snapshot(grade_pass_default)
	expect_true(grade_pass_default$correct)
	expect_match(grade_pass_default$message, "PASS", fixed = TRUE)
	expect_null(grade_pass_default$problem)
})

test_that("pre_check setup", {
	grade_pass <-
		tblcheck_test_grade({
			grade_this_vector(
				pre_check = {.result <- .result$b; .solution <- .solution$b},
				correct = "Correct"
			)(
				gradethis::mock_this_exercise(
					.user_code = "tibble::tibble(a = 1:10, b = 1:10)",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_pass)
	expect_true(grade_pass$correct)
	expect_null(grade_pass$problem)

	grade_class <-
		tblcheck_test_grade({
			grade_this_vector(
				pre_check = {.result <- .result$b; .solution <- .solution$b}
			)(
				gradethis::mock_this_exercise(
					.user_code = "tibble::tibble(a = 1:10, b = as.numeric(1:10))",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_class)

	expect_equal(
		grade_class$problem,
		problem(
			"class",
			expected = 1:10,
			actual = as.numeric(1:10),
			location = "vector"
		),
		ignore_attr = "class"
	)

	grade_pass_default <-
		tblcheck_test_grade({
			grade_this_vector(
				pre_check = {.result <- .result$b; .solution <- .solution$b},
				check_class = FALSE, check_values = FALSE,
				correct = "PASS"
			)(
				gradethis::mock_this_exercise(
					.user_code = "tibble::tibble(a = 1:10, b = as.numeric(1:10))",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_pass_default)
	expect_true(grade_pass_default$correct)
	expect_match(grade_pass_default$message, "PASS", fixed = TRUE)
	expect_null(grade_pass_default$problem)
})

test_that("pre_check test", {
	grade_fail <-
		tblcheck_test_grade({
			grade_this_table(
				pre_check = {gradethis::fail_if(is.integer(.result$b), "Incorrect")}
			)(
				gradethis::mock_this_exercise(
					.user_code = "tibble::tibble(a = 1:10, b = 1:10)",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_fail)
	expect_false(grade_fail$correct)
	expect_null(grade_fail$problem)
})

test_that("post_check test", {
	grade_pass <-
		tblcheck_test_grade({
			grade_this_table(
				pass_if_equal = TRUE,
				post_check = {gradethis::fail_if(is.integer(.result$b), "Incorrect")},
				correct = "Correct"
			)(
				gradethis::mock_this_exercise(
					.user_code = "tibble::tibble(a = 1:10, b = 1:10)",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_pass)
	expect_true(grade_pass$correct)
	expect_null(grade_pass$problem)

	grade_fail <-
		tblcheck_test_grade({
			grade_this_table(
				post_check = {gradethis::fail_if(is.integer(.result$b), "Incorrect")},
				check_class = FALSE
			)(
				gradethis::mock_this_exercise(
					.user_code = "data.frame(a = 1:10, b = 1:10)",
					.solution_code = "tibble::tibble(a = 1:10, b = 1:10)"
				)
			)
		})

	expect_snapshot(grade_fail)
	expect_false(grade_fail$correct)
	expect_null(grade_fail$problem)
})
