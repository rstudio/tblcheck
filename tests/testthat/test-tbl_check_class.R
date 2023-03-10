test_that("tbl_grade_class()", {
	grade_num_chr_1 <-
		tblcheck_test_grade({
			.result <- "1"
			.solution <- 1
			tbl_grade_class()
		})

	expect_snapshot(grade_num_chr_1)

	expect_problem(
		grade_num_chr_1$problem,
		type = "class",
		expected = 1,
		actual = "1"
	)

	grade_num_chr_2 <-
		tblcheck_test_grade({
			.result <- c("1", "2")
			.solution <- c(1, 2)
			tbl_grade_class()
		})

	expect_snapshot(grade_num_chr_2)

	expect_problem(
		grade_num_chr_2$problem,
		type = "class",
		expected = c(1, 2),
		actual = c("1", "2")
	)

	grade_posixct_1 <-
		tblcheck_test_grade({
			.result <- "2021-07-29 10:59:59"
			.solution <- as.POSIXct("2021-07-29 10:59:59")
			tbl_grade_class()
		})

	expect_snapshot(grade_posixct_1)

	expect_problem(
		grade_posixct_1$problem,
		type = "class",
		expected = as.POSIXct("2021-07-29 10:59:59"),
		actual = "2021-07-29 10:59:59"
	)

	grade_posixct_2 <-
		tblcheck_test_grade({
			.result <- c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
			.solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
			tbl_grade_class()
		})

	expect_snapshot(grade_posixct_2)

	expect_problem(
		grade_posixct_2$problem,
		type = "class",
		expected = as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00")),
		actual = c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
	)
})

test_that("tbl_grade_class() ignore classes", {
	grade_glue_chr <-
		tblcheck_test_grade({
			.result <- glue::glue("x")
			.solution <- "x"
			tbl_grade_class()
		})

	expect_snapshot(grade_glue_chr)

	expect_problem(
		grade_glue_chr$problem,
		type = "class",
		expected = "x",
		actual = glue::glue("x")
	)

	grade_glue_chr_ignore <-
		tblcheck_test_grade({
			.result <- glue::glue("x")
			.solution <- "x"
			tbl_grade_class(ignore_class = "glue")
		})

	expect_null(grade_glue_chr_ignore)

	grade_tbl_df <-
		tblcheck_test_grade({
			.result <- data.frame(a = 1, b = 2)
			.solution <- tibble::tibble(a = 1, b = 2)
			tbl_grade_class()
		})

	expect_snapshot(grade_tbl_df)

	expect_problem(
		grade_tbl_df$problem,
		type = "class",
		expected = tibble::tibble(a = 1, b = 2),
		actual = data.frame(a = 1, b = 2)
	)

	grade_tbl_df_ignore <-
		tblcheck_test_grade({
			.result <- data.frame(a = 1, b = 2)
			.solution <- tibble::tibble(a = 1, b = 2)
			tbl_grade_class(ignore_class = c("tbl_df", "tbl"))
		})

	expect_null(grade_tbl_df_ignore)
})

test_that("tbl_grade_class() with paired ignore_class", {
	grade_int_dbl <-
		tblcheck_test_grade({
			.result <- 1L
			.solution <- 1
			tbl_grade_class()
		})

	expect_snapshot(grade_int_dbl)

	expect_problem(
		grade_int_dbl$problem,
		type = "class",
		expected = 1,
		actual = 1L
	)

	grade_int_dbl_ignore <-
		tblcheck_test_grade({
			.result <- 1L
			.solution <- 1
			tbl_grade_class(ignore_class = c("integer" = "numeric"))
		})

	expect_null(grade_int_dbl_ignore)

	grade_int_chr_wrong_ignore <-
		tblcheck_test_grade({
			.result <- "1"
			.solution <- 1
			tbl_grade_class(ignore_class = c("integer" = "numeric"))
		})

	expect_snapshot(grade_int_chr_wrong_ignore)

	expect_problem(
		grade_int_chr_wrong_ignore$problem,
		type = "class",
		expected = 1,
		actual = "1"
	)

	grade_posix_ct_lt <-
		tblcheck_test_grade({
			.result <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
			.solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
			tbl_grade_class()
		})

	expect_snapshot(grade_posix_ct_lt)

	expect_problem(
		grade_posix_ct_lt$problem,
		type = "class",
		expected = as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00")),
		actual = as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
	)

	grade_posix_ct_lt_ignore <-
		tblcheck_test_grade({
			.result <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
			.solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
			tbl_grade_class(ignore_class = c("POSIXct" = "POSIXlt"))
		})

	expect_null(grade_posix_ct_lt_ignore)
})

test_that("tbl_grade_class() with multiple paired ignore_class", {
	grade_int_dbl_ignore <-
		tblcheck_test_grade({
			.result <- 1L
			.solution <- 1
			tbl_grade_class(
				ignore_class = c("numeric" = "integer", "character" = "numeric")
			)
		})

	expect_null(grade_int_dbl_ignore)
})

test_that("tbl_grade_class() with ignore_class leaving NULL solution class", {
	grade_int_dbl_ignore <-
		tblcheck_test_grade({
			.result <- 1L
			.solution <- 1
			tbl_grade_class(ignore_class = c("numeric"))
		})

	expect_null(grade_int_dbl_ignore)

	grade_chr_dbl_ignore <-
		tblcheck_test_grade({
			.result <- "1"
			.solution <- 1
			tbl_grade_class(ignore_class = c("numeric"))
		})

	expect_null(grade_int_dbl_ignore)

	grade_tbl_dbl_ignore <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = 1)
			.solution <- 1
			tbl_grade_class(ignore_class = c("numeric"))
		})

	expect_null(grade_int_dbl_ignore)
})

test_that("tbl_grade_class() with multiple classes", {
	grade_class_solution <-
		tblcheck_test_grade({
			.result <- 1L
			.solution <- 1L
			class(.solution) <- c("test", "class", "integer")
			tbl_grade_class()
		})

	expect_snapshot(grade_class_solution)

	.solution <- 1L
	class(.solution) <- c("test", "class", "integer")

	expect_problem(
		grade_class_solution$problem,
		type = "class",
		expected = .solution,
		actual = 1L
	)

	grade_class_result <-
		tblcheck_test_grade({
			.result <- 1L
			class(.result) <- c("test", "class", "integer")
			.solution <- 1L
			tbl_grade_class()
		})

	expect_snapshot(grade_class_result)

	.result <- 1L
	class(.result) <- c("test", "class", "integer")

	expect_problem(
		grade_class_result$problem,
		type = "class",
		expected = 1L,
		actual = .result
	)
})

test_that("tbl_grade_class() with classes in different orders", {
	grade <-
		tblcheck_test_grade({
			.result <- 1L
			.solution <- 1L
			class(.result) <- c("test", "class", "integer")
			class(.solution) <- c("class", "test", "integer")
			tbl_grade_class()
		})

	expect_null(grade)
})
