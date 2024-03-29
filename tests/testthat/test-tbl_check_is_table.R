test_that("tbl_grade_is_table() with non-tables", {
	grade_list <-
		tblcheck_test_grade({
			.result <- list(a = 1:10)
			tbl_grade_is_table()
		})

	expect_snapshot(grade_list)

	expect_problem(
		grade_list$problem,
		type = "not_table",
		actual = list(a = 1:10)
	)

	grade_vector <-
		tblcheck_test_grade({
			.result <- 1:10
			tbl_grade_is_table()
		})

	expect_snapshot(grade_vector)

	expect_problem(
		grade_vector$problem,
		type = "not_table",
		actual = 1:10
	)
})

test_that("tbl_grade_is_table() with tables", {
	grade_data.frame <-
		tblcheck_test_grade({
			.result <- data.frame(a = 1:10)
			tbl_grade_is_table()
		})

	expect_null(grade_data.frame)

	grade_tibble <-
		tblcheck_test_grade({
			.result <- tibble::tibble(a = 1:10)
			tbl_grade_is_table()
		})

	expect_null(grade_tibble)
})
