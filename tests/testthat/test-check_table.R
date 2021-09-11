test_that("tbl_grade_table() class", {
  grade_tbl_class_df <-
    tblcheck_test_grade({
    .result   <- data.frame(a = 1:10, b = 1:10)
    .solution <- tibble::tibble(a = 1:10, b = 1:10)
    tbl_grade_table()
  })
  
  expect_snapshot(grade_tbl_class_df)
  
  expect_equal(
    grade_tbl_class_df$problem,
    problem(
      "table_class",
      c("tbl_df", "tbl", "data.frame"),
      "data.frame",
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )
  
  grade_tbl_class_grouped <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = 1:10, b = a)
    .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
    tbl_grade_table()
  })
  
  expect_snapshot(grade_tbl_class_grouped)
  
  expect_equal(
    grade_tbl_class_grouped$problem,
    problem(
      "table_class",
      c("grouped_df", "tbl_df", "tbl", "data.frame"),
      c("tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )
  
  grade_tbl_class_rowwise <-
    tblcheck_test_grade({
    .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
    .solution <- tibble::tibble(a = 1:10, b = a)
    tbl_grade_table()
  })
  
  expect_snapshot(grade_tbl_class_rowwise)
  
  expect_equal(
    grade_tbl_class_rowwise$problem,
    problem(
      "table_class",
      c("tbl_df", "tbl", "data.frame"),
      c("rowwise_df", "tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )
  
  grade_tbl_class_grouped_rowwise <-
    tblcheck_test_grade({
    .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
    .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
    tbl_grade_table()
  })
  
  expect_snapshot(grade_tbl_class_grouped_rowwise)
  
  expect_equal(
    grade_tbl_class_grouped_rowwise$problem,
    problem(
      "table_class",
      c("grouped_df", "tbl_df", "tbl", "data.frame"),
      c("rowwise_df", "tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_table() rows", {
  grade_tbl_rows_missing_1 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a)
    .solution <- tibble::tibble(a = letters[-1], b = a)
    tbl_grade_table()
  })
  
  expect_snapshot(grade_tbl_rows_missing_1)
  
  expect_equal(
    grade_tbl_rows_missing_1$problem,
    problem("table_nrow", 25, 26),
    ignore_attr = "class"
  )
  
  grade_tbl_rows_extra_1 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a)
    .solution <- tibble::tibble(a = letters[1], b = a)
    tbl_grade_table()
  })
  
  expect_snapshot(grade_tbl_rows_extra_1)
  
  expect_equal(
    grade_tbl_rows_extra_1$problem,
    problem("table_nrow", 1, 26),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_table() ncol", {
  grade_tbl_cols_extra_1 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a)
    .solution <- tibble::tibble(a = letters, b = a)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_snapshot(grade_tbl_cols_extra_1)
  
  expect_equal(
    grade_tbl_cols_extra_1$problem,
    problem("table_ncol", 2, 3),
    ignore_attr = "class"
  )
  
  grade_tbl_cols_extra_2 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a)
    .solution <- tibble::tibble(a = letters)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_snapshot(grade_tbl_cols_extra_2)
  
  expect_equal(
    grade_tbl_cols_extra_2$problem,
    problem("table_ncol", 1, 3),
    ignore_attr = "class"
  )
  
  grade_tbl_cols_missing_1 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = letters, b = a)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_snapshot(grade_tbl_cols_missing_1)
  
  expect_equal(
    grade_tbl_cols_missing_1$problem,
    problem("table_ncol", 2, 1),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_table() names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(x = letters[1:3], y = x)
    tbl_grade_table()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "table_names",
      missing = c("x", "y"),
      unexpected = c("a", "b")
    ),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_table() columns", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[24:26])
    tbl_grade_table()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("column_value_diffs", letters[24:26], column = "a"),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_table() with no problems returns invisible()", {
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  problem <- expect_invisible(
    tbl_check_table(object = .solution, expected = .solution)
  )
  
  grade <- expect_invisible(
    tbl_grade_table(object = .solution, expected = .solution)
  )
  
  expect_null(problem)
  expect_null(grade)
})

test_that("tbl_grade_table() returns grades with row problems", {
  grade_rows_extra <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = letters[1:25])
    tbl_grade_table()
  })
  
  expect_snapshot(grade_rows_extra)
  
  expect_equal(
    grade_rows_extra$problem,
    problem("table_nrow", 25, 26),
    ignore_attr = "class"
  )
  
  grade_rows_missing <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = letters[1])
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_snapshot(grade_rows_missing)
  
  expect_equal(
    grade_rows_missing$problem,
    problem("table_nrow", 1, 26),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_table() returns names feedback to learnr", {
  grade_tbl_names_3 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    tbl_grade_table(max_diffs = 3)
  })
  
  expect_snapshot(grade_tbl_names_3)
  
  expect_equal(
    grade_tbl_names_3$problem,
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )
  
  # ---- with all diffs ---
  grade_tbl_names_inf <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    problem   <- tbl_check_table(max_diffs = Inf)
    tbl_grade_table(max_diffs = Inf)
  })
  
  expect_snapshot(grade_tbl_names_inf)
  
  expect_equal(
    grade_tbl_names_inf$problem,
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )
  
  # ---- with one diff ---
  grade_tbl_names_1 <-
    tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    tbl_grade_table(max_diffs = 1)
  })
  
  expect_snapshot(grade_tbl_names_1)
  
  expect_equal(
    grade_tbl_names_1$problem,
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )
})

test_that("number of levels", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = as.factor(c("a", "b", "b")))
    .solution <- tibble::tibble(a = as.factor(c("a", "b", "c")))
    tbl_grade_table()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("column_n_levels", 3, 2, column = "a"),
    ignore_attr = "class"
  )
})

test_that("level labels", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = as.factor(c("a", "b", "c")))
    .solution <- tibble::tibble(a = as.factor(c("x", "y", "z")))
    tbl_grade_table()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "column_levels",
      missing = c("x", "y", "z"),
      unexpected = c("a", "b", "c"),
      column = "a"
    ),
    ignore_attr = "class"
  )
})

test_that("level order", {
  grade_reverse <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = as.factor(c("a", "b", "c")))
    .solution <- tibble::tibble(a = factor(c("a", "b", "c"), levels = c("c", "b", "a")))
    tbl_grade_table()
  })
  
  expect_snapshot(grade_reverse)
  
  expect_equal(
    grade_reverse$problem,
    problem("column_reverse_levels", column = "a"),
    ignore_attr = "class"
  )
  
  grade_diffs <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = factor(1:3, c("a", "b", "c")))
    .solution <- tibble::tibble(a = factor(1:3, c("c", "a", "b")))
    tbl_grade_table()
  })
  
  expect_snapshot(grade_diffs)
  
  expect_equal(
    grade_diffs$problem,
    problem(
      "column_level_order_diffs",
      expected = c("c", "a", "b"),
      actual = c("a", "b", "c"),
      column = "a"
    ),
    ignore_attr = "class"
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = as.factor(c("a", "b", "c", "d", "e")))
    .solution <- tibble::tibble(a = factor(c("a", "b", "c", "d", "e"), levels = c("a", "b", "c", "e", "d")))
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("column_level_order", column = "a"),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_table() groups", {
  grade <- tblcheck_test_grade({
    .result   <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), a)
    .solution <- dplyr::group_by(tibble::tibble(a = letters[1:3], b = a), b)
    tbl_grade_table()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("table_groups", missing = "b", unexpected = "a"),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_table() handles bad user input", {
  expect_internal_problem(
    tblcheck_test_grade({
      solution <- result <- tibble::tibble(a = 1:3)
      tbl_grade_table(object = result, expected = solution, check_dimensions = "yes")
    }),
    "check_dimensions"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      solution <- result <- tibble::tibble(a = 1:3)
      tbl_grade_table(object = result, expected = solution, check_names = 5)
    }),
    "check_names"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      solution <- result <- tibble::tibble(a = 1:3)
      tbl_grade_table(object = result, expected = solution, check_columns = NULL)
    }),
    "check_columns"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      solution <- result <- tibble::tibble(a = 1:3)
      tbl_grade_table(object = result, expected = solution, check_class = NA)
    }),
    "check_class"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      solution <- result <- tibble::tibble(a = 1:3)
      tbl_grade_table(object = result, expected = solution, check_column_values = c(TRUE, TRUE))
    }),
    "check_column_values"
  )
})

test_that("tbl_check_table() handles bad user input", {
  .result <- .solution <- tibble::tibble(a = 1:3)
  
  expect_message(
    problem <- tbl_check_table(check_dimensions = "yes"),
    "check_dimensions"
  )
  testthat::expect_s3_class(problem, "tblcheck_internal_problem")
  testthat::expect_match(problem$error, "check_dimensions")
  
  expect_message(
    problem <- tbl_check_table(check_names = 5),
    "check_names"
  )
  testthat::expect_s3_class(problem, "tblcheck_internal_problem")
  testthat::expect_match(problem$error, "check_names")
  
  expect_message(
    problem <- tbl_check_table(check_columns = NULL),
    "check_columns"
  )
  testthat::expect_s3_class(problem, "tblcheck_internal_problem")
  testthat::expect_match(problem$error, "check_columns")
  
  expect_message(
    problem <- tbl_check_table(check_class = NA),
    "check_class"
  )
  testthat::expect_s3_class(problem, "tblcheck_internal_problem")
  testthat::expect_match(problem$error, "check_class")
  
  expect_message(
    problem <- tbl_check_table(check_column_values = c(TRUE, TRUE)),
    "check_column_values"
  )
  testthat::expect_s3_class(problem, "tblcheck_internal_problem")
  testthat::expect_match(problem$error, "check_column_values")
})
