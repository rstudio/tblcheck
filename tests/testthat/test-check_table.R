test_that("tbl_grade_table() class", {
  grade <- tblcheck_test_grade({
    .result   <- data.frame(a = 1:10, b = 1:10)
    .solution <- tibble::tibble(a = 1:10, b = 1:10)
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem(
      "table_class",
      c("tbl_df", "tbl", "data.frame"),
      "data.frame",
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table should be a tibble (class `tbl_df`), but it is a data frame (class `data.frame`)",
    problem = problem,
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = 1:10, b = a)
    .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem(
      "table_class",
      c("grouped_df", "tbl_df", "tbl", "data.frame"),
      c("tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?",
    problem = problem,
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
    .solution <- tibble::tibble(a = 1:10, b = a)
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem(
      "table_class",
      c("tbl_df", "tbl", "data.frame"),
      c("rowwise_df", "tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table is a rowwise data frame, but I wasn't expecting it to be rowwise. Maybe you need to use `ungroup()`?",
    problem = problem,
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
    .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem(
      "table_class",
      c("grouped_df", "tbl_df", "tbl", "data.frame"),
      c("rowwise_df", "tbl_df", "tbl", "data.frame"),
      expected_length = 2,
      actual_length = 2
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table is a rowwise data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?",
    problem = problem,
    fixed = TRUE
  )
})

test_that("tbl_grade_table() rows", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a)
    .solution <- tibble::tibble(a = letters[-1], b = a)
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem("table_nrow", 25, 26),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "should have 25 rows",
    problem = problem
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a)
    .solution <- tibble::tibble(a = letters[1], b = a)
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem("table_nrow", 1, 26),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "should have 1 row",
    problem = problem
  )
})

test_that("tbl_grade_table() ncol", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a)
    .solution <- tibble::tibble(a = letters, b = a)
    problem   <- tbl_check_table(check_names = FALSE)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_equal(
    problem,
    problem("table_ncol", 2, 3),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "should have 2 columns",
    problem = problem
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a)
    .solution <- tibble::tibble(a = letters)
    problem   <- tbl_check_table(check_names = FALSE)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_equal(
    problem,
    problem("table_ncol", 1, 3),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "should have 1 column",
    problem = problem
  )
})

test_that("tbl_grade_table() names", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3], b = a)
    .solution <- tibble::tibble(x = letters[1:3], y = x)
    problem   <- tbl_check_table(object = .result, expected = .solution)
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem(
      "table_names",
      missing = c("x", "y"),
      unexpected = c("a", "b")
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your table should have columns named `x` and `y`. Your table should not have columns named `a` or `b`.",
    problem = problem,
    fixed = TRUE
  )
})

test_that("tbl_grade_table() columns", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[24:26])
    problem   <- tbl_check_table(object = .result, expected = .solution)
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem("column_value_diffs", letters[24:26], column = "a"),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "first 3 values of your `a` column should be `x`, `y`, and `z`",
    problem = problem
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
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = letters[1:25])
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem("table_nrow", 25, 26),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "should have 25 rows",
    problem = problem
  )
  
  grade_single <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = letters[1])
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem,
    problem("table_nrow", 1, 26),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade_single,
    "should have 1 row",
    problem = problem
  )
})

test_that("tbl_grade_table() returns ncol feedback to learnr", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = letters, c = letters)
    .solution <- tibble::tibble(a = letters, b = letters)
    problem   <- tbl_check_table(check_names = FALSE)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_equal(
    problem,
    problem("table_ncol", 2, 3),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "should have 2 columns",
    problem = problem
  )
  
  grade_one <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = letters, c = letters)
    .solution <- tibble::tibble(a = letters)
    problem   <- tbl_check_table(check_names = FALSE)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_equal(
    problem,
    problem("table_ncol", 1, 3),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade_one,
    "should have 1 column",
    problem = problem
  )
})

test_that("tbl_grade_table() returns names feedback to learnr", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    problem   <- tbl_check_table()
    tbl_grade_table()
  })
  
  expect_equal(
    problem, 
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )

  expect_grade(
    grade, 
    "Your table should have columns named .*x.*, .*y.*, .*z.*, and 1 more",
    problem = problem
  )
  
  expect_grade(
    grade,
    "Your table should not have columns named .*a.*, .*b.*, .*c.*, or 1 more"
  )
  
  # ---- with all diffs ---
  grade_inf <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    problem   <- tbl_check_table(max_diffs = Inf)
    tbl_grade_table(max_diffs = Inf)
  })
  
  expect_equal(
    problem,
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade_inf,
    "Your table should have columns named .*x.*, .*y.*, .*z.*, and .*w",
    problem = problem
  )
  
  expect_grade(
    grade_inf,
    "Your table should not have columns named .*a.*, .*b.*, .*c.*, or .*d"
  )
  
  # ---- with one diff ---
  grade_one <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    problem   <- tbl_check_table(max_diffs = 1)
    tbl_grade_table(max_diffs = 1)
  })
  
  expect_equal(
    problem,
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade_one,
    "Your table should have columns named .*x.* and 3 more",
    problem = problem
  )
  
  expect_grade(
    grade_one,
    "Your table should not have columns named .*a.* or 3 more"
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
  expect_internal_problem(
    {
      .solution <- .result <- tibble::tibble(a = 1:3)
      problem <- tbl_check_table(check_dimensions = "yes")
    },
    "check_dimensions"
  )
  
  expect_internal_problem(
    {
      .solution <- .result <- tibble::tibble(a = 1:3)
      problem   <- tbl_check_table(check_names = 5)
    },
    "check_names"
  )
  
  expect_internal_problem(
    {
      .solution <- .result <- tibble::tibble(a = 1:3)
      problem   <- tbl_check_table(check_columns = NULL)
    },
    "check_columns"
  )
  
  expect_internal_problem(
    {
      .solution <- .result <- tibble::tibble(a = 1:3)
      problem   <- tbl_check_table(check_class = NA)
    },
    "check_class"
  )
  
  expect_internal_problem(
    {
      .solution <- .result <- tibble::tibble(a = 1:3)
      problem   <- tbl_check_table(check_column_values = c(TRUE, TRUE))
    },
    "check_column_values"
  )
})
