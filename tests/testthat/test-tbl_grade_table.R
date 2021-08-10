test_that("tbl_grade_table() class", {
  grade <- tblcheck_test_grade({
    .result   <- data.frame(a = 1:10, b = 1:10)
    .solution <- tibble::tibble(a = 1:10, b = 1:10)
    tbl_grade_table()
  })
  
  expect_grade(
    grade,
    message = "Your table should be a tibble (class `tbl_df`), but it is a data frame (class `data.frame`)",
    problem = problem(
      "table_class",
      list(class = c("tbl_df", "tbl", "data.frame"), length = 2),
      list(class = "data.frame", length = 2),
      object_label = "table"
    ),
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = 1:10, b = a)
    .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
    tbl_grade_table()
  })
  
  expect_grade(
    grade,
    message = "Your table isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?",
    problem = problem(
      "table_class",
      list(class = c("grouped_df", "tbl_df", "tbl", "data.frame"), length = 2),
      list(class = c("tbl_df", "tbl", "data.frame"), length = 2),
      object_label = "table"
    ),
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
    .solution <- tibble::tibble(a = 1:10, b = a)
    tbl_grade_table()
  })
  
  expect_grade(
    grade,
    message = "Your table is a rowwise data frame, but I wasn't expecting it to be rowwise. Maybe you need to use `ungroup()`?",
    problem = problem(
      "table_class",
      list(class = c("tbl_df", "tbl", "data.frame"), length = 2),
      list(class = c("rowwise_df", "tbl_df", "tbl", "data.frame"), length = 2),
      object_label = "table"
    ),
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
    .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
    tbl_grade_table()
  })
  
  expect_grade(
    grade,
    message = "Your table is a rowwise data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?",
    problem = problem(
      "table_class",
      list(class = c("grouped_df", "tbl_df", "tbl", "data.frame"), length = 2),
      list(class = c("rowwise_df", "tbl_df", "tbl", "data.frame"), length = 2),
      object_label = "table"
    ),
    fixed = TRUE
  )
})

test_that("tbl_grade_table() rows", {
  grade <- tblcheck_test_grade({
    result <- tibble::tibble(a = letters, b = a)
    solution <- tibble::tibble(x = letters[-1], y = x)
    tbl_grade_table(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "should have 25 rows",
    problem = problem("table_nrow", 25, 26, object_label = "table")
  )
  
  grade <- tblcheck_test_grade({
    result <- tibble::tibble(a = letters, b = a)
    solution <- tibble::tibble(x = letters[1], y = x)
    tbl_grade_table(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "should have 1 row",
    problem = problem("table_nrow", 1, 26, object_label = "table")
  )
})

test_that("tbl_grade_table() ncol", {
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble(a = letters, b = a, c = a)
    solution <- tibble::tibble(a = letters, b = a)
    tbl_grade_table(object = result, expected = solution, check_names = FALSE)
  })
  
  expect_grade(
    grade,
    message = "should have 2 columns",
    problem = problem("table_ncol", 2, 3, object_label = "table")
  )
  
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble(a = letters, b = a, c = a)
    solution <- tibble::tibble(a = letters)
    tbl_grade_table(object = result, expected = solution, check_names = FALSE)
  })
  
  expect_grade(
    grade,
    message = "should have 1 column",
    problem = problem("table_ncol", 1, 3, object_label = "table")
  )
})

test_that("tbl_grade_table() names", {
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble(a = letters[1:3], b = a)
    solution <- tibble::tibble(x = letters[1:3], y = x)
    tbl_grade_table(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "Your table should have columns named `x` and `y`",
    problem = problem(
      "table_names",
      missing = c("x", "y"),
      unexpected = c("a", "b"),
      object_label = "table"
    )
  )
  expect_match(grade$message, "should not have columns named `a` or `b`")
})

test_that("tbl_grade_table() columns", {
  grade <- tblcheck_test_grade({
    result   <- tibble::tibble(a = letters[1:3])
    solution <- tibble::tibble(a = letters[24:26])
    tbl_grade_table(object = result, expected = solution)
  })
  
  expect_grade(
    grade,
    message = "first 3 values of your `a` column should be `x`, `y`, and `z`",
    problem = problem(
      "column_values", letters[24:26], object_label = "`a` column"
    )
  )
})

test_that("tbl_grade_table() with no problems returns invisible()", {
  solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  grade <- expect_invisible(
    tbl_grade_table(object = solution, expected = solution)
  )
  
  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("tbl_grade_table() handles bad user input", {
  expect_internal_problem(
    tblcheck_test_grade({
      solution <- result <- tibble::tibble(a = 1:3)
      tbl_grade_table(object = result, expected = solution, check_nrow = "yes")
    }),
    "check_nrow"
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
      tbl_grade_table(object = result, expected = solution, check_ncol = list())
    }),
    "check_ncol"
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

test_that("tbl_grade_table() returns grades with row problems", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = letters[1:25])
    tbl_grade_table()
  })
  
  expect_grade(
    grade,
    "should have 25 rows",
    problem = problem(
      type = "table_nrow", expected = 25L, actual = 26L, object_label = "table"
    )
  )
  
  grade_single <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = letters[1])
    tbl_grade_table()
  })
  
  expect_grade(
    grade_single,
    "should have 1 row",
    problem = problem(
      type = "table_nrow", expected = 1L, actual = 26L, object_label = "table"
    )
  )
})

test_that("tbl_grade_table() returns ncol feedback to learnr", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = letters, c = letters)
    .solution <- tibble::tibble(a = letters, b = letters)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_grade(
    grade,
    "should have 2 columns",
    problem = problem("table_ncol", 2, 3, object_label = "table")
  )
  
  
  grade_one <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = letters, c = letters)
    .solution <- tibble::tibble(a = letters)
    tbl_grade_table(check_names = FALSE)
  })
  
  expect_grade(
    grade_one,
    "should have 1 column",
    problem = problem("table_ncol", 1, 3, object_label = "table")
  )
})

test_that("tbl_grade_table() returns names feedback to learnr", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    tbl_grade_table()
  })

  expect_grade(
    grade, 
    "Your table should have columns named .*x.*, .*y.*, .*z.*, and 1 more",
    problem = problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d"),
      object_label = "table"
    )
  )
  
  expect_grade(
    grade,
    "Your table should not have columns named .*a.*, .*b.*, .*c.*, or 1 more"
  )
  
  # ---- with all diffs ---
  grade_inf <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    tbl_grade_table(max_diffs = Inf)
  })
  
  expect_grade(
    grade_inf,
    "Your table should have columns named .*x.*, .*y.*, .*z.*, and .*w",
    problem = problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d"),
      object_label = "table"
    )
  )
  
  expect_grade(
    grade_inf,
    "Your table should not have columns named .*a.*, .*b.*, .*c.*, or .*d"
  )
  
  # ---- with one diff ---
  grade_one <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
    .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
    tbl_grade_table(max_diffs = 1)
  })
  
  expect_grade(
    grade_one,
    "Your table should have columns named .*x.* and 3 more",
    problem = problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d"),
      object_label = "table"
    )
  )
  
  expect_grade(
    grade_one,
    "Your table should not have columns named .*a.* or 3 more"
  )
})
