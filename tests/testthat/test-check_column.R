test_that("tbl_grade_column() checks classes", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = 1:3)
    tbl_grade_column("a", .result, .solution)
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "column_class",
      "integer", 
      "character",
      expected_length = 3,
      actual_length = 26,
      column = "a"
    ),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_column() checks the first three values", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = rev(letters))
    .solution <- tibble::tibble(a = letters)
    tbl_grade_column("a")
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "column_value_diffs",
      letters[1:3],
      column = "a"
    ),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_column() checks multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = data.frame(x = 1))
    .solution <- tibble::tibble(a = tibble::tibble(x = 1))
    tbl_grade_column("a")
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type = "column_class", 
      expected = c("tbl_df", "tbl", "data.frame"), 
      actual = "data.frame",
      expected_length = 1,
      actual_length = 1,
      column = "a"
    ),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_column() checks for value differences beyond the first 3", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = c(rep(1, 3), 5:10))
    .solution <- tibble::tibble(a = c(rep(1, 3), 10:15))
    tbl_grade_column("a")
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("column_values", column = "a"),
    ignore_attr = "class"
  )
})

test_that("max_diffs modifies the number of values to print", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters)
    .solution <- tibble::tibble(a = rev(letters))
    tbl_grade_column("a", max_diffs = 5)
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("column_value_diffs", letters[26:22], column = "a"),
    ignore_attr = "class"
  )
})

test_that("max_diffs doesn't overflow", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:2])
    .solution <- tibble::tibble(a = letters[2:1])
    tbl_grade_column("a", max_diffs = 3)
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("column_value_diffs", letters[2:1], column = "a"),
    ignore_attr = "class"
  )
})

test_that("checks that columns have the same length", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:4])
    tbl_grade_column("a")
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("column_length", 4, 3, column = "a"),
    ignore_attr = "class"
  )
})

test_that("checks that the column is present in object", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(b = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_column("a")
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("table_names", missing = "a"),
    ignore_attr = "class"
  )
})

test_that("checks that the column is present in expected", {
  expect_warning(
    {
      grade <- tblcheck_test_grade({
        .result   <- tibble::tibble(b = letters[1:3])
        .solution <- tibble::tibble(a = letters[1:3])
        tbl_grade_column("b")
      })
    },
    "`b` is not a column in `expected`"
  )
  
  expect_null(grade)
})

test_that("tbl_grade_column() with no problems returns invisible()", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = letters[1:3])
    .solution <- tibble::tibble(a = letters[1:3])
    tbl_grade_column("a")
  })
  
  expect_null(grade)
  
  expect_invisible(
    tbl_grade_column("a", tibble::tibble(a = 1), tibble::tibble(a = 1))
  )

  expect_null(grade)
})

test_that("number of levels", {
  grade <- tblcheck_test_grade({
    .result   <- tibble::tibble(a = as.factor(c("a", "b", "b")))
    .solution <- tibble::tibble(a = as.factor(c("a", "b", "c")))
    tbl_grade_column("a")
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
    tbl_grade_column("a")
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
    tbl_grade_column("a")
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
    tbl_grade_column("a")
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
    problem   <- tbl_check_column("a")
    tbl_grade_column("a")
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("column_level_order", column = "a"),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_column() handles bad user input", {
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column(3, object = result, expected = solution)
    }),
    "column"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column(c("a", "b"), object = result, expected = solution)
    }),
    "column"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = solution, check_class = "yes")
    }),
    "check_class"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = solution, check_length = c(TRUE, TRUE))
    }),
    "check_length"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = solution, check_values = "yes")
    }),
    "check_values"
  )
  
  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = solution, max_diffs = 1:3)
    }),
    "max_diffs"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = 12, expected = solution)
    }),
    "object"
  )

  expect_internal_problem(
    tblcheck_test_grade({
      result <- solution <- tibble::tibble(b = letters[1:3])
      tbl_grade_column("b", object = result, expected = list(a = 1))
    }),
    "expected"
  )
})

test_that("tbl_check_column() handles bad user input", {
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column(3, object = result, expected = solution)
    },
    "column"
  )
  
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column(c("a", "b"), object = result, expected = solution)
    },
    "column"
  )
  
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column("b", object = result, expected = solution, check_class = "yes")
    },
    "check_class"
  )
  
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column("b", object = result, expected = solution, check_length = c(TRUE, TRUE))
    },
    "check_length"
  )
  
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column("b", object = result, expected = solution, check_values = "yes")
    },
    "check_values"
  )
  
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column("b", object = result, expected = solution, max_diffs = 1:3)
    },
    "max_diffs"
  )
  
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column("b", object = 12, expected = solution)
    },
    "object"
  )
  
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column("b", object = result, expected = list(a = 1))
    },
    "expected"
  )
})
