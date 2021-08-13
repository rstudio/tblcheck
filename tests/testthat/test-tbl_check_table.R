test_that("tbl_check_table() class", {
  .result   <- data.frame(a = 1:10, b = 1:10)
  .solution <- tibble::tibble(a = 1:10, b = 1:10)
  problem   <- tbl_check_table()
  
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
  
  .result   <- tibble::tibble(a = 1:10, b = a)
  .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
  problem   <- tbl_check_table()
  
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
  
  .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
  .solution <- tibble::tibble(a = 1:10, b = a)
  problem   <- tbl_check_table()
  
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

  .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
  .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
  problem   <- tbl_check_table()
  
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
})

test_that("tbl_check_table() rows", {
  .result   <- tibble::tibble(a = letters, b = a)
  .solution <- tibble::tibble(a = letters[-1], b = a)
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem("table_dimensions", c(25, 2), c(26, 2)),
    ignore_attr = "class"
  )
  
  .result   <- tibble::tibble(a = letters, b = a)
  .solution <- tibble::tibble(a = letters[1], b = a)
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem("table_dimensions", c(1, 2), c(26, 2)),
    ignore_attr = "class"
  )
})

test_that("tbl_check_table() ncol", {
  .result   <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters, b = a)
  problem   <- tbl_check_table(check_names = FALSE)
  
  expect_equal(
    problem,
    problem("table_dimensions", c(26, 2), c(26, 3)),
    ignore_attr = "class"
  )
  
  .result   <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters)
  problem   <- tbl_check_table(check_names = FALSE)
  
  expect_equal(
    problem,
    problem("table_dimensions", c(26, 1), c(26, 3)),
    ignore_attr = "class"
  )
})

test_that("tbl_check_table() names", {
  .result   <- tibble::tibble(a = letters[1:3], b = a)
  .solution <- tibble::tibble(x = letters[1:3], y = x)
  problem   <- tbl_check_table(object = .result, expected = .solution)
  
  expect_equal(
    problem,
    problem(
      "table_names",
      missing = c("x", "y"),
      unexpected = c("a", "b")
    ),
    ignore_attr = "class"
  )
})

test_that("tbl_check_table() columns", {
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[24:26])
  problem   <- tbl_check_table(object = .result, expected = .solution)
  
  expect_equal(
    problem,
    problem("column_values", letters[24:26], column = "a"),
    ignore_attr = "class"
  )
})

test_that("tbl_check_table() with no problems returns invisible()", {
  .solution <- tibble::tibble(a = letters[1:3], b = a, c = a)
  
  problem <- expect_invisible(
    tbl_check_table(object = .solution, expected = .solution)
  )
  
  expect_null(problem)
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

test_that("tbl_check_table() returns grades with row problems", {
  .result   <- tibble::tibble(a = letters)
  .solution <- tibble::tibble(a = letters[1:25])
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem("table_dimensions", c(25, 1), c(26, 1)),
    ignore_attr = "class"
  )
  
  .result   <- tibble::tibble(a = letters)
  .solution <- tibble::tibble(a = letters[1])
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem("table_dimensions", c(1, 1), c(26, 1)),
    ignore_attr = "class"
  )
})

test_that("tbl_check_table() returns ncol feedback to learnr", {
  .result   <- tibble::tibble(a = letters, b = letters, c = letters)
  .solution <- tibble::tibble(a = letters, b = letters)
  problem   <- tbl_check_table(check_names = FALSE)
  
  expect_equal(
    problem,
    problem("table_dimensions", c(26, 2), c(26, 3)),
    ignore_attr = "class"
  )
  
  .result   <- tibble::tibble(a = letters, b = letters, c = letters)
  .solution <- tibble::tibble(a = letters)
  problem   <- tbl_check_table(check_names = FALSE)
  
  expect_equal(
    problem,
    problem("table_dimensions", c(26, 1), c(26, 3)),
    ignore_attr = "class"
  )
})

test_that("tbl_check_table() returns names feedback to learnr", {
  .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
  .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
  problem   <- tbl_check_table()

  expect_equal(
    problem, 
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )
  
  # ---- with all diffs ---
  .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
  .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
  problem   <- tbl_check_table(max_diffs = Inf)
  
  expect_equal(
    problem,
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )
  
  # ---- with one diff ---
  .result   <- tibble::tibble(a = letters, b = a, c = a, d = a)
  .solution <- tibble::tibble(x = letters, y = x, z = x, w = x)
  problem   <- tbl_check_table(max_diffs = 1)
  
  expect_equal(
    problem,
    problem(
      "table_names",
      missing = c("x", "y", "z", "w"),
      unexpected = c("a", "b", "c", "d")
    ),
    ignore_attr = "class"
  )
})
