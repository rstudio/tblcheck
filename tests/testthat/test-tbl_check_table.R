test_that("tbl_check_table() class", {
  .result   <- data.frame(a = 1:10, b = 1:10)
  .solution <- tibble::tibble(a = 1:10, b = 1:10)
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem(
      "table_class",
      list(class = c("tbl_df", "tbl", "data.frame"), length = 2),
      list(class = "data.frame", length = 2),
      object_label = "table"
    )
  )
  
  .result   <- tibble::tibble(a = 1:10, b = a)
  .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem(
      "table_class",
      list(class = c("grouped_df", "tbl_df", "tbl", "data.frame"), length = 2),
      list(class = c("tbl_df", "tbl", "data.frame"), length = 2),
      object_label = "table"
    )
  )
  
  .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
  .solution <- tibble::tibble(a = 1:10, b = a)
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem(
      "table_class",
      list(class = c("tbl_df", "tbl", "data.frame"), length = 2),
      list(class = c("rowwise_df", "tbl_df", "tbl", "data.frame"), length = 2),
      object_label = "table"
    )
  )

  .result   <- dplyr::rowwise(tibble::tibble(a = 1:10, b = a))
  .solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = a), a)
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem(
      "table_class",
      list(class = c("grouped_df", "tbl_df", "tbl", "data.frame"), length = 2),
      list(class = c("rowwise_df", "tbl_df", "tbl", "data.frame"), length = 2),
      object_label = "table"
    )
  )
})

test_that("tbl_check_table() rows", {
  .result   <- tibble::tibble(a = letters, b = a)
  .solution <- tibble::tibble(x = letters[-1], y = x)
  problem   <- tbl_check_table(object = .result, expected = .solution)
  
  expect_equal(
    problem,
    problem("table_nrow", 25, 26, object_label = "table")
  )
  
  .result   <- tibble::tibble(a = letters, b = a)
  .solution <- tibble::tibble(x = letters[1], y = x)
  problem   <- tbl_check_table(object = .result, expected = .solution)
  
  expect_equal(
    problem,
    problem("table_nrow", 1, 26, object_label = "table")
  )
})

test_that("tbl_check_table() ncol", {
  .result   <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters, b = a)
  problem   <- tbl_check_table(check_names = FALSE)
  
  expect_equal(
    problem,
    problem("table_ncol", 2, 3, object_label = "table")
  )
  
  .result   <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters)
  problem   <- tbl_check_table(check_names = FALSE)
  
  expect_equal(
    problem,
    problem("table_ncol", 1, 3, object_label = "table")
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
      unexpected = c("a", "b"),
      object_label = "table"
    )
  )
})

test_that("tbl_check_table() columns", {
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[24:26])
  problem   <- tbl_check_table(object = .result, expected = .solution)
  
  expect_equal(
    problem,
    problem("column_values", letters[24:26], object_label = "`a` column")
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
      problem <- tbl_check_table(check_nrow = "yes")
    },
    "check_nrow"
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
      problem   <- tbl_check_table(check_ncol = list())
    },
    "check_ncol"
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
    problem(
      type = "table_nrow", expected = 25L, actual = 26L, object_label = "table"
    )
  )
  
  .result   <- tibble::tibble(a = letters)
  .solution <- tibble::tibble(a = letters[1])
  problem   <- tbl_check_table()
  
  expect_equal(
    problem,
    problem(
      type = "table_nrow", expected = 1L, actual = 26L, object_label = "table"
    )
  )
})

test_that("tbl_check_table() returns ncol feedback to learnr", {
  .result   <- tibble::tibble(a = letters, b = letters, c = letters)
  .solution <- tibble::tibble(a = letters, b = letters)
  problem   <- tbl_check_table(check_names = FALSE)
  
  expect_equal(
    problem,
    problem("table_ncol", 2, 3, object_label = "table")
  )
  
  .result   <- tibble::tibble(a = letters, b = letters, c = letters)
  .solution <- tibble::tibble(a = letters)
  problem   <- tbl_check_table(check_names = FALSE)
  
  expect_equal(
    problem,
    problem("table_ncol", 1, 3, object_label = "table")
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
      unexpected = c("a", "b", "c", "d"),
      object_label = "table"
    )
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
      unexpected = c("a", "b", "c", "d"),
      object_label = "table"
    )
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
      unexpected = c("a", "b", "c", "d"),
      object_label = "table"
    )
  )
})
