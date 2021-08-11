test_that("tbl_check_column() checks classes", {
  .result   <- tibble::tibble(a = letters)
  .solution <- tibble::tibble(a = 1:3)
  problem   <- tbl_check_column("a")
  
  expect_equal(
    problem,
    problem(
      "column_class",
      list(class = "integer", length = 3), 
      list(class = "character", length = 26),
      vector = TRUE,
      column = "a"
    )
  )
})

test_that("tbl_check_column() checks the first three values", {
  .result   <- tibble::tibble(a = rev(letters))
  .solution <- tibble::tibble(a = letters)
  problem   <- tbl_check_column("a")
  
  expect_equal(
    problem,
    problem(
      "column_values",
      letters[1:3],
      vector = TRUE,
      column = "a"
    )
  )
})

test_that("tbl_check_column() checks multiple classes", {
  .result   <- tibble::tibble(a = data.frame(x = 1))
  .solution <- tibble::tibble(a = tibble::tibble(x = 1))
  problem   <- tbl_check_column('a')
  
  expect_equal(
    problem,
    problem(
      type = "column_class", 
      expected = list(class = c("tbl_df", "tbl", "data.frame"), length = 1), 
      actual = list(class = "data.frame", length = 1),
      vector = TRUE,
      column = "a"
    )
  )
})

test_that("tbl_check_column() checks for value differences beyond the first 3", {
  .result   <- tibble::tibble(a = c(rep(1, 3), 5:10))
  .solution <- tibble::tibble(a = c(rep(1, 3), 10:15))
  problem   <- tbl_check_column("a")
  
  expect_equal(
    problem, 
    problem("column_values", vector = TRUE, column = "a")
  )
})

test_that("max_diffs modifies the number of values to print", {
  .result   <- tibble::tibble(a = letters)
  .solution <- tibble::tibble(a = rev(letters))
  problem   <- tbl_check_column("a", max_diffs = 5)
  
  expect_equal(
    problem,
    problem("column_values", letters[26:22], vector = TRUE, column = "a")
  )
})

test_that("max_diffs doesn't overflow", {
  .result   <- tibble::tibble(a = letters[1:2])
  .solution <- tibble::tibble(a = letters[2:1])
  problem   <- tbl_check_column("a", max_diffs = 3)

  expect_equal(
    problem,
    problem("column_values", letters[2:1], vector = TRUE, column = "a")
  )
})

test_that("checks that columns have the same length", {
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:4])
  problem   <- tbl_check_column("a")
  
  expect_equal(
    problem,
    problem("column_length", 4, 3, vector = TRUE, column = "a")
  )
})

test_that("checks that the column is present in object", {
  .result   <- tibble::tibble(b = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3])
  problem   <- tbl_check_column("a")
  
  expect_equal(
    problem,
    problem("column_missing", "a", column = "a"),
  )
})

test_that("checks that the column is present in expected", {
  .result   <- tibble::tibble(b = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3])
  expect_warning(
    problem <- tbl_check_column("b"),
    "`b` is not a column in `expected`"
  )

  expect_null(problem)
})

test_that("tbl_check_column() with no problems returns invisible()", {
  .result   <- tibble::tibble(a = letters[1:3])
  .solution <- tibble::tibble(a = letters[1:3])
  expect_invisible(problem <- tbl_check_column("a"))

  expect_null(problem)
})

test_that("tbl_check_column() handles bad user input", {
  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column(3, object = result, expected = solution)
    },
    "name"
  )

  expect_internal_problem(
    {
      result <- solution <- tibble::tibble(b = letters[1:3])
      problem <- tbl_check_column(c("a", "b"), object = result, expected = solution)
    },
    "name"
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
