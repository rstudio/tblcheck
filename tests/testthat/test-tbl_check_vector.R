test_that("tbl_check_vector() checks classes", {
  .result   <- letters
  .solution <- 1:3
  problem   <- tbl_check_vector()
  
  expect_equal(
    problem,
    problem(
      "vector_class",
      "integer",
      "character",
      expected_length = 3,
      actual_length = 26,
      vector = TRUE
    )
  )
})

test_that("tbl_check_vector() checks the first three values", {
  .result   <- rev(letters)
  .solution <- letters
  problem   <- tbl_check_vector()
  
  expect_equal(problem, problem("vector_values", letters[1:3], vector = TRUE))
})

test_that("tbl_check_vector() checks multiple classes", {
  .result   <- 1:10
  .solution <- 1:10
  class(.solution) <- c("test", "class", "integer")
  problem <- tbl_check_vector()
  
  expect_equal(
    problem,
    problem(
      type = "vector_class", 
      expected = c("test", "class", "integer"),
      actual = "integer",
      expected_length = 10,
      actual_length = 10,
      vector = TRUE
    )
  )
})

test_that("tbl_check_vector() checks for value differences beyond the first 3", {
  .result   <- c(rep(1, 3), 5:10)
  .solution <- c(rep(1, 3), 10:15)
  problem   <- tbl_check_vector()
  
  expect_equal(problem, problem("vector_values", vector = TRUE))
})

test_that("max_diffs modifies the number of values to print", {
  .result   <- letters
  .solution <- rev(letters)
  problem   <- tbl_check_vector(max_diffs = 5)
  
  expect_equal(problem, problem("vector_values", letters[26:22], vector = TRUE))
})

test_that("max_diffs doesn't overflow", {
  .result   <- letters[1:2]
  .solution <- letters[2:1]
  problem   <- tbl_check_vector(max_diffs = 3)
  
  expect_equal(problem, problem("vector_values", letters[2:1], vector = TRUE))
})

test_that("checks that vectors have the same length", {
  .result   <- letters[1:3]
  .solution <- letters[1:4]
  problem   <- tbl_check_vector()
  
  expect_equal(problem, problem("vector_dimensions", 4, 3, vector = TRUE))
})

test_that("checks that vectors have the same names", {
  .result   <- c(x = 1, y = 2, z = 3)
  .solution <- c(a = 1, b = 2, c = 3)
  problem <- tbl_check_vector()
  
  expect_equal(
    problem, 
    problem(
      "vector_names",
      missing = letters[1:3], unexpected = letters[24:26], vector = TRUE
    )
  )
})

test_that("tbl_check_vector() with no problems returns invisible()", {
  .result   <- letters[1:3]
  .solution <- letters[1:3]
  expect_invisible(problem <- tbl_check_vector())
  
  expect_null(problem)
})

test_that("tbl_check_vector() handles bad user input", {
  expect_internal_problem(
    {
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_check_vector(check_class = "yes")
    },
    "check_class"
  )
  
  expect_internal_problem(
    {
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_check_vector(check_length = c(TRUE, TRUE))
    },
    "check_length"
  )
  
  expect_internal_problem(
    {
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_check_vector(check_values = NULL)
    },
    "check_values"
  )
  
  expect_internal_problem(
    {
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_check_vector(max_diffs = 1:3)
    },
    "max_diffs"
  )
  
  expect_internal_problem(
    {
      .result   <- NULL
      .solution <- letters[1:3]
      problem <- tbl_check_vector()
    },
    "object"
  )
  
  expect_internal_problem(
    {
      .result   <- letters[1:3]
      .solution <- NULL
      problem <- tbl_check_vector()
    },
    "expected"
  )
})
