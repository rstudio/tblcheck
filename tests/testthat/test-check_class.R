test_that("tbl_grade_class()", {
  grade <- tblcheck_test_grade({
    .result   <- "1"
    .solution <- 1
    problem   <- tbl_check_class()
    tbl_grade_class()
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "character",
      expected_length = 1,
      actual_length = 1
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be a number (class `numeric`), but it is a text string (class `character`).",
    problem = problem,
    fixed   = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- c("1", "2")
    .solution <- c(1, 2)
    problem   <- tbl_check_class()
    tbl_grade_class()
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "character",
      expected_length = 2,
      actual_length = 2
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be a vector of numbers (class `numeric`), but it is a vector of text (class `character`).",
    problem = problem,
    fixed   = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- "2021-07-29 10:59:59"
    .solution <- as.POSIXct("2021-07-29 10:59:59")
    problem   <- tbl_check_class()
    tbl_grade_class()
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = c("POSIXct", "POSIXt"),
      actual   = "character",
      expected_length = 1,
      actual_length = 1
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be a date-time (class `POSIXct`), but it is a text string (class `character`).",
    problem = problem,
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    problem   <- tbl_check_class()
    tbl_grade_class()
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = c("POSIXlt", "POSIXt"),
      actual   = "character",
      expected_length = 2,
      actual_length = 2
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be a vector of date-times (class `POSIXlt`), but it is a vector of text (class `character`).",
    problem = problem,
    fixed   = TRUE
  )
})

test_that("tbl_grade_class() ignores inconsequential mismatches", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1
    problem   <- expect_invisible(tbl_check_class())
    tbl_grade_class()
  })
  
  expect_null(problem)
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1
    problem   <- tbl_check_class(all_differences = TRUE)
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "numeric",
      actual   = "integer",
      expected_length = 1,
      actual_length = 1
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be a number (class `numeric`), but it is an integer (class `integer`).",
    problem = problem,
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- glue::glue("x")
    .solution <- "x"
    problem   <- expect_invisible(tbl_check_class())
    tbl_grade_class()
  })
  
  expect_null(problem)
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- glue::glue("x")
    .solution <- "x"
    problem   <- tbl_check_class(all_differences = TRUE)
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "character",
      actual   = c("glue", "character"),
      expected_length = 1,
      actual_length = 1
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be a text string (class `character`), but it is an object with classes `glue` and `character`.",
    problem = problem,
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    problem   <- expect_invisible(tbl_check_class())
    tbl_grade_class()
  })
  
  expect_null(problem)
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    problem   <- tbl_check_class(all_differences = TRUE)
    tbl_grade_class(all_differences = TRUE)
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = c("POSIXlt", "POSIXt"),
      actual   = c("POSIXct", "POSIXt"),
      expected_length = 2,
      actual_length = 2
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be a vector of date-times (class `POSIXlt`), but it is a vector of date-times (class `POSIXct`).",
    problem = problem,
    fixed = TRUE
  )
})

test_that("tbl_grade_class() with multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1L
    class(.solution) <- c("test", "class", "integer")
    problem <- tbl_check_class()
    tbl_grade_class()
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = c("test", "class", "integer"),
      actual   = "integer",
      expected_length = 1,
      actual_length = 1
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be an object with classes `test`, `class`, and `integer`, but it is an integer (class `integer`).",
    problem = problem,
    fixed = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result <- 1L
    class(.result) <- c("test", "class", "integer")
    .solution <- 1L
    problem <- tbl_check_class()
    tbl_grade_class()
  })
  
  expect_equal(
    problem,
    problem(
      type     = "class",
      expected = "integer",
      actual   = c("test", "class", "integer"),
      expected_length = 1,
      actual_length = 1
    )
  )
  
  expect_grade(
    grade,
    message = "Your result should be an integer (class `integer`), but it is an object with classes `test`, `class`, and `integer`.",
    problem = problem,
    fixed = TRUE
  )
})
