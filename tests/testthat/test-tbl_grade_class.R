test_that("tbl_grade_class()", {
  grade <- tblcheck_test_grade({
    .result   <- "1"
    .solution <- 1
    tbl_grade_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be a number (class `numeric`), but it is a text string (class `character`).",
    problem = problem(
      type     = "class",
      expected = list(class = "numeric", length = 1),
      actual   = list(class = "character", length = 1)
    ),
    fixed   = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- c("1", "2")
    .solution <- c(1, 2)
    tbl_grade_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be a vector of numbers (class `numeric`), but it is a vector of text (class `character`).",
    problem = problem(
      type     = "class",
      expected = list(class = "numeric", length = 2),
      actual   = list(class = "character", length = 2)
    ),
    fixed   = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- "2021-07-29 10:59:59"
    .solution <- as.POSIXct("2021-07-29 10:59:59")
    tbl_grade_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be a date-time (class `POSIXct`), but it is a text string (class `character`).",
    problem = problem(
      type     = "class",
      expected = list(class = c("POSIXct", "POSIXt"), length = 1),
      actual   = list(class = "character", length = 1)
    ),
    fixed   = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- c("2021-07-29 15:18:00", "1996-03-05 12:00:00")
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    tbl_grade_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be a vector of date-times (class `POSIXlt`), but it is a vector of text (class `character`).",
    problem = problem(
      type     = "class",
      expected = list(class = c("POSIXlt", "POSIXt"), length = 2),
      actual   = list(class = "character", length = 2)
    ),
    fixed   = TRUE
  )
})

test_that("tbl_grade_class() ignores inconsequential mismatches", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1
    tbl_grade_class()
  })
  
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- glue::glue("x")
    .solution <- "x"
    tbl_grade_class()
  })
  
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- as.POSIXct(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    .solution <- as.POSIXlt(c("2021-07-29 15:18:00", "1996-03-05 12:00:00"))
    tbl_grade_class()
  })
  
  expect_null(grade)
})

test_that("tbl_grade_class() with multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1L
    class(.solution) <- c("test", "class", "integer")
    tbl_grade_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be an object with classes `test`, `class`, and `integer`, but it is an integer (class `integer`).",
    problem = problem(
      type     = "class",
      expected = list(class = c("test", "class", "integer"), length = 1),
      actual   = list(class = "integer", length = 1)
    ),
    fixed   = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- 1L
    class(.result) <- c("test", "class", "integer")
    .solution <- 1L
    tbl_grade_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be an integer (class `integer`), but it is an object with classes `test`, `class`, and `integer`.",
    problem = problem(
      type     = "class",
      expected = list(class = "integer", length = 1),
      actual   = list(class = c("test", "class", "integer"), length = 1)
    ),
    fixed   = TRUE
  )
})
