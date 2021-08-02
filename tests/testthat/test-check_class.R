test_that("check_class()", {
  grade <- tblcheck_test_grade({
    .result   <- "1"
    .solution <- 1
    check_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be a number (class `numeric`), but it is a text string (class `character`).",
    problem = problem("class", "numeric", "character"),
    fixed   = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- c("1", "2")
    .solution <- c(1, 2)
    check_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be a vector of numbers (class `numeric`), but it is a vector of text (class `character`).",
    problem = problem("class", "numeric", "character"),
    fixed   = TRUE
  )
})

test_that("check_class() ignores inconsequential mismatches", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1
    check_class()
  })
  
  expect_null(grade)
  
  grade <- tblcheck_test_grade({
    .result   <- glue::glue("x")
    .solution <- "x"
    check_class()
  })
  
  expect_null(grade)
})

test_that("check_class() with multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1L
    class(.solution) <- c("test", "class", "integer")
    check_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should have classes `test`, `class`, and `integer`, but it is an integer (class `integer`).",
    problem = problem("class", c("test", "class", "integer"), "integer"),
    fixed   = TRUE
  )
  
  grade <- tblcheck_test_grade({
    .result   <- 1L
    class(.result) <- c("test", "class", "integer")
    .solution <- 1L
    check_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should be an integer (class `integer`), but it has classes `test`, `class`, and `integer`.",
    problem = problem("class", "integer", c("test", "class", "integer")),
    fixed   = TRUE
  )
})
