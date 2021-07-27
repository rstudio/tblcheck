test_that("check_class()", {
  grade <- tblcheck_test_grade({
    .result   <- 1L
    .solution <- 1
    check_class()
  })
  
  expect_grade(
    grade,
    message = "Your result should have class `numeric`, but it has class `integer`",
    problem = problem("class", "numeric", "integer")
  )
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
    message = "Your result should have classes `test`, `class`, and `integer`, but it has class `integer`",
    problem = problem("class", c("test", "class", "integer"), "integer")
  )
})
