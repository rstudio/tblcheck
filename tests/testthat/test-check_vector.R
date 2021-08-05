test_that("check_vector() checks classes", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- 1:3
    check_vector()
  })
  
  expect_grade(
    grade,
    "Your result should be a vector of integers (class `integer`), but it is a vector of text (class `character`).",
    problem = problem("vector_class", "integer", "character"),
    fixed = TRUE
  )
})

test_that("check_vector() checks the first three values", {
  grade <- tblcheck_test_grade({
    .result   <- rev(letters)
    .solution <- letters
    check_vector()
  })
  
  expect_grade(
    grade,
    "The first 3 values of your result should be `a`, `b`, and `c",
    problem = problem("vector_values", letters[1:3])
  )
})

test_that("check_vector() checks multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 1:10
    class(.solution) <- c("test", "class", "integer")
    check_vector()
  })
  
  expect_grade(
    grade,
    "Your result should be a vector with classes `test`, `class`, and `integer`, but it is a vector of integers (class `integer`).",
    problem = problem(
      type = "vector_class", 
      expected = c("test", "class", "integer"), 
      actual = "integer"
    ),
    fixed = TRUE
  )
})

test_that("check_vector() checks for value differences beyond the first 3", {
  grade <- tblcheck_test_grade({
    .result   <- c(rep(1, 3), 5:10)
    .solution <- c(rep(1, 3), 10:15)
    check_vector()
  })
  
  expect_grade(
    grade,
    "Your result contains unexpected values.",
    problem = problem("vector_values")
  )
})

test_that("max_diffs modifies the number of values to print", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- rev(letters)
    check_vector(max_diffs = 5)
  })
  
  expect_grade(
    grade,
    "The first 5 values of your result should be `z`, `y`, `x`, `w`, and `v`",
    problem = problem("vector_values", letters[26:22])
  )
})

test_that("max_diffs doesn't overflow", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:2]
    .solution <- letters[2:1]
    check_vector(max_diffs = 3)
  })

  expect_equal(grade$problem, problem("vector_values", letters[2:1]))
  expect_false(grade$correct)
  expect_no_match(grade$message, "`NA`")
  expect_match(grade$message, "`b` and `a`.")
})

test_that("checks that vectors have the same length", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[1:4]
    check_vector()
  })

  expect_equal(grade$problem, problem("vector_length", 4, 3))
  expect_false(grade$correct)
  expect_match(grade$message, "should contain 4 values")
})

test_that("checks that vectors have the same names", {
  grade <- tblcheck_test_grade({
    .result   <- c(x = 1, y = 2, z = 3)
    .solution <- c(a = 1, b = 2, c = 3)
    check_vector()
  })
  
  expect_grade(
    grade, 
    message = "Your result should have the names `a`, `b`, and `c`. Your result should not have the names `x`, `y`, or `z`.",
    problem = problem("vector_names", missing = letters[1:3], unexpected = letters[24:26])
  )
})

test_that("check_vector() with no problems returns invisible()", {
  expect_invisible(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      check_vector()
    })
  )
  
  expect_null(grade$problem)
  expect_null(grade$correct)
  expect_null(grade$message)
})

test_that("check_vector() handles bad user input", {
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      check_vector(check_class = "yes")
    }),
    "check_class"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      check_vector(check_length = c(TRUE, TRUE))
    }),
    "check_length"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      check_vector(check_values = NULL)
    }),
    "check_values"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      check_vector(max_diffs = 1:3)
    }),
    "max_diffs"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- NULL
      .solution <- letters[1:3]
      check_vector()
    }),
    "object"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- NULL
      check_vector()
    }),
    "expected"
  )
})
