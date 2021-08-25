test_that("tbl_grade_vector() checks classes", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- 1:3
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem,
    problem(
      "vector_class",
      "integer",
      "character",
      expected_length = 3,
      actual_length = 26
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "Your result should be a vector of integers (class `integer`), but it is a vector of text (class `character`).",
    problem = problem,
    fixed = TRUE
  )
})

test_that("tbl_grade_vector() checks the first three values", {
  grade <- tblcheck_test_grade({
    .result   <- rev(letters)
    .solution <- letters
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem,
    problem("vector_value_diffs", letters[1:3]),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "The first 3 values of your result should be `a`, `b`, and `c",
    problem = problem
  )
})

test_that("tbl_grade_vector() checks multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 1:10
    class(.solution) <- c("test", "class", "integer")
    problem <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem,
    problem(
      type = "vector_class", 
      expected = c("test", "class", "integer"),
      actual = "integer",
      expected_length = 10,
      actual_length = 10
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "Your result should be a vector with classes `test`, `class`, and `integer`, but it is a vector of integers (class `integer`).",
    problem = problem,
    fixed = TRUE
  )
})

test_that("tbl_grade_vector() checks for value differences beyond the first 3", {
  grade <- tblcheck_test_grade({
    .result   <- c(rep(1, 3), 5:10)
    .solution <- c(rep(1, 3), 10:15)
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem,
    problem("vector_values"),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "Your result contains unexpected values.",
    problem = problem
  )
})

test_that("max_diffs modifies the number of values to print", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- rev(letters)
    problem   <- tbl_check_vector(max_diffs = 5)
    tbl_grade_vector(max_diffs = 5)
  })
  
  expect_equal(
    problem, 
    problem("vector_value_diffs", letters[26:22]),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "The first 5 values of your result should be `z`, `y`, `x`, `w`, and `v`",
    problem = problem
  )
})

test_that("max_diffs doesn't overflow", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:2]
    .solution <- letters[2:1]
    problem   <- tbl_check_vector(max_diffs = 3)
    tbl_grade_vector(max_diffs = 3)
  })

  expect_equal(
    problem,
    problem("vector_value_diffs", letters[2:1]),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "`b` and `a`.",
    problem = problem
  )
})

test_that("checks that vectors have the same length", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[1:4]
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })

  expect_equal(
    problem,
    problem("vector_length", 4, 3),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    "should contain 4 values",
    problem = problem
  )
})

test_that("checks that vectors have the same names", {
  grade <- tblcheck_test_grade({
    .result   <- c(x = 1, y = 2, z = 3)
    .solution <- c(a = 1, b = 2, c = 3)
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem, 
    problem(
      "vector_names",
      missing = letters[1:3], unexpected = letters[24:26]
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade, 
    message = "Your result should have the names `a`, `b`, and `c`. Your result should not have the names `x`, `y`, or `z`.",
    problem = problem
  )
})

test_that("tbl_grade_vector() with no problems returns invisible()", {
  expect_invisible(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      problem   <- expect_invisible(tbl_check_vector())
      tbl_grade_vector()
    })
  )
  
  expect_null(problem)
  expect_null(grade)
})

test_that("tbl_grade_vector() handles bad user input", {
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_grade_vector(check_class = "yes")
    }),
    "check_class"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_grade_vector(check_length = c(TRUE, TRUE))
    }),
    "check_length"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_grade_vector(check_values = NULL)
    }),
    "check_values"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_grade_vector(max_diffs = 1:3)
    }),
    "max_diffs"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- NULL
      .solution <- letters[1:3]
      tbl_grade_vector()
    }),
    "object"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- NULL
      tbl_grade_vector()
    }),
    "expected"
  )
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
