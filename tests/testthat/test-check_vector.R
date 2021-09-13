test_that("vec_grade_vector() checks classes", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- 1:3
    vec_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "vector_class",
      "integer",
      "character",
      expected_length = 3,
      actual_length = 26
    ),
    ignore_attr = "class"
  )
})

test_that("vec_grade_vector() checks the first three values", {
  grade <- tblcheck_test_grade({
    .result   <- rev(letters)
    .solution <- letters
    vec_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("vector_value_diffs", letters[1:3]),
    ignore_attr = "class"
  )
})

test_that("vec_grade_vector() checks multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 1:10
    class(.solution) <- c("test", "class", "integer")
    vec_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type = "vector_class", 
      expected = c("test", "class", "integer"),
      actual = "integer",
      expected_length = 10,
      actual_length = 10
    ),
    ignore_attr = "class"
  )
})

test_that("vec_grade_vector() checks for value differences beyond the first 3", {
  grade <- tblcheck_test_grade({
    .result   <- c(rep(1, 3), 5:10)
    .solution <- c(rep(1, 3), 10:15)
    vec_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("vector_values"),
    ignore_attr = "class"
  )
})

test_that("max_diffs modifies the number of values to print", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- rev(letters)
    vec_grade_vector(max_diffs = 5)
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("vector_value_diffs", letters[26:22]),
    ignore_attr = "class"
  )
})

test_that("max_diffs doesn't overflow", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:2]
    .solution <- letters[2:1]
    vec_grade_vector(max_diffs = 3)
  })

  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("vector_value_diffs", letters[2:1]),
    ignore_attr = "class"
  )
})

test_that("checks that vectors have the same length", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[1:4]
    vec_grade_vector()
  })

  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("vector_length", 4, 3),
    ignore_attr = "class"
  )
})

test_that("checks that vectors have the same names", {
  grade <- tblcheck_test_grade({
    .result   <- c(x = 1, y = 2, z = 3)
    .solution <- c(a = 1, b = 2, c = 3)
    vec_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "vector_names",
      missing = letters[1:3], unexpected = letters[24:26]
    ),
    ignore_attr = "class"
  )
})

test_that("number of levels", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "b"))
    .solution <- as.factor(c("a", "b", "c"))
    vec_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("vector_n_levels", 3, 2),
    ignore_attr = "class"
  )
})

test_that("level labels", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c"))
    .solution <- as.factor(c("x", "y", "z"))
    vec_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "vector_levels", missing = c("x", "y", "z"), unexpected = c("a", "b", "c")
    ),
    ignore_attr = "class"
  )
})

test_that("level order", {
  grade_reverse <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c"))
    .solution <- factor(.result, levels = rev(levels(.result)))
    vec_grade_vector()
  })
  
  expect_snapshot(grade_reverse)
  
  expect_equal(
    grade_reverse$problem,
    problem("vector_reverse_levels"),
    ignore_attr = "class"
  )
  
  grade_diffs <- tblcheck_test_grade({
    .result   <- factor(1:3, c("a", "b", "c"))
    .solution <- factor(1:3, c("c", "a", "b"))
    vec_grade_vector()
  })
  
  expect_snapshot(grade_diffs)
  
  expect_equal(
    grade_diffs$problem,
    problem("vector_level_order_diffs", c("c", "a", "b"), c("a", "b", "c")),
    ignore_attr = "class"
  )
  
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c", "d", "e"))
    .solution <- factor(.result, levels = c("a", "b", "c", "e", "d"))
    vec_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("vector_level_order"),
    ignore_attr = "class"
  )
})


test_that("vec_grade_vector() with no problems returns invisible()", {
  expect_invisible(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade_vector()
    })
  )
  
  expect_null(grade)
})

test_that("vec_grade_vector() handles bad user input", {
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade_vector(check_class = "yes")
    }),
    "check_class"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade_vector(check_length = c(TRUE, TRUE))
    }),
    "check_length"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade_vector(check_values = NULL)
    }),
    "check_values"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade_vector(max_diffs = 1:3)
    }),
    "max_diffs"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- NULL
      .solution <- letters[1:3]
      vec_grade_vector()
    }),
    "object"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- NULL
      vec_grade_vector()
    }),
    "expected"
  )
})

test_that("vec_check_vector() handles bad user input", {
  .result <- .solution <- letters[1:3]
  
  expect_message(
    problem <- vec_check_vector(check_class = "yes"),
    "check_class"
  )
  expect_s3_class(problem, "tblcheck_internal_problem")
  expect_match(problem$error, "check_class")
  
  expect_message(
    problem <- vec_check_vector(check_length = c(TRUE, TRUE)),
    "check_length"
  )
  expect_s3_class(problem, "tblcheck_internal_problem")
  expect_match(problem$error, "check_length")
  
  expect_message(
    problem <- vec_check_vector(check_values = NULL),
    "check_values"
  )
  expect_s3_class(problem, "tblcheck_internal_problem")
  expect_match(problem$error, "check_values")
  
  expect_message(
    problem <- vec_check_vector(max_diffs = 1:3),
    "max_diffs"
  )
  expect_s3_class(problem, "tblcheck_internal_problem")
  expect_match(problem$error, "max_diffs")
  
  .result   <- NULL
  .solution <- letters[1:3]
  expect_message(problem <- vec_check_vector(), "object")
  expect_s3_class(problem, "tblcheck_internal_problem")
  expect_match(problem$error, "object")
  
  .result   <- letters[1:3]
  .solution <- NULL
  expect_message(problem <- vec_check_vector(), "expected")
  expect_s3_class(problem, "tblcheck_internal_problem")
  expect_match(problem$error, "expected")
})
