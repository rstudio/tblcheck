test_that("vec_grade() checks classes", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- 1:3
    vec_grade()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "class",
      "integer",
      "character",
      expected_length = 3,
      actual_length = 26,
      location = "vector"
    ),
    ignore_attr = "class"
  )
})

test_that("vec_grade() checks the first three values", {
  grade <- tblcheck_test_grade({
    .result   <- rev(letters)
    .solution <- letters
    vec_grade()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("values", letters, rev(letters), location = "vector"),
    ignore_attr = "class"
  )
})

test_that("vec_grade() checks multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 1:10
    class(.solution) <- c("test", "class", "integer")
    vec_grade()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      type = "class", 
      expected = c("test", "class", "integer"),
      actual = "integer",
      expected_length = 10,
      actual_length = 10,
      location = "vector"
    ),
    ignore_attr = "class"
  )
})

test_that("vec_grade() checks for value differences beyond the first 3", {
  grade <- tblcheck_test_grade({
    .result   <- c(rep(1, 3), 5:10)
    .solution <- c(rep(1, 3), 10:15)
    vec_grade()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "values", c(rep(1, 3), 10:15), c(rep(1, 3), 5:10), location = "vector"
    ),
    ignore_attr = "class"
  )
})

test_that("max_diffs modifies the number of values to print", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- rev(letters)
    vec_grade(max_diffs = 5)
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("values", rev(letters), letters, location = "vector"),
    ignore_attr = "class"
  )
})

test_that("max_diffs doesn't overflow", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:2]
    .solution <- letters[2:1]
    vec_grade(max_diffs = 3)
  })

  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("values", letters[2:1], letters[1:2], location = "vector"),
    ignore_attr = "class"
  )
})

test_that("checks that vectors have the same length", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[1:4]
    vec_grade()
  })

  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "length", letters[1:4], letters[1:3],
      expected_length = 4, actual_length = 3, location = "vector"
    ),
    ignore_attr = "class"
  )
})

test_that("checks that vectors have the same names", {
  grade <- tblcheck_test_grade({
    .result   <- c(x = 1, y = 2, z = 3)
    .solution <- c(a = 1, b = 2, c = 3)
    vec_grade()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "names",
      missing = letters[1:3], unexpected = letters[24:26],
      location = "vector"
    ),
    ignore_attr = "class"
  )
})

test_that("number of levels", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "b"))
    .solution <- as.factor(c("a", "b", "c"))
    vec_grade()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("levels_n", 3, 2, location = "vector"),
    ignore_attr = "class"
  )
})

test_that("level labels", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c"))
    .solution <- as.factor(c("x", "y", "z"))
    vec_grade()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "levels",
      missing = c("x", "y", "z"),
      unexpected = c("a", "b", "c"),
      location = "vector"
    ),
    ignore_attr = "class"
  )
})

test_that("level order", {
  grade_reverse <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c"))
    .solution <- factor(.result, levels = rev(levels(.result)))
    vec_grade()
  })
  
  expect_snapshot(grade_reverse)
  
  expect_equal(
    grade_reverse$problem,
    problem("levels_reversed", location = "vector"),
    ignore_attr = "class"
  )
  
  grade_diffs <- tblcheck_test_grade({
    .result   <- factor(1:3, c("a", "b", "c"))
    .solution <- factor(1:3, c("c", "a", "b"))
    vec_grade()
  })
  
  expect_snapshot(grade_diffs)
  
  expect_equal(
    grade_diffs$problem,
    problem(
      "levels_order", c("c", "a", "b"), c("a", "b", "c"), location = "vector"
    ),
    ignore_attr = "class"
  )
  
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c", "d", "e"))
    .solution <- factor(.result, levels = c("a", "b", "c", "e", "d"))
    vec_grade()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem(
      "levels_order",
      c("a", "b", "c", "e", "d"),
      c("a", "b", "c", "d", "e"),
      location = "vector"
    ),
    ignore_attr = "class"
  )
})


test_that("vec_grade() with no problems returns invisible()", {
  expect_invisible(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade()
    })
  )
  
  expect_null(grade)
})

test_that("vec_grade() handles bad user input", {
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade(check_class = "yes")
    }),
    "check_class"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade(check_length = c(TRUE, TRUE))
    }),
    "check_length"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      vec_grade(check_values = NULL)
    }),
    "check_values"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[4:6]
      vec_grade(max_diffs = 1:3)
    }),
    "max_diffs"
  )
  
  expect_internal_problem(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- NULL
      vec_grade()
    }),
    "expected"
  )
})

test_that("vec_check() handles bad user input", {
  .result <- .solution <- letters[1:3]
  
  expect_internal_problem(
    vec_check(check_class = "yes"),
    message = "check_class"
  )
  
  expect_internal_problem(
    vec_check(check_length = c(TRUE, TRUE)),
    message = "check_length"
  )
  
  expect_internal_problem(
    vec_check(check_values = NULL),
    message = "check_values"
  )
  
  .result   <- letters[1:3]
  .solution <- NULL
  expect_internal_problem(vec_check(), "expected")
})
