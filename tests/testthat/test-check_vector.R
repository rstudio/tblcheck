test_that("tbl_grade_vector() checks classes", {
  grade <- tblcheck_test_grade({
    .result   <- letters
    .solution <- 1:3
    tbl_grade_vector()
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

test_that("tbl_grade_vector() checks the first three values", {
  grade <- tblcheck_test_grade({
    .result   <- rev(letters)
    .solution <- letters
    tbl_grade_vector()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("vector_value_diffs", letters[1:3]),
    ignore_attr = "class"
  )
})

test_that("tbl_grade_vector() checks multiple classes", {
  grade <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 1:10
    class(.solution) <- c("test", "class", "integer")
    tbl_grade_vector()
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

test_that("tbl_grade_vector() checks for value differences beyond the first 3", {
  grade <- tblcheck_test_grade({
    .result   <- c(rep(1, 3), 5:10)
    .solution <- c(rep(1, 3), 10:15)
    tbl_grade_vector()
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
    tbl_grade_vector(max_diffs = 5)
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
    tbl_grade_vector(max_diffs = 3)
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
    tbl_grade_vector()
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
    tbl_grade_vector()
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
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem,
    problem("vector_n_levels", 3, 2),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your result should have 3 levels, but it has 2 levels.",
    problem = problem
  )
})

test_that("level labels", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c"))
    .solution <- as.factor(c("x", "y", "z"))
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem,
    problem(
      "vector_levels", missing = c("x", "y", "z"), unexpected = c("a", "b", "c")
    ),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your result should have levels named `x`, `y`, and `z`. Your result should not have levels named `a`, `b`, or `c`.",
    problem = problem
  )
})

test_that("level order", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c"))
    .solution <- factor(.result, levels = rev(levels(.result)))
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem,
    problem("vector_level_order_diffs", c("c", "b", "a")),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your result's levels were not in the expected order. The first 3 levels of your result should be `c`, `b`, and `a`.",
    problem = problem
  )
  
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c", "d", "e"))
    .solution <- factor(.result, levels = c("a", "b", "c", "e", "d"))
    problem   <- tbl_check_vector()
    tbl_grade_vector()
  })
  
  expect_equal(
    problem,
    problem("vector_level_order"),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your result's levels were not in the expected order.",
    problem = problem
  )
})


test_that("tbl_grade_vector() with no problems returns invisible()", {
  expect_invisible(
    grade <- tblcheck_test_grade({
      .result   <- letters[1:3]
      .solution <- letters[1:3]
      tbl_grade_vector()
    })
  )
  
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
