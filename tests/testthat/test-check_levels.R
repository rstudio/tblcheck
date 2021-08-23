test_that("number of levels", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "b"))
    .solution <- as.factor(c("a", "b", "c"))
    problem   <- tbl_check_levels()
    tbl_grade_levels()
  })
  
  expect_equal(
    problem,
    problem("n_levels", 3, 2),
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
    problem   <- tbl_check_levels()
    tbl_grade_levels()
  })
  
  expect_equal(
    problem,
    problem("levels", missing = c("x", "y", "z"), unexpected = c("a", "b", "c")),
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
    problem   <- tbl_check_levels()
    tbl_grade_levels()
  })
  
  expect_equal(
    problem,
    problem("level_order_diffs", c("c", "b", "a")),
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
    problem   <- tbl_check_levels()
    tbl_grade_levels()
  })
  
  expect_equal(
    problem,
    problem("level_order"),
    ignore_attr = "class"
  )
  
  expect_grade(
    grade,
    message = "Your result's levels were not in the expected order.",
    problem = problem
  )
})
