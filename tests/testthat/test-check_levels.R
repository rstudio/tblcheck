test_that("number of levels", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "b"))
    .solution <- as.factor(c("a", "b", "c"))
    vec_grade_levels()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("n_levels", 3, 2),
    ignore_attr = "class"
  )
})

test_that("level labels", {
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c"))
    .solution <- as.factor(c("x", "y", "z"))
    vec_grade_levels()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("levels", missing = c("x", "y", "z"), unexpected = c("a", "b", "c")),
    ignore_attr = "class"
  )
})

test_that("level order", {
  grade_diffs <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c"))
    .solution <- factor(.result, levels = rev(levels(.result)))
    vec_grade_levels()
  })
  
  expect_snapshot(grade_diffs)
  
  expect_equal(
    grade_diffs$problem,
    problem("level_order_diffs", c("c", "b", "a")),
    ignore_attr = "class"
  )
  
  grade <- tblcheck_test_grade({
    .result   <- as.factor(c("a", "b", "c", "d", "e"))
    .solution <- factor(.result, levels = c("a", "b", "c", "e", "d"))
    vec_grade_levels()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("level_order"),
    ignore_attr = "class"
  )
})
