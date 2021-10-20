test_that("value differences", {
  grade_default <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 11:20
    vec_grade_values()
  })

  expect_snapshot(grade_default)

  expect_equal(
    grade_default$problem, problem("values", 11:20, 1:10), ignore_attr = "class"
  )

  grade_1 <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 11:20
    vec_grade_values(max_diffs = 1)
  })

  expect_snapshot(grade_1)
  expect_equal(grade_1$problem, grade_default$problem)

  grade_5 <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 11:20
    vec_grade_values(max_diffs = 5)
  })

  expect_snapshot(grade_5)
  expect_equal(grade_5$problem, grade_default$problem)

  grade_Inf <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- 11:20
    vec_grade_values(max_diffs = Inf)
  })

  expect_snapshot(grade_Inf)
  expect_equal(grade_Inf$problem, grade_default$problem)
})
