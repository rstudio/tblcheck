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

test_that("NA values", {
  grade_na <- tblcheck_test_grade({
    .result   <- c(TRUE, TRUE, NA)
    .solution <- c(TRUE, TRUE, TRUE)
    vec_grade_values()
  })

  expect_snapshot(grade_na)

  expect_equal(
    grade_na$problem,
    problem("values", c(TRUE, TRUE, TRUE), c(TRUE, TRUE, NA)),
    ignore_attr = "class"
  )

  grade_na_match <- tblcheck_test_grade({
    .result   <- c(TRUE, TRUE, NA)
    .solution <- c(TRUE, TRUE, NA)
    vec_grade_values()
  })

  expect_null(grade_na_match)
})

test_that("vec_grade_values() failures", {
  grade_length <- tblcheck_test_grade({
    .result   <- 1:9
    .solution <- 1:10
    vec_grade_values()
  })

  expect_equal(grade_length, vec_grade_dimensions(1:9, 1:10))

  grade_class <- tblcheck_test_grade({
    .result   <- 1:10
    .solution <- letters[1:10]
    vec_grade_values()
  })

  expect_equal(grade_class, vec_grade_class(1:10, letters[1:10]))

  grade_attr <- tblcheck_test_grade({
    .result   <- structure(1, class = "test", attr = "not a")
    .solution <- structure(1, class = "test", attr = "match")
    vec_grade_values()
  })

  expect_snapshot(grade_attr)

  expect_equal(
    grade_attr$problem,
    problem("values"),
    ignore_attr = "class"
  )
})
