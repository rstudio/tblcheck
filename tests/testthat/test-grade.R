test_that("grading", {
  .result <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters, b = a)

  tbl_grade <- tbl_grade()
  problem_grade <- problem_grade(tbl_check())

  expect_equal(tbl_grade, problem_grade)
})

test_that("list grading", {
  .result <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters, b = a)

  problem <- tbl_check()
  problem_list <- unclass(problem)

  expect_equal(problem_grade(problem), problem_grade(problem_list))
})
