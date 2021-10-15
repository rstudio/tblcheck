test_that("grading", {
  .result   <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters, b = a)
  
  tbl_grade <- tbl_grade()
  problem_grade <- tblcheck_grade(tbl_check())
  
  expect_equal(tbl_grade, problem_grade)
})

test_that("list grading", {
  .result   <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters, b = a)
  
  problem_grade <- tblcheck_grade(tbl_check())
  list_grade <- tblcheck_grade(unclass(tbl_check()))
  
  expect_equal(problem_grade, list_grade)
})
