test_that("grading", {
  .result   <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters, b = a)
  
  tbl_grade <- tbl_grade_table()
  problem_grade <- tblcheck_grade(tbl_check_table())
  
  expect_equal(tbl_grade, problem_grade)
})

test_that("list grading", {
  .result   <- tibble::tibble(a = letters, b = a, c = a)
  .solution <- tibble::tibble(a = letters, b = a)
  
  problem_grade <- tblcheck_grade(tbl_check_table())
  list_grade <- tblcheck_grade(unclass(tbl_check_table()))
  
  expect_equal(problem_grade, list_grade)
})
