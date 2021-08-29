test_that("vector length", {
  grade <- tblcheck_test_grade({
    .result   <- letters[1:3]
    .solution <- letters[1:4]
    vec_grade_length()
  })
  
  expect_snapshot(grade)
  
  expect_equal(
    grade$problem,
    problem("length", 4, 3),
    ignore_attr = "class"
  )
})
