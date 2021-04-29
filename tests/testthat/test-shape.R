
test_that("has_n_rows works", {
  expect_true(has_n_rows(mtcars, mtcars))
  expect_true(has_n_rows(mtcars, n_rows = 32))
  expect_false(has_n_rows(mtcars, mtcars[1:2, ]))
  expect_false(has_n_rows(mtcars, n_rows = 2))
})

test_that("has_n_cols works", {
  expect_true(has_n_cols(mtcars, mtcars))
  expect_true(has_n_cols(mtcars, n_cols = 11))
  expect_false(has_n_cols(mtcars, mtcars[, 1:2]))
  expect_false(has_n_cols(mtcars, n_cols = 2))
})
