
test_that("has_extra_columns works", {
  expect_false(has_extra_columns(mtcars, mtcars))
  expect_true(has_extra_columns(mtcars, mtcars[, 1:2]))
  expect_false(has_extra_columns(mtcars, col_names = names(mtcars)))
  expect_true(has_extra_columns(mtcars, col_names = "mpg"))
  expect_true(has_extra_columns(mtcars, col_names = c("mpg", "cyl")))
})

test_that("has_missing_columns works", {
  expect_false(has_missing_columns(mtcars, mtcars))
  expect_false(has_missing_columns(mtcars, col_names = names(mtcars)))
  expect_true(has_missing_columns(mtcars["mpg"], mtcars))
  expect_false(has_missing_columns(mtcars["mpg"], col_names = "mpg"))
  expect_true(has_missing_columns(mtcars["mpg"], col_names = c("mpg", "disp")))
})

test_that("has_column_types works", {
  expect_true(has_column_types(mtcars, mtcars))
  expect_true(has_column_types(mtcars["mpg"], col_types = "numeric"))
  # incorrect case
  mtcars_bad <- mtcars
  mtcars_bad["mpg"] <- as.character(mtcars_bad["mpg"])
  expect_false(has_column_types(mtcars_bad, mtcars))
})


