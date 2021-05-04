
test_that("has_classes on property values works", {
  expect_error(has_classes(mtcars))
  expect_true(has_classes(mtcars, mtcars))
  expect_true(has_classes(mtcars, classes = "data.frame"))
  expect_false(has_classes(mtcars, classes = "tbl_df"))
  expect_false(has_classes(mtcars, classes = "grouped_df"))
  expect_false(has_classes(mtcars, classes = "rowwise_df"))
})

test_that("has_classes with grouped and rowwise objects work", {
  testthat::skip_if_not_installed("dplyr")
  # grouped
  expect_false(has_classes(mtcars, mtcars %>% dplyr::group_by(cyl)))
  expect_true(
    has_classes(
      mtcars %>% dplyr::group_by(cyl),
      mtcars %>% dplyr::group_by(cyl)
    )
  )
  # exact class names
  expect_false(
    has_classes(
      mtcars %>% dplyr::group_by(cyl),
      classes = "data.frame"
    )
  )
  # loose class names
  expect_true(
    has_classes(
      mtcars %>% dplyr::group_by(cyl),
      classes = "data.frame",
      exact = FALSE
    )
  )

  # rowwise
  expect_false(has_classes(mtcars, mtcars %>% dplyr::rowwise()))
  # exact class names
  expect_false(
    has_classes(
      mtcars %>% dplyr::rowwise(),
      classes = "rowwise_df"
    )
  )
  # loose class names
  expect_true(
    has_classes(
      mtcars %>% dplyr::rowwise(),
      classes = "rowwise_df",
      exact = FALSE
    )
  )
  
  # tibble
  testthat::skip_if_not_installed("tibble")
  expect_false(has_classes(mtcars, tibble::as_tibble(mtcars)))
  expect_true(has_classes(tibble::as_tibble(mtcars), tibble::as_tibble(mtcars)))
})
