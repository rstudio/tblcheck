
test_that("has_classes works", {
  expect_true(has_classes(mtcars, mtcars))
  expect_true(has_classes(mtcars, classes = "data.frame"))
  expect_false(has_classes(mtcars, classes = "grouped_df"))
  expect_false(has_classes(mtcars, classes = "rowwise_df"))

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
})
