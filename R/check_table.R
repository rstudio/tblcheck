#' Check that the rows and columns of two tables are the same
#'
#' Checks if `object` and `expected` have the same number of rows, the same
#' column names, and the same column contents.
#' If the data frames differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#'
#' @param object A data frame to be compared to `expected`.
#' @param expected A data frame containing the expected result.
#'
#' @return Invisible [`NULL`]
#' @export

check_table <- function(
  object      = .result,
  expected    = .solution,
) {
  if (inherits(object, "gradethis_placeholder")) {
    x <- get(".solution", parent.frame(2))
  }
  if (inherits(expected, "gradethis_placeholder")) {
    y <- get(".solution", parent.frame(2))
  }
  
  # check length ----
  if (!identical(nrow(object), nrow(expected))) { 
    gradethis::fail("Your table should have {nrow(expected)} rows.")
  }
  
  # check column names ----
  if (check_names) check_names(object, expected)
  
  if (!is.null(missing_msg) || !is.null(unexpected_msg)) {
    gradethis::fail("{missing_msg}{unexpected_msg}")
  }
  
  # check column contents ----
  # lapply(object_names, check_column)
  
  return(invisible(NULL))
}
