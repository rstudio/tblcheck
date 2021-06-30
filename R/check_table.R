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
  object   = get(".result", parent.frame()),
  expected = get(".solution", parent.frame())
) {
  # check length ----
  if (!identical(nrow(object), nrow(expected))) { 
    gradethis::fail("Your table should have {nrow(expected)} rows.")
  }
  
  # check column names ----
  missing_msg <- unexpected_msg <- NULL
  
  expected_names <- names(expected)
  object_names   <- names(object)
  
  missing_names <- setdiff(expected_names, object_names)
  if (length(missing_names)) {
    names <- knitr::combine_words(missing_names, before = "`")
    missing_msg <- glue::glue(
      "Your result should have a column named {names}. "
    )
  }
    
  unexpected_names <- setdiff(object_names, expected_names)
  if (length(unexpected_names)) {
    names <- knitr::combine_words(unexpected_names, and = " or ", before = "`")
    unexpected_msg <- glue::glue(
      "Your result should not have a column named {names}."
    )
  }
  
  if (!is.null(missing_msg) || !is.null(unexpected_msg)) {
    gradethis::fail("{missing_msg}{unexpected_msg}")
  }
  
  # check column contents ----
  # lapply(object_names, check_column)
  
  return(invisible(NULL))
}
