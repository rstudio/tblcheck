#' Checks that a column is identical across two tables
#'
#' Checks if the `name` column has the same class and values in `object` and
#' `expected`. If the columns differ, returns a failure state and an informative
#' message with [gradethis::fail()].
#'
#' @param name A character string of the name of the column to check.
#' @param object A data frame to be compared to `expected`.
#' @param expected A data frame containing the expected result.
#' @param check_class A logical indicating whether to check that `name` has the
#'   same class in `object` and `expected`.
#' @param check_values A logical indicating whether to check that `name` has the
#'   same values in `object` and `expected`.
#' @param n_values The number of mismatched values to print. Defaults to 3.
#'
#' @return Invisible [`NULL`]
#' @export
#'
check_column <- function(
  name,
  object = .result,
  expected = .solution,
  check_class = TRUE,
  check_values = TRUE,
  n_values = 3
) {
  if (inherits(object, ".result")) {
    object <- get(".result", parent.frame())
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", parent.frame())
  }

  obj_col <- object[[name]]
  exp_col <- expected[[name]]
  n_values <- min(length(exp_col), n_values)

  obj_class <- class(obj_col)
  exp_class <- class(exp_col)

  # check class
  if (check_class && !identical(obj_class, exp_class)) {
    problem <- problem("column_class", exp_class, obj_class)
    
    t_class <- plu::ral("class", exp_class)
    exp_class <- exp_class %>% md_code() %>% knitr::combine_words()
    
    t_obj_class <- plu::ral("class", obj_class)
    obj_class <- obj_class %>% md_code() %>% knitr::combine_words()
    
    gradethis::fail(
      "Your `{name}` column should have {t_class} {exp_class}, but it has {t_obj_class} {obj_class}.",
      problem = problem
    )
  }
  
  # check length
  obj_col_len <- length(obj_col)
  exp_col_len <- length(exp_col)
  if (obj_col_len != exp_col_len) {
    gradethis::fail(
      "Your `{name}` column should contain {exp_col_len} values, but it has {obj_col_len}.",
      problem = problem("column_length", exp_col_len, obj_col_len)
    )
  }

  # check rows
  if (check_values) {
    if (!identical(obj_col[1:n_values], exp_col[1:n_values])) {
      t_values <- plu::ral("n value", n = n_values)
      first_n_values <- knitr::combine_words(exp_col[1:n_values], before = "`")
      gradethis::fail(
        "The first {t_values} of your `{name}` column should be {first_n_values}.",
        problem = problem("column_values")
      )
    }

    if (!identical(obj_col, exp_col)) {
      gradethis::fail(
        "Your `{name}` column contains unexpected values.",
        problem = problem("column_values")
      )
    }
  }

  invisible()
}
