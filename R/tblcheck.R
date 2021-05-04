
# template strings when signaling grades (if we want to implement that)
check_tibble_template <- list(
  "fail not data frame" = "I expected {x_name} to be a `{s3_class}`.",
  "fail not grouped"    = "I expected {x_name} to be grouped by {group_vars}.",
  "fail not rowwise"    = "I expected {x_name} to be grouped by rows.",
  "fail col_names"      = "{missing_cols}{extra_cols}",
  "extra columns"       = "I didn't expect {x_name} to have the column{if (n != 1) 's' else ''} {cols}.",
  "missing columns"     = "I expected {x_name} to also have the column{if (n != 1) 's' else ''} {cols}.\n",
  "fail n_cols"         = "I expected a table with {n_cols} column{if (n_cols != 1) 's' else ''}, but {x_name} has **{ncol(x)}**.",
  "fail n_rows"         = "I expected a table with {n_rows} row{if (n_rows != 1) 's' else ''}, but {x_name} has **{nrow(x)}**.",
  "fail col_types"      = "In {x_name}, I expected:{wrong}",
  "col_type mismatch"   = function(name, type) {
    # this function is applied to each column `name` which should have `type`
    pred <- if (substr(type, 1, 1) %in% c("a", "e", "i", "o", "u")) "an" else "a"
    as.character(glue::glue("\n    - `{name}` to be {pred} {type}", .trim = FALSE))
  }
)

#' Checks if a table has expected classes.
#'
#' @param x tibble or data.frame
#' @param reference tibble or data.frame
#' @param classes character
#' @param exact A boolean to indicate whether to use exact matching
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
#' @examples
#' has_classes(mtcars, mtcars)
#' has_classes(mtcars, classes = "data.frame")
has_classes <- function(
  x,
  reference = NULL,
  classes = NULL,
  exact = TRUE
) {
  if (is.null(reference) && is.null(classes)) {
    stop("Grading error: `has_classes()` requires a value for either the `reference` or the `classes` argument.")
  }
  if (!is.null(reference)) {
    classes <- classes %||% class(reference)
  }
  if (exact) {
    return(identical(class(x), classes))
  } else {
    return(all(classes %in% class(x)))
  }
}

# TODO does it have these groups ?
