#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

"%||%" <- function(x, y) if (is.null(x)) y else x

md_code <- function(x) {
  if (!length(x)) return(x)
  paste0("`", x, "`")
}

combine_words_with_more <- function(
  x, max_length = Inf, transform = md_code, ...
) {
  if (!length(x)) {
    return(NULL)
  }
  
  x_length <- length(x)
  
  x_max <- x[seq_len(min(max_length, x_length))]
  
  more <- if (x_length > max_length) {
    paste(x_length - max_length, "more")
  }
  
  knitr::combine_words(c(transform(x_max), more), ...)
}

find_tblcheck_call <- function() {
  calls <- sys.calls()
  calls <- vapply(calls, FUN.VALUE = character(1), function(x) {
    paste(rlang::expr_deparse(x), collapse = "\n")
  })
  
  tblcheck_fn_pattern <- "^(tbl|vec|tblcheck)_(check|grade)"
  tblcheck_calls <- grep(tblcheck_fn_pattern, calls, value = TRUE)
  if (length(tblcheck_calls)) {
    tblcheck_calls[[1]]
  }
}

has_common_ptype <- function(x, y) {
  tryCatch({
    vctrs::vec_ptype_common(x, y)
    TRUE
  }, error = function(err) {
    FALSE
  })
}
