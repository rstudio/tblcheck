#' Checks that two objects have the same classes
#'
#' Checks if `object` and `expected` have the same [class][class()].
#' If the classes differ, returns a failure state and an informative message
#' with [gradethis::fail()].
#' 
#' @section Problems:
#' 
#' 1. `class`: `object` doesn't have the same classes as `expected`
#'
#' @param object An object to be compared to `expected`.
#' @param expected An object containing the expected result.
#' @param unit `[character(1)]`\cr The label used to describe the object in
#'   feedback messages. Defaults to `"result"`.
#' @param problem_prefix `[character(1)]`\cr The prefix appended to the
#'   `problem` label in [gradethis::fail()] objects.
#'   Defaults to `""`, which appends no prefix.
#'
#' @inherit check_table return
#' @export


check_class <- function(
  object = .result, expected = .solution, unit = "result", problem_prefix = ""
) {
  if (inherits(object, ".result")) {
    object <- get(".result", parent.frame())
  }
  if (inherits(expected, ".solution")) {
    expected <- get(".solution", parent.frame())
  }
  
  obj_class <- class(object)
  exp_class <- class(expected)
  
  assert_internally({
    checkmate::assert_string(unit)
    checkmate::assert_string(problem_prefix)
  })
  
  if (!identical(obj_class, exp_class)) {
    if (!has_meaningful_class_difference(exp_class, obj_class)) {
      return()
    }
    
    class_problem <- problem(paste0(problem_prefix, "class"), exp_class, obj_class)
    
    hinted_class_message <- hinted_class_message(obj_class, exp_class)
    if (!is.null(hinted_class_message)) {
      return_fail(hinted_class_message, problem = class_problem)
    }
    
    friendly_exp_class <- friendly_class(exp_class, expected)
    friendly_obj_class <- friendly_class(obj_class, object)
    message <- glue::glue(
      "Your {unit} should be {friendly_exp_class}, but it is {friendly_obj_class}."
    )
    
    return_fail(message, problem = class_problem)
  }
}

has_meaningful_class_difference <- function(exp_class, obj_class) {
  class_union <- sort(union(exp_class, obj_class))
  
  # Check that the union of `exp_class` and `obj_class` is not in the list
  # of insignificant class differences
  !any(
    purrr::map_lgl(insignificant_class_differences(), identical, class_union)
  )
}

insignificant_class_differences <- function() {
  # The list is sorted when called in order to ensure the same
  # locale-specific sorting rules apply to both inputs to
  # `identical()` in `has_meaningful_class_difference()`
  lapply(
    list(
      c("integer", "numeric"),
      c("character", "glue"),
      c("POSIXct", "POSIXlt", "POSIXt")
    ),
    sort
  )
}

friendly_class <- function(class, x) {
  if (identical(class, "character")) {
    if (length(x) > 1) return("a vector of text (class `character`)")
    return("a text string (class `character`)")
  }
  
  if (identical(class, "numeric")) {
    if (length(x) > 1) return("a vector of numbers (class `numeric`)")
    return("a number (class `numeric`)")
  }
  
  if (identical(class, "integer")) {
    if (length(x) > 1) return("a vector of integers (class `integer`)")
    return("an integer (class `integer`)")
  }
  
  if (all(class %in% c("POSIXt", "POSIXct", "POSIXlt"))) {
    class <- setdiff(class, "POSIXt")
    if (length(x) > 1) return(glue::glue("a vector of date-times (class `{class}`)"))
    return(glue::glue("a date-time (class `{class}`)"))
  }
  
  if (identical(class, c("tbl_df", "tbl", "data.frame"))) {
    return("a tibble (class `tbl_df`)")
  }
  
  if (identical(class, "data.frame")) {
    return("a data frame (class `data.frame`)")
  }
  
  paste(
    ifelse(length(x) > 1, "a vector", "an object"),
    "with",
    ngettext(length(class), "class", "classes"),
    knitr::combine_words(md_code(class))
  )
}

hinted_class_message <- function(obj_class, exp_class) {
  hinted_class_message_list <- hinted_class_message_list()
  
  for (i in seq_along(hinted_class_message_list)) {
    list <- hinted_class_message_list[[i]]
    
    if (
      list$obj_test(list$obj_class, obj_class) &&
      list$exp_test(list$exp_class, exp_class)
    ) {
      return(list$message)
    }
  }
  
  invisible()
}

hinted_class_message_list <- function() {
  list(
    list(
      obj_class = "rowwise_df",
      obj_test  = `%in%`,
      exp_class = "grouped_df",
      exp_test  = `%in%`,
      message   = "Your {unit} is a rowwise data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?"
    ),
    list(
      obj_test  = function(...) TRUE,
      exp_class = "grouped_df",
      exp_test  = `%in%`,
      message   = "Your {unit} isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?"
    ),
    list(
      obj_class = "grouped_df",
      obj_test  = `%in%`,
      exp_test  = function(...) TRUE,
      message   = "Your {unit} is a grouped data frame, but I wasn't expecting it to be grouped. Maybe you need to use `ungroup()`?"
    ),
    list(
      obj_test  = function(...) TRUE,
      exp_class = "rowwise_df",
      exp_test  = `%in%`,
      message   = "Your {unit} isn't a rowwise data frame, but I was expecting it to be rowwise. Maybe you need to use `rowwise()`?"
    ),
    list(
      obj_class = "rowwise_df",
      obj_test  = `%in%`,
      exp_test  = function(...) TRUE,
      message   = "Your {unit} is a rowwise data frame, but I wasn't expecting it to be rowwise. Maybe you need to use `ungroup()`?"
    )
  )
}
