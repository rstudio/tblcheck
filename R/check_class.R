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
  
  if (!identical(obj_class, exp_class)) {
    if (identical(exp_class, "numeric") && identical(obj_class, "integer")) return()
    if (identical(sort(union(exp_class, obj_class)), c("character", "glue"))) return()
    if (identical(sort(union(exp_class, obj_class)), c("POSIXct", "POSIXlt", "POSIXt"))) return()
    
    class_problem <- problem(paste0(problem_prefix, "class"), exp_class, obj_class)
    
    if ("rowwise_df" %in% obj_class && "grouped_df" %in% exp_class) {
      return_fail(
        "Your {unit} is a rowwise data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?",
        problem = class_problem
      )
    }
    
    table_class_message <- table_class_message(obj_class, exp_class)
    if (!is.null(table_class_message)) {
      return_fail(
        table_class_message,
        problem = class_problem
      )
    }
    
    friendly_exp_class <- friendly_class(exp_class, expected)
    
    if (!is.null(friendly_exp_class)) {
      exp_message <- glue::glue("Your {unit} should be {friendly_exp_class},")
    } else {
      exp_class_str <- knitr::combine_words(md_code(exp_class))
      exp_message   <- ngettext(
        length(exp_class),
        glue::glue("Your {unit} should have class {exp_class_str},"),
        glue::glue("Your {unit} should have classes {exp_class_str},")
      )
    }
    
    friendly_obj_class <- friendly_class(obj_class, object)
    
    if (!is.null(friendly_obj_class)) {
      obj_message <- glue::glue("but it is {friendly_obj_class}.")
    } else {
      obj_class_str <- knitr::combine_words(md_code(obj_class))
      obj_message   <- ngettext(
        length(obj_class),
        glue::glue("but it has class {obj_class_str}."),
        glue::glue("but it has classes {obj_class_str}.")
      )
    }
    
    return_fail(paste(exp_message, obj_message), problem = class_problem)
  }
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
  
  invisible()
}

table_class_message <- function(obj_class, exp_class) {
  for (class in class_message_list[, "class"]) {
    if (class %in% obj_class && !class %in% exp_class) {
      return(
        class_message_list[class_message_list[, "class"] == class, "unexpected"]
      )
    }
    
    if (!class %in% obj_class && class %in% exp_class) {
      return(
        class_message_list[class_message_list[, "class"] == class, "missing"]
      )
    }
  }
  
  invisible()
}

class_message_list <- rbind(
  list(
    class = "grouped_df",
    missing = "Your {unit} isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?",
    unexpected = "Your {unit} is a grouped data frame, but I wasn't expecting it to be grouped. Maybe you need to use `ungroup()`?"
  ),
  list(
    class = "rowwise_df",
    missing = "Your {unit} isn't a rowwise data frame, but I was expecting it to be rowwise. Maybe you need to use `rowwise()`?",
    unexpected = "Your {unit} is a rowwise data frame, but I wasn't expecting it to be rowwise. Maybe you need to use `ungroup()`?"
  )
)
