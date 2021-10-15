catch_internal_problem <- function(expr, ..., call = NULL) {
  tryCatch(expr, ..., error = function(err) {
    message("An error occurred in the grading code: ", err$message)
    err$call <- call %||% paste(deparse(err$call), collapse = "\n")
    problem("tblcheck_internal", error = unclass(err))
  })
}

return_if_internal_problem <- function(expr, ..., env = parent.frame()) {
  prob <- catch_internal_problem(expr, ..., call = find_tblcheck_call())
  return_if_problem(prob, env = env)
}

tblcheck_grade.tblcheck_internal_problem <- function(
  problem, max_diffs = 3, env = parent.frame(), ...
) {
  # move error up to top-level of grade
  error <- problem$error
  problem$error <- NULL
  
  gradethis::graded(
    message = paste(
      "Uh-oh! We can't provide feedback at this time. Don't worry, it's not", 
      "your fault! There's an issue behind-the-scenes with this exercise."
    ),
    correct = logical(0),
    type = "warning",
    location = "replace",
    problem = problem,
    error = error
  )
}
