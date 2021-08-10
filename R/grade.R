tbl_grade <- function(problem, max_diffs = 3) {
  if (is.null(problem)) {
    return(invisible())
  }
  
  assert_internally({
    checkmate::assert_class(problem, "tblcheck_problem")
    checkmate::assert_number(max_diffs, lower = 1)
  })
  
  message_fn <- methods::getFunction(
    gsub("(.*_)?(.*)", "tbl_message_\\2", problem$type)
  )
  
  return_if_graded(message_fn(problem, max_diffs = max_diffs))
}
