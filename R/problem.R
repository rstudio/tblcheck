problem <- function(type, expected = NULL, actual = NULL, ...) {
  stopifnot(
    "`type` must be string" = is.character(type),
    "`type` must be length 1" = length(type) == 1,
    "`type` must have a character value" = nzchar(type)
  )
  
  list(
    type = type,
    expected = expected,
    actual = actual,
    ...
  )
}
