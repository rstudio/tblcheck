`%||%` <- rlang::`%||%`

# Add hooks to turn a chunk into an unevaluated verbatim exercise chunk
# adapted from https://bookdown.org/yihui/rmarkdown-cookbook/show-header.html
knitr::knit_hooks$set(chunk_label = function(before, options) {
  # the original chunk might be indented
  indent <- options$indent %||% ""
  in_grade_this <- isTRUE(options$grade_this)
  is_exercise <- isTRUE(options$is_exercise)

  if (isTRUE(options$chunk_label) || identical(options$chunk_label, "TRUE")) {
    options$chunk_label <- options$label
  }

  code <-
    if (before) {
      c(
        sprintf(
          "```{r %s%s}",
          options$chunk_label,
          if (is_exercise) ", exercise=TRUE" else ""
        ),
        if (in_grade_this) "grade_this({"
      )
    } else {
      c(
        if (in_grade_this) "})",
        if (!nzchar(trimws(paste(options$code, collapse = "\n")))) "\n",
        "```"
      )
    }

  paste0(indent, code, collapse = "\n")
})

knitr::opts_hooks$set(chunk_label = function(options) {
  options$eval <- FALSE
  options
})

og_source_hook <- knitr::knit_hooks$get("source")
knitr::knit_hooks$set(source = function(x, options) {
  if (!is.null(options$chunk_label)) {
    indent <- options$indent %||% ""
    in_grade_this <- isTRUE(options$grade_this)
    paste0(
      indent,
      c("", paste0(if (in_grade_this) "  ", x), ""),
      collapse = "\n"
    )
  } else {
    og_source_hook(x, options)
  }
})

# approximate the appearance of a grade in a learnr tutorial
grade_html <- function(grade) {
  x <- paste0(
    '<div class="',
    if (!length(grade$correct)) {
      sprintf("alert alert-%s", grade$type)
    } else if (isTRUE(grade$correct)) {
      "alert alert-success"
    } else {
      "alert alert-danger"
    },
    '">',
    grade$message,
    "</div>"
  )
  knitr::asis_output(x)
}

fake_grade_chunks <- function(
  user,
  solution = paste0(user, "-solution"),
  check = paste0(user, "-check"),
  setup = paste0(user, "-setup")
) {
  setup <- knitr::knit_code$get(setup)

  force(solution)
  force(check)

  user <- knitr::knit_code$get(user)
  solution <- knitr::knit_code$get(solution)
  check <- knitr::knit_code$get(check)

  env <- new.env()

  if (!is.null(setup)) {
    suppressMessages(eval(parse(text = setup), envir = env))
  }
  assign(".result", eval(parse(text = user), envir = env), envir = env)
  assign(".solution", eval(parse(text = solution), envir = env), envir = env)
  assign(".user_code", user, envir = env)
  assign(".solution_code", solution, envir = env)
  grade <- tryCatch(
    {
      grader <- eval(parse(text = check), envir = env)
      if (is.function(grader)) {
        grader(env)
      } else {
        grader
      }
    },
    gradethis_graded = identity
  )
  if (inherits(grade, "gradethis_graded")) return(grade_html(grade))
  if (!is.null(grade)) print(grade)
}
