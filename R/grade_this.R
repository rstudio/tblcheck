#' Grade this table
#'
#' Automatically grade a table resulting from student code using
#' [gradethis::grade_this()] and [tbl_grade()] to compare the
#' student's result with the author's solution.
#'
#' @examples
#  <!-- TODO: improve these examples -->
#' ex <- gradethis::mock_this_exercise(
#'   .solution_code = tibble(x = 1:3, y = letters[x]),
#'   .user_code = tibble(x = 1:3, y = c("A", "b", "c"))
#' )
#'
#' ## Grading Tables ----
#' grade_this_table()(ex)
#'
#' # Roughly equivalent to...
#' gradethis::grade_this({
#'   gradethis::pass_if_equal()
#'   tbl_grade()
#'   gradethis::fail()
#' })(ex)
#' @family graders
#' @seealso [tbl_grade()]
#'
#' @inheritParams tbl_grade
#' @param correct `[character(1)]`\cr The message shown to the student when
#'   their `.result` matches the exercise `.solution`, if `pass_if_equal` is
#'   `TRUE`.
#' @param pre_check,post_check `[expression]`\cr Code to run before or after the
#'   table or vector grading is performed. The pre check runs before calling
#'   [gradethis::pass_if_equal()] so that you can modify or adjust the student's
#'   `.result` or the `.solution` if there are parts of either that need to be
#'   ignored. These arguments can also be used in conjunction with the
#'   `pass_if_equal` option when the grading requirements are more involved.
#' @param pass_if_equal `[logical(1)]`\cr When `TRUE` (default), the `.result`
#'   is compared to the `.solution` with [gradethis::pass_if_equal()] after the
#'   _pre check_ and before calling the \pkg{tblcheck} grading function. When
#'   `FALSE`, you will need to include your own call to [gradethis::pass()] in
#'   either `pre_check` or `post_check` for the student to be able to receive a
#'   passing grade.
#' @param fail.message The feedback `message` used by the final, fallback
#'   [gradethis::fail()].
#' @inheritParams gradethis::fail
#' @inheritParams gradethis::gradethis_setup
#'
#' @return The returned feedback is equivalent to \pkg{gradethis} grading code
#'   using [`grade_this()`][gradethis::grade_this] with the following
#'   components:
#'
#'   1. First the `pre_check` code, if any, is evaluated. If this code calls
#'      [`pass()`][gradethis::graded], [`fail()`][gradethis::graded], or their
#'      equivalents, that feedback is provided immediately.
#'   2. If `pass_if_equal` is `TRUE`, then
#'      [`pass_if_equal()`][gradethis::pass_if_equal] is called to compare the
#'      [`.result`][gradethis::grade_this-objects] to the
#'      [`.solution`][gradethis::grade_this-objects]. The message in `correct`
#'      is used for the feedback.
#'   3. The appropriate \pkg{tblcheck} grading function is called, returning
#'      any feedback:
#'       1. `grade_this_table()` returns the results from [tbl_grade()]
#'       2. `grade_this_column()` returns the results from [tbl_grade_column()]
#'       3. `grade_this_vector()` returns the results from [vec_grade()]
#'   4. The `post_check` code, if any, is evaluated and any feedback from a call
#'      to [`pass()`][gradethis::graded], [`fail()`][gradethis::graded], or
#'      their equivalents is returned.
#'   5. Finally, if no other feedback is returned, the feedback from
#'      [gradethis::fail()] is provided to the student, using the options
#'      `fail.message`, `fail.hint` and `fail.encourage`.
#'
#' @export
grade_this_table <- function(
  correct = NULL,
  pre_check = NULL,
  post_check = NULL,
  pass_if_equal = TRUE,
  ...,
  # all the arguments from tbl_grade_table() except object/expected
  max_diffs = 3,
  check_class = TRUE,
  check_names = TRUE,
  check_dimensions = TRUE,
  check_groups = TRUE,
  check_columns = TRUE,
  check_column_class = check_columns,
  check_column_values = check_columns,
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE),
  # gradethis pass/fail options
  pass.praise = NULL
) {
  ellipsis::check_dots_empty()
  grader <- call2_tblcheck_grade_this(tbl_grade)
  rlang::eval_bare(grader)
}

#' Grade this vector
#'
#' Automatically grade a vector resulting from student code using
#' [gradethis::grade_this()] and [vec_grade()] to compare the
#' student's result with the author's solution.
#'
#' @examples
#  <!-- TODO: improve these examples -->
#' ex <- gradethis::mock_this_exercise(
#'   .solution_code = tibble(x = 1:3, y = letters[x]),
#'   .user_code = tibble(x = 1:3, y = c("A", "b", "c"))
#' )
#'
#' #' ## Grading Vectors ----
#' # Here we use `pre_check` to modify `.result` and
#' grade_this_vector(
#'   pre_check = {
#'     .result <- .result$y
#'     .solution <- .solution$y
#'   }
#' )(ex)
#'
#' # Roughly equivalent to...
#' gradethis::grade_this({
#'   .result <- .result$y
#'   .solution <- .solution$y
#'   gradethis::pass_if_equal()
#'   vec_grade()
#'   gradethis::fail()
#' })(ex)
#' @family graders
#' @seealso [vec_grade()]
#'
#' @inheritParams vec_grade
#' @inheritParams grade_this_table
#'
#' @inherit grade_this_table return
#'
#' @export
grade_this_vector <- function(
  correct = NULL,
  pre_check = NULL,
  post_check = NULL,
  pass_if_equal = TRUE,
  ...,
  # all the arguments from tbl_grade_table() except object/expected
  max_diffs = 3,
  check_class = TRUE,
  check_length = TRUE,
  check_values = TRUE,
  check_names = TRUE,
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE),
  # gradethis pass/fail options
  pass.praise = NULL
) {
  ellipsis::check_dots_empty()
  grader <- call2_tblcheck_grade_this(vec_grade)
  rlang::eval_bare(grader)
}

rlang_call_match <- function(n = 2) {
  # match/standardize the call of the caller of this functions caller (usually)
  call <- sys.call(sys.parent(n = n))
  fn <- sys.function(sys.parent(n = n))

  if (has_rlang_version("0.4.12.9002")) {
    # TODO: Require rlang 1.0 when released
    # call_match() is preferred but will be part of rlang 1.0
    getFromNamespace("call_match", "rlang")(call, fn, defaults = TRUE)
  } else {
    # replicate the relevant parts of call_match()
    call <- match.call(fn, call, expand.dots = FALSE, envir = parent.frame(n + 1))
    fmls <- rlang::fn_fmls(fn)
    names <- names(fmls)
    missing <- !names %in% names(call)
    args <- c(as.list(call[-1]), fmls[missing])
    args <- args[names]
    rlang::call2(call[[1]], !!!args)
  }
}

call2_tblcheck_grade_this <- function(
  tblcheck_grader = tbl_grade
) {
  # take args of the function calling this one
  call <- rlang_call_match()
  args <- rlang::call_args(call)

  if ("..." %in% names(args)) {
    # drop the `...` that shouldn't have been used anyway
    args <- args[setdiff(names(args), "...")]
  }

  # add the tblcheck grader to the arg list
  args$tblcheck_grader <- rlang::enexpr(tblcheck_grader)

  # and construct the call to the general purpose `tblcheck_grade_this_impl()`
  rlang::call2(tblcheck_grade_this_impl, !!!args)
}

tblcheck_grade_this_impl <- function(
  tblcheck_grader = tbl_grade,
  ...,
  correct = NULL,
  pre_check = NULL,
  post_check = NULL,
  pass_if_equal = TRUE,
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE),
  # gradethis pass/fail options
  pass.praise = NULL
) {
  pre_check  <- rlang::enexpr(pre_check)
  post_check <- rlang::enexpr(post_check)

  tblcheck_grader_args <- list(
    object = .result,
    expected = .solution,
    hint = hint,
    encourage = encourage,
    ...
  )

  function(check_env) {
    check_env_pre <- rlang::new_environment(
      list(
        ".__tblcheck_grader_args" = tblcheck_grader_args,
        ".__tblcheck_grader" = tblcheck_grader,
        ".__pre_check"       = pre_check,
        ".__post_check"      = post_check,
        ".__pass_if_equal"   = pass_if_equal,
        ".__correct"         = correct        %||% getOption("gradethis.pass"),
        ".__pass.praise"     = pass.praise    %||% getOption("gradethis.pass.praise", FALSE)
      ),
      parent = rlang::env_parent(check_env)
    )
    # insert the pre-check env one level above check_env
    rlang::env_poke_parent(check_env, check_env_pre)

    grade <- gradethis::grade_this({
      # Evaluate pre-check code
      pre_check <- get0(".__pre_check")
      if (!is.null(pre_check)) {
        rlang::eval_bare(pre_check)
      }

      if (isTRUE(get0(".__pass_if_equal", inherits = TRUE, ifnotfound = FALSE))) {
        # pass immediately if they're _exactly_ the same
        gradethis::pass_if_equal(message = get(".__correct"), praise = get(".__pass.praise"))
      }

      # call the tblcheck grader
      do.call(get(".__tblcheck_grader"), get(".__tblcheck_grader_args"))

      # evaluate post check or extra check code
      post_check <- get0(".__post_check")
      if (!is.null(post_check)) {
        rlang::eval_bare(post_check)
      }

      # pass, possibly with code feedback
      gradethis::pass(message = get(".__correct"), praise = get(".__pass.praise"))
    })(check_env)

    class(grade) <- c("tblcheck_graded", class(grade))
    grade
  }
}
