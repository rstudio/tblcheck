# Grade this vector

Automatically grade a vector resulting from student code using
[`gradethis::grade_this()`](https://pkgs.rstudio.com/gradethis/reference/grade_this.html)
and
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/vec_check.md)
to compare the student's result with the author's solution.

## Usage

``` r
grade_this_vector(
  correct = NULL,
  pre_check = NULL,
  post_check = NULL,
  pass_if_equal = TRUE,
  ...,
  max_diffs = 3,
  check_class = TRUE,
  ignore_class = NULL,
  check_length = TRUE,
  check_levels = TRUE,
  check_values = TRUE,
  tolerance = sqrt(.Machine$double.eps),
  check_names = TRUE,
  hint = getOption("gradethis.fail.hint", FALSE),
  encourage = getOption("gradethis.fail.encourage", FALSE),
  pass.praise = NULL
)
```

## Arguments

- correct:

  `[character(1)]`  
  The message shown to the student when their `.result` matches the
  exercise `.solution`, if `pass_if_equal` is `TRUE`.

- pre_check, post_check:

  `[expression]`  
  Code to run before or after the table or vector grading is performed.
  The pre check runs before calling
  [`gradethis::pass_if_equal()`](https://pkgs.rstudio.com/gradethis/reference/pass_if_equal.html)
  so that you can modify or adjust the student's `.result` or the
  `.solution` if there are parts of either that need to be ignored.
  These arguments can also be used in conjunction with the
  `pass_if_equal` option when the grading requirements are more
  involved.

- pass_if_equal:

  `[logical(1)]`  
  When `TRUE` (default for `grade_this_vector()` but not
  [`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/grade_this_table.md)),
  the `.result` is compared to the `.solution` with
  [`gradethis::pass_if_equal()`](https://pkgs.rstudio.com/gradethis/reference/pass_if_equal.html)
  after the *pre check* and before calling the tblcheck grading
  function.

- ...:

  Additional arguments passed to `graded()` or additional data to be
  included in the feedback object.

- max_diffs:

  `[numeric(1)]`  
  The maximum number of mismatched values to print. Defaults to 3.

- check_class:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same classes.

- ignore_class:

  `[character()]`  
  A vector of classes to ignore when finding differences between
  `object` and `expected`.

  If an element is named, differences will only be ignored between the
  pair of the element and its name. For example,
  `ignore_class = c("integer" = "numeric")` will ignore class
  differences only if `object` has class
  [integer](https://rdrr.io/r/base/integer.html) and `expected` has
  class [numeric](https://rdrr.io/r/base/numeric.html), or vice versa.

  If all the classes of `expected` are included in `ignore_class`, a
  `class` problem will never be returned.

- check_length:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same length.

- check_levels:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same [factor
  levels](https://rdrr.io/r/base/levels.html).

- check_values:

  `[logical(1)]`  
  Whether to check that `object` and `expected` contain the same values.

- tolerance:

  `[numeric(1) ≥ 0]`  
  `values` differences smaller than `tolerance` are ignored. The default
  value is close to `1.5e-8`.

- check_names:

  `[logical(1)]`  
  Whether to check that `object` and `expected` have the same names.

- hint:

  Include a code feedback hint with the failing message? This argument
  only applies to `fail()` and `fail_if_equal()` and the message is
  added using the default options of
  [`give_code_feedback()`](https://pkgs.rstudio.com/gradethis/reference/code_feedback.html)
  and
  [`maybe_code_feedback()`](https://pkgs.rstudio.com/gradethis/reference/code_feedback.html).
  The default value of `hint` can be set using
  [`gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html)
  or the `gradethis.fail.hint` option.

- encourage:

  Include a random encouraging phrase with
  [`random_encouragement()`](https://pkgs.rstudio.com/gradethis/reference/praise.html)?
  The default value of `encourage` can be set using
  [`gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html)
  or the `gradethis.fail.encourage` option.

- pass.praise:

  Logical `TRUE` or `FALSE` to determine whether a praising phrase
  should be automatically prepended to any
  [`pass()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
  or
  [`pass_if_equal()`](https://pkgs.rstudio.com/gradethis/reference/pass_if_equal.html)
  messages. Sets the `gradethis.pass.praise` option.

## Value

The returned feedback is equivalent to gradethis grading code using
[`grade_this()`](https://pkgs.rstudio.com/gradethis/reference/grade_this.html)
with the following components:

1.  First the `pre_check` code, if any, is evaluated. If this code calls
    [`pass()`](https://pkgs.rstudio.com/gradethis/reference/graded.html),
    [`fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html),
    or their equivalents, that feedback is provided immediately.

2.  If `pass_if_equal` is `TRUE`, then
    [`pass_if_equal()`](https://pkgs.rstudio.com/gradethis/reference/pass_if_equal.html)
    is called to compare the
    [`.result`](https://pkgs.rstudio.com/gradethis/reference/grade_this-objects.html)
    to the
    [`.solution`](https://pkgs.rstudio.com/gradethis/reference/grade_this-objects.html).
    The message in `correct` is used for the feedback.

3.  The appropriate tblcheck grading function is called, returning any
    feedback:

    1.  [`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/grade_this_table.md)
        returns the results from
        [`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/tbl_check.md)

    2.  `grade_this_vector()` returns the results from
        [`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/vec_check.md)

4.  The `post_check` code, if any, is evaluated and any feedback from a
    call to
    [`pass()`](https://pkgs.rstudio.com/gradethis/reference/graded.html),
    [`fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html),
    or their equivalents is returned.

5.  Finally, if no other feedback is returned, the feedback from
    [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
    is provided to the student, using the options `fail.message`,
    `fail.hint` and `fail.encourage`.

## See also

[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr133/reference/vec_check.md)

Other graders:
[`grade_this_table`](https://rstudio.github.io/tblcheck/preview/pr133/reference/grade_this_table.md)`()`

## Examples

``` r
ex <- gradethis::mock_this_exercise(
  .solution_code = tibble::tibble(x = 1:3, y = letters[x]),
  .user_code = tibble::tibble(x = 1:3, y = c("A", "b", "c"))
)

#' ## Grading Vectors ----
# Here we use `pre_check` to modify `.result` and
grade_this_vector(
  pre_check = {
    .result <- .result$y
    .solution <- .solution$y
  }
)(ex)
#> <tblcheck_graded: [Incorrect]
#>   The first 3 values of your result should be `a`, `b`, and `c`,
#>   not `A`, `b`, and `c`.
#> >

# Roughly equivalent to...
gradethis::grade_this({
  .result <- .result$y
  .solution <- .solution$y
  gradethis::pass_if_equal()
  vec_grade()
  gradethis::fail()
})(ex)
#> <gradethis_graded: [Incorrect]
#>   The first 3 values of your result should be `a`, `b`, and `c`,
#>   not `A`, `b`, and `c`.
#> >
```
