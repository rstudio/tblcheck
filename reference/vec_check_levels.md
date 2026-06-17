# Check that the levels of two factors are the same

Checks if `object` and `expected` have the same
[levels](https://rdrr.io/r/base/levels.html). If the levels differ

- `vec_check_levels()` returns a list describing the problem

- `vec_grade_levels()` returns a failing grade and informative message
  with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
vec_check_levels(object = .result, expected = .solution, env = parent.frame())

vec_grade_levels(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  env = parent.frame(),
  ...
)
```

## Arguments

- object:

  An object to be compared to `expected`.

- expected:

  An object containing the expected result.

- env:

  The environment in which to find `.result` and `.solution`.

- max_diffs:

  `[numeric(1)]`  
  The maximum number of missing and/or unexpected names to include in an
  informative failure message. Defaults to 3.

- ...:

  Arguments passed on to
  [`gradethis::fail`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

  `correct`

  :   A logical value of whether or not the checked code is correct.

  `type,location`

  :   The `type` and `location` of the feedback object provided to
      learnr. See
      <https://rstudio.github.io/learnr/exercises.html#Custom_checking>
      for more details.

      `type` may be one of "auto", "success", "info", "warning",
      "error", or "custom".

      `location` may be one of "append", "prepend", or "replace".

  `praise`

  :   Include a random praising phrase with
      [`random_praise()`](https://pkgs.rstudio.com/gradethis/reference/praise.html)?
      The default value of `praise` can be set using
      [`gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html)
      or the `gradethis.pass.praise` option.

  `hint`

  :   Include a code feedback hint with the failing message? This
      argument only applies to `fail()` and `fail_if_equal()` and the
      message is added using the default options of
      [`give_code_feedback()`](https://pkgs.rstudio.com/gradethis/reference/code_feedback.html)
      and
      [`maybe_code_feedback()`](https://pkgs.rstudio.com/gradethis/reference/code_feedback.html).
      The default value of `hint` can be set using
      [`gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html)
      or the `gradethis.fail.hint` option.

  `encourage`

  :   Include a random encouraging phrase with
      [`random_encouragement()`](https://pkgs.rstudio.com/gradethis/reference/praise.html)?
      The default value of `encourage` can be set using
      [`gradethis_setup()`](https://pkgs.rstudio.com/gradethis/reference/gradethis_setup.html)
      or the `gradethis.fail.encourage` option.

## Value

If there are any issues, a [list](https://rdrr.io/r/base/list.html) from
`vec_check_levels()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `vec_grade_levels()`. Otherwise, invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `levels_n`: `object` and `expected` have a different number of
    levels.

2.  `levels`: The object has levels that are not expected, or is missing
    levels that are expected.

3.  `levels_reversed`: The `levels` of `object` are in the opposite
    order of `expected`.

4.  `level_order`: The levels of `object` are not in the same order as
    `expected`.

## Examples

``` r
.result <- as.factor(rep_len(letters[1:3], 6))
.solution <- as.factor(rep_len(letters[1:2], 6))
vec_check_levels()
#> <tblcheck problem>
#> Your result should have 2 levels, but it has 3 levels.
#> $ type    : chr "levels_n"
#> $ expected: int 2
#> $ actual  : int 3
vec_grade_levels()
#> <gradethis_graded: [Incorrect]
#>   Your result should have 2 levels, but it has 3 levels.
#> >

.result <- as.factor(letters[1:6])
.solution <- as.factor(letters[21:26])
vec_check_levels()
#> <tblcheck problem>
#> Your result should have levels named `u`, `v`, `w`, and 3 more. Your result should not have levels named `a`, `b`, `c`, or 3 more.
#> $ type      : chr "levels"
#> $ missing   : chr [1:6] "u" "v" "w" "x" ...
#> $ unexpected: chr [1:6] "a" "b" "c" "d" ...
vec_grade_levels()
#> <gradethis_graded: [Incorrect]
#>   Your result should have levels named `u`, `v`, `w`, and 3
#>   more. Your result should not have levels named `a`, `b`,
#>   `c`, or 3 more.
#> >
vec_grade_levels(max_diffs = 5)
#> <gradethis_graded: [Incorrect]
#>   Your result should have levels named `u`, `v`, `w`, `x`,
#>   `y`, and 1 more. Your result should not have levels named
#>   `a`, `b`, `c`, `d`, `e`, or 1 more.
#> >
vec_grade_levels(max_diffs = Inf)
#> <gradethis_graded: [Incorrect]
#>   Your result should have levels named `u`, `v`, `w`, `x`,
#>   `y`, and `z`. Your result should not have levels named `a`,
#>   `b`, `c`, `d`, `e`, or `f`.
#> >
```
