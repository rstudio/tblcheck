# Checks that two vectors are contain the same values

Check if two vectors contain the same values. If the values differ

- `vec_check_values()` returns a list describing the problem

- `vec_grade_values()` returns a failing grade and informative message
  with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
vec_check_values(
  object = .result,
  expected = .solution,
  tolerance = sqrt(.Machine$double.eps),
  env = parent.frame()
)

vec_grade_values(
  object = .result,
  expected = .solution,
  tolerance = sqrt(.Machine$double.eps),
  max_diffs = 3,
  env = parent.frame(),
  ...
)
```

## Arguments

- object:

  A vector to be compared to `expected`.

- expected:

  A vector containing the expected result.

- tolerance:

  `[numeric(1) ≥ 0]`  
  `values` differences smaller than `tolerance` are ignored. The default
  value is close to `1.5e-8`.

- env:

  The environment in which to find `.result` and `.solution`.

- max_diffs:

  `[numeric(1)]`  
  The maximum number of mismatched values to print. Defaults to 3.

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
`vec_check_values()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `vec_grade_values()`. Otherwise, invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `values`: `object` doesn't contain the same values as `expected`

## Examples

``` r
.result <- 1:10
.solution <- letters[1:10]
vec_check_values()
#> <tblcheck problem>
#> Your result should be a vector of text (class `character`), but it is a vector of integers (class `integer`).
#> $ type    : chr "class"
#> $ expected: chr [1:10] "a" "b" "c" "d" ...
#> $ actual  : int [1:10] 1 2 3 4 5 6 7 8 9 10
vec_grade_values()
#> <gradethis_graded: [Incorrect]
#>   Your result should be a vector of text (class `character`),
#>   but it is a vector of integers (class `integer`).
#> >

.result <- 1:10
.solution <- 1:11
vec_check_values()
#> <tblcheck problem>
#> Your result should contain 11 values, but it has 10 values. I expected your result to include the value `11`.
#> $ type           : chr "length"
#> $ expected       : int [1:11] 1 2 3 4 5 6 7 8 9 10 ...
#> $ actual         : int [1:10] 1 2 3 4 5 6 7 8 9 10
#> $ expected_length: int 11
#> $ actual_length  : int 10
vec_grade_values()
#> <gradethis_graded: [Incorrect]
#>   Your result should contain 11 values, but it has 10 values.
#>   I expected your result to include the value `11`.
#> >

.result <- 1:10
.solution <- rlang::set_names(1:10, letters[1:10])
vec_check_values()
vec_grade_values()
vec_grade_values(max_diffs = 5)
vec_grade_values(max_diffs = Inf)

.result <- 1:10
.solution <- 11:20
vec_check_values()
#> <tblcheck problem>
#> The first 3 values of your result should be `11`, `12`, and `13`, not `1`, `2`, and `3`.
#> $ type    : chr "values"
#> $ expected: int [1:10] 11 12 13 14 15 16 17 18 19 20
#> $ actual  : int [1:10] 1 2 3 4 5 6 7 8 9 10
vec_grade_values()
#> <gradethis_graded: [Incorrect]
#>   The first 3 values of your result should be `11`, `12`, and
#>   `13`, not `1`, `2`, and `3`.
#> >
vec_grade_values(max_diffs = 5)
#> <gradethis_graded: [Incorrect]
#>   The first 5 values of your result should be `11`, `12`,
#>   `13`, `14`, and `15`, not `1`, `2`, `3`, `4`, and `5`.
#> >
vec_grade_values(max_diffs = Inf)
#> <gradethis_graded: [Incorrect]
#>   The first 10 values of your result should be `11`, `12`,
#>   `13`, `14`, `15`, `16`, `17`, `18`, `19`, and `20`, not `1`,
#>   `2`, `3`, `4`, `5`, `6`, `7`, `8`, `9`, and `10`.
#> >
```
