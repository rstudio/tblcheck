# Check that the names of two object are the same

Checks if `object` and `expected` have the same
[names](https://rdrr.io/r/base/names.html). If the names differ

- `tbl_check_names()` and `vec_check_names()` returns a list describing
  the problem

- `tbl_grade_names()` and `vec_grade_names()` returns a failing grade
  and informative message with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
tbl_check_names(
  object = .result,
  expected = .solution,
  check_order = TRUE,
  env = parent.frame()
)

vec_check_names(
  object = .result,
  expected = .solution,
  check_order = TRUE,
  env = parent.frame()
)

tbl_grade_names(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_order = TRUE,
  env = parent.frame(),
  ...
)

vec_grade_names(
  object = .result,
  expected = .solution,
  max_diffs = 3,
  check_order = TRUE,
  env = parent.frame(),
  ...
)
```

## Arguments

- object:

  An object to be compared to `expected`.

- expected:

  An object containing the expected result.

- check_order:

  `[logical(1)]`  
  Whether to check that the names of `object` and `expected` are in the
  same order.

- env:

  The environment in which to find `.result` and `.solution`.

- max_diffs:

  `[numeric(1)]`  
  The maximum number of missing and/or unexpected names to include in an
  informative failure message. Defaults to 3.

- ...:

  Arguments passed on to
  [`gradethis::fail`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

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
`tbl_check_names()` and `vec_check_names()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `tbl_grade_names()` and `vec_grade_names()`. Otherwise,
invisibly returns [`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `names`: The object has names that are not expected, or is missing
    names that are expected.

2.  `names_order`: The object has the same names as expected, but in a
    different order.

## Examples

``` r
.result <- c(1, 2, 3, 4, 5, 6, 7)
.solution <- c(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7)
vec_check_names()
#> <tblcheck problem>
#> Your result should have the names `a`, `b`, `c`, and 4 more. 
#> $ type      : chr "names"
#> $ missing   : chr [1:7] "a" "b" "c" "d" ...
#> $ unexpected: chr(0) 
vec_grade_names()
#> <gradethis_graded: [Incorrect]
#>   Your result should have the names `a`, `b`, `c`, and 4 more.
#> >
vec_grade_names(max_diffs = 5)
#> <gradethis_graded: [Incorrect]
#>   Your result should have the names `a`, `b`, `c`, `d`, `e`,
#>   and 2 more.
#> >
vec_grade_names(max_diffs = Inf)
#> <gradethis_graded: [Incorrect]
#>   Your result should have the names `a`, `b`, `c`, `d`, `e`,
#>   `f`, and `g`.
#> >

.result <- tibble::tibble(a = 1:5, b = 6:10, c = 11:15)
.solution <- tibble::tibble(a = 1:5, x = 6:10, y = 11:15)
tbl_check_names()
#> <tblcheck problem>
#> Your table should have columns named `x` and `y`. Your table should not have columns named `b` or `c`.
#> $ type      : chr "names"
#> $ missing   : chr [1:2] "x" "y"
#> $ unexpected: chr [1:2] "b" "c"
#> $ location  : chr "table"
tbl_grade_names()
#> <gradethis_graded: [Incorrect]
#>   Your table should have columns named `x` and `y`. Your table
#>   should not have columns named `b` or `c`.
#> >
```
