# Check that the groups of two object are the same

Checks if `object` and `expected` have the same
[groups](https://dplyr.tidyverse.org/reference/group_by.html). If the
groups differ

- `tbl_check_groups()` returns a list describing the problem

- `tbl_grade_groups()` returns a failing grade and informative message
  with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
tbl_check_groups(object = .result, expected = .solution, env = parent.frame())

tbl_grade_groups(
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
`tbl_check_groups()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `tbl_grade_groups()`. Otherwise, invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `groups`: The object has groups that are not expected, or is missing
    groups that are expected.

## Examples

``` r
.result <- dplyr::group_by(tibble::tibble(a = 1:10, b = 11:20), a)
.solution <- dplyr::group_by(tibble::tibble(a = 1:10, b = 11:20), b)
tbl_check_groups()
#> <tblcheck problem>
#> Your table should be grouped by `b`. Your table should not be grouped by `a`. 
#> $ type      : chr "groups"
#> $ missing   : chr "b"
#> $ unexpected: chr "a"
#> $ location  : chr "table"
tbl_grade_groups()
#> <gradethis_graded: [Incorrect]
#>   Your table should be grouped by `b`. Your table should not
#>   be grouped by `a`.
#> >
```
