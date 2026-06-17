# Checks that an object is a table

Checks if `object` inherits the
[data.frame](https://rdrr.io/r/base/data.frame.html) class. If the not

- `tbl_check_is_table()` returns a list describing the problem

- `tbl_grade_is_table()` returns a failing grade and informative message
  with
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)

## Usage

``` r
tbl_check_is_table(object = .result, env = parent.frame())

tbl_grade_is_table(object = .result, env = parent.frame(), ...)
```

## Arguments

- object:

  An object to be compared to `expected`.

- env:

  The environment in which to find `.result` and `.solution`.

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
`tbl_check_is_table()` or a
[`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
message from `tbl_grade_is_table()`. Otherwise, invisibly returns
[`NULL`](https://rdrr.io/r/base/NULL.html).

## Problems

1.  `not_table`: The object is not a table

## Examples

``` r
.result <- data.frame(a = 1:10)
tbl_check_is_table()
tbl_grade_is_table()

.result <- tibble::tibble(a = 1:10)
tbl_check_is_table()
tbl_grade_is_table()

.result <- list(a = 1:10)
tbl_check_is_table()
#> <tblcheck problem>
#> Your result should be a table, but it is a list (class `list`).
#> $ type  : chr "not_table"
#> $ actual:List of 1
#>  ..$ a: int [1:10] 1 2 3 4 5 6 7 8 9 10
tbl_grade_is_table()
#> <gradethis_graded: [Incorrect]
#>   Your result should be a table, but it is a list (class
#>   `list`).
#> >
```
