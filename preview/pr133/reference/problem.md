# Declare a problem

Useful for constructing a small list to communicate the problem that was
discovered during checking.

## Usage

``` r
problem(
  type,
  expected,
  actual,
  ...,
  .class = c(paste0(type, "_problem"), "tblcheck_problem")
)
```

## Arguments

- type:

  A character string, e.g. `column_values` or `table_rows`, that
  describes the problem that was discovered.

- expected, actual:

  The expected and actual values. These should be included when the
  value is a summary, e.g. `nrow(expected)` or `length(actual)`. Be
  careful not to include large amounts of data.

- ...:

  Additional elements to be included in the `problem` object.

- .class:

  The class of the problem. Typically, we expect the problem class to be
  `<type>_problem`, but if you are building custom classes you may set
  these classes as desired.

## Value

Returns a problem with class `<type>_problem` and the base classes
`tblcheck_problem` and `gradethis_problem`.

## See also

Other Problem functions:
[`problem_grade`](https://rstudio.github.io/tblcheck/preview/pr133/reference/problem_grade.md)`()`,
[`problem_message`](https://rstudio.github.io/tblcheck/preview/pr133/reference/problem_message.md)`()`,
[`problem_type`](https://rstudio.github.io/tblcheck/preview/pr133/reference/problem_type.md)`()`

## Examples

``` r
problem(
  type = "class",
  expected = "character",
  actual = "numeric",
  expected_length = 1,
  actual_length = 2
)
#> <tblcheck problem>
#> Your result should be a text string (class `character`), but it is a text string (class `character`).
#> $ type           : chr "class"
#> $ expected       : chr "character"
#> $ actual         : chr "numeric"
#> $ expected_length: num 1
#> $ actual_length  : num 2
```
