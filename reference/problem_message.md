# Create a message from a problem object

`problem_message()` is an S3 generic that powers the conversion of
problems detected by
[`tbl_check()`](https://rstudio.github.io/tblcheck/reference/tbl_check.md),
[`vec_check()`](https://rstudio.github.io/tblcheck/reference/vec_check.md),
and their related helper functions into a human-readable message.

## Usage

``` r
problem_message(problem, ...)
```

## Arguments

- problem:

  An object with base class `gradethis_problem`. Problems identified by
  tblcheck also include `tblcheck_problem`, plus additional classes that
  more specifically identify the problem type.

- ...:

  Additional arguments passed to the underlying methods.

## Value

A length-1 character string with a message describing the problem.

## See also

Other Problem functions:
[`problem()`](https://rstudio.github.io/tblcheck/reference/problem.md),
[`problem_grade()`](https://rstudio.github.io/tblcheck/reference/problem_grade.md),
[`problem_type()`](https://rstudio.github.io/tblcheck/reference/problem_type.md)

## Examples

``` r
problem <- problem(
  type = "class",
  expected = "character",
  actual = "numeric",
  expected_length = 1,
  actual_length = 2
)

problem_message(problem)
#> Your result should be a text string (class `character`), but it is a text string (class `character`).
```
