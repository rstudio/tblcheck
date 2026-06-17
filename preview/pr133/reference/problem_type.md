# Problem helper functions

- `problem_type()` returns a problem's type, or
  [`NULL`](https://rdrr.io/r/base/NULL.html) if the input is not a
  problem.

- `is_problem()` tests whether an object is a `gradethis` problem.

- `is_tblcheck_problem()` tests whether an object is a problem created
  by `tblcheck`.

- `as_problem()` converts a list to a `tblcheck_problem`.

## Usage

``` r
problem_type(x)

is_problem(x, type = NULL)

is_tblcheck_problem(x, type = NULL)

as_problem(x)
```

## Arguments

- x:

  An object

- type:

  `[character(1)]`  
  A `problem` type

## Value

`is_problem()` and `is_tblcheck_problem()` return a
[logical](https://rdrr.io/r/base/logical.html)

of length 1. `problem_type()` returns a
[character](https://rdrr.io/r/base/character.html) of length 1.
`as_problem()` returns a `tblcheck_problem`.

## Details

If `type` is specified, `is_problem()` and `is_tblcheck_problem()` test
whether an object is a problem of the specified type.

## See also

Other Problem functions:
[`problem_grade`](https://rstudio.github.io/tblcheck/preview/pr133/reference/problem_grade.md)`()`,
[`problem_message`](https://rstudio.github.io/tblcheck/preview/pr133/reference/problem_message.md)`()`,
[`problem`](https://rstudio.github.io/tblcheck/preview/pr133/reference/problem.md)`()`

## Examples

``` r
problem_type(vec_check(1, "1"))
#> [1] "class"
is_problem(vec_check(1, "1"), "vector_class")
#> [1] FALSE
is_tblcheck_problem(vec_check(1, "1"), "class")
#> [1] TRUE
```
