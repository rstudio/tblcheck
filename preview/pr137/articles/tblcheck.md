# Get started with tblcheck

## Overview

### Introducing tblcheck

tblcheck works with [gradethis](https://rstudio.github.io/gradethis/) to
help instructors compare students’ exercise results with intended
solutions in [learnr](https://rstudio.github.io/learnr/) tutorials. If
you are new to grading learnr tutorials, we recommend that you [get
comfortable with
gradethis](https://rstudio.github.io/gradethis/articles/gradethis.html)
before incorporating tblcheck into your tutorials.

tblcheck provides four levels of grading:

1.  For fully automatic grading as a drop-in replacement for
    [`gradethis::grade_this()`](https://pkgs.rstudio.com/gradethis/reference/grade_this.html),
    use
    [`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
    or
    [`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md).
2.  To integrate tblcheck into existing grading code, you can use
    [`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
    or
    [`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
    for specific automated checks.
3.  You can directly call the [specialized individual grading
    functions](#specific-grading-functions) that power the higher-level
    functions.
4.  Finally, you can check for table and vector problems to [create your
    own custom feedback](#custom-mistake-handling).

The most common use case for tblcheck is to provide automatic feedback
for [common problems in tables](#grading-tables). Because the columns of
data frames in R are vectors, tblcheck can also provide feedback for
[common problems in vectors](#grading-vectors) with
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md).

These all-in-one grading functions check for a set of possible problems,
or differences, between the exercise solution and the student’s table or
vector. You can [control which checks are applied](#skipping-tests) with
the arguments of these functions, or you can [directly call individual
grading functions](#specific-grading-functions). The table grading
functions are prefixed with `tbl_grade_` and the vector grading
functions prefixed with `vec_grade_`.

For complete control over the feedback presented to users, each
`tbl_grade_` or `vec_grade_` function is paired with a `tbl_check_` or
`vec_check_` counterpart that finds problems and returns an object that
you can use to [create custom feedback](#custom-mistake-handling).

### Usage

To use tblcheck in a learnr tutorial, load tblcheck after learnr and
gradethis in the `setup` chunk of your tutorial:

```` markdown
```{r setup}
library(learnr)
library(gradethis)
library(tblcheck)
```
````

Then, ensure your exercise has a `-solution` chunk and choose one of the
following grading functions to grade your exercise:

- If the solution is a **table**, use
  [`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)

  ```` markdown
  ```{r ex-check}
  grade_this_table()
  ```
  ````

  or use
  [`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
  in existing grading code.

  ```` markdown
  ```{r ex-check}
  grade_this({
    pass_if_equal()
    tbl_grade()
    pass()
  })
  ```
  ````

- If the solution is a ***column* in a table**, use
  `grade_this_column()`

  ```` markdown
  ```{r ex-check}
  grade_this_column("eruptions")
  ```
  ````

  or use
  [`tbl_grade_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md)
  in existing grading code.

  ```` markdown
  ```{r ex-check}
  grade_this({
    pass_if_equal()
    tbl_grade_column()
    pass()
  })
  ```
  ````

- If the solution is a **vector**, use
  [`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)

  ```` markdown
  ```{r ex-check}
  grade_this_vector()
  ```
  ````

  or use
  [`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
  in existing grading code.

  ```` markdown
  ```{r ex-check}
  grade_this({
    pass_if_equal()
    vec_grade()
    pass()
  })
  ```
  ````

In each of the above cases, the fully automated first version is
functionally equivalent to the second version that uses
[`gradethis::grade_this()`](https://pkgs.rstudio.com/gradethis/reference/grade_this.html).

## Grading tables

### Automated table checking

[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
uses
[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
to compare the result of the user’s input to the result of the
`-solution` chunk, automatically returning targeted feedback to the user
if any problems are discovered.

[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
checks that the user’s table

1.  is the correct class,
2.  has the correct column names,
3.  has the correct number of rows and columns,
4.  and that each column of the user’s table
    1.  is the correct class and
    2.  contains the correct values.

If any of these checks detect a problem in the submitted code, the
student will see a single message with the first detected issue, based
on the order described above.

### Usage

To grade an exercise where the solution is a table, ensure you have a
`-solution` chunk and call
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
in your `-check` chunk

```` markdown
```{r example-check}
grade_this_table()
```
````

or add
[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
to the grading code in your `-check` chunk.

```` markdown
```{r example-check}
grade_this({
  pass_if_equal()
  tbl_grade()
  pass()
})
```
````

By default, `tblcheck` functions compare the `gradethis` objects
`.result` and `.solution`, just like
[`gradethis::pass_if_equal()`](https://pkgs.rstudio.com/gradethis/reference/pass_if_equal.html)
and
[`gradethis::fail_if_equal()`](https://pkgs.rstudio.com/gradethis/reference/pass_if_equal.html).

If you are using
[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md),
be sure to include a function like `pass()` or `pass_if_equal()` in your
checking code to ensure students can get a passing grade!

[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
only returns feedback to the student if it discovers a problem; if the
student gives the correct answer, it produces no output. This lets you
quickly check for simple problems, following up with more detailed
checking with other `gradethis` functions.

### Finding problems

If the user’s submitted table differs from the correct table,
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
returns a failing grade and a message with an explanation for what went
wrong. If there are multiple problems with a student’s submission,
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
tries to give the most actionable item first.

We’ll demonstrate how this works for a simple exercise that asks
students to create the following table using `tibble()`.

| food    | vegetable | color |
|:--------|:----------|:------|
| lettuce | TRUE      | green |
| tomato  | FALSE     | red   |

In the R Markdown source of the learnr tutorial, we use an exercise
chunk labelled `food`, with a `food-solution` chunk with the expected
solution and a `food-check` chunk with the exercise checking code using
[gradethis](https://rstudio.github.io/gradethis/) and tblcheck.

```` markdown
```{r food, exercise=TRUE}

```

```{r food-solution}
tibble(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, FALSE),
    color = c("green", "red")
)
```

```{r food-check}
grade_this_table()
```
````

We’ll use this example throughout the sections that follow to
demonstrate how tblcheck will respond to various types of errors that
students may make. Keep in mind this is a contrived example designed for
this vignette. In real-world usage, students are likely to only
encounter one or two of the problems
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
is designed to find.

#### Checking class

First,
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
ensures that the class of the student’s submission matches the class of
the expected solution. Here, the student attempts to store the data in
the table as a list rather than by using `tibble()`.

``` r

list(
    food = "lettuce",
    fruit = "TRUE",
    color = "green"
)
#> $food
#> [1] "lettuce"
#> 
#> $fruit
#> [1] "TRUE"
#> 
#> $color
#> [1] "green"
```

Your table should be a tibble (class `tbl_df`), but it is a list (class
`list`).

Based on this advice, the student revises their solution to use
`tibble()` instead of [`list()`](https://rdrr.io/r/base/list.html).

#### Checking column names

Next, the code checks that the student used the correct column names,
and they haven’t missed any columns or included any unexpected columns.
Here,
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
notices that the student has an unexpected column named `fruit`.

``` r

tibble(
    food = "lettuce",
    fruit = "TRUE",
    color = "green"
)
#> # A tibble: 1 × 3
#>   food    fruit color
#>   <chr>   <chr> <chr>
#> 1 lettuce TRUE  green
```

Your table should have a column named `vegetable`. Your table should not
have a column named `fruit`.

Based on this advice, the student revises their solution to name the
second column `vegetable` instead of `fruit`.

#### Checking length

Next,
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
checks that the student has submitted the correct number of rows, and in
this case notices that the student has only included one row.

``` r

tibble(
    food = "lettuce",
    vegetable = "TRUE",
    color = "green"
)
#> # A tibble: 1 × 3
#>   food    vegetable color
#>   <chr>   <chr>     <chr>
#> 1 lettuce TRUE      green
```

Your table should have 2 rows, but it has 1 row.

Based on this advice, the student realizes they’ve only entered the
first row of the table. They go back to the example table and add the
second row to their submission.

#### Checking column classes

Next,
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
checks that each individual column contains the correct type of data.
Here, the student has stored the values of the `vegetable` column as a
string, but we were expecting them to be logical values.

``` r

tibble(
    food = c("lettuce", "tomato"),
    vegetable = c("TRUE", "TRUE"),
    color = c("green", "red")
)
#> # A tibble: 2 × 3
#>   food    vegetable color
#>   <chr>   <chr>     <chr>
#> 1 lettuce TRUE      green
#> 2 tomato  TRUE      red
```

Your `vegetable` column should be a vector of `TRUE`/`FALSE` values
(class `logical`), but it is a vector of text (class `character`).

Based on this advice, the student removes the `"` around the values in
the `vegetable` column to use R’s logical `TRUE`.

#### Checking column values

Finally,
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
gives a hint as to what the values in each column should look like.
Here, the student made a mistake during their transcription of the
`vegetable` column.

``` r

tibble(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, TRUE),
    color = c("green", "red")
)
#> # A tibble: 2 × 3
#>   food    vegetable color
#>   <chr>   <lgl>     <chr>
#> 1 lettuce TRUE      green
#> 2 tomato  TRUE      red
```

The first 2 values of your `vegetable` column should be `TRUE` and
`FALSE`, not `TRUE` and `TRUE`.

Based on this advice, the student revises their submission, changing the
second value of the `vegetable` column from `TRUE` to `FALSE`.

``` r

tibble(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, FALSE),
    color = c("green", "red")
)
#> # A tibble: 2 × 3
#>   food    vegetable color
#>   <chr>   <lgl>     <chr>
#> 1 lettuce TRUE      green
#> 2 tomato  FALSE     red
```

That’s a first-class answer! Correct!

## Grading vectors

### Automated vector checking

Many of the table-grading tests that apply to the *columns* of tables
can also be applied to vectors — after all, data frame columns in R are
*vectors*.

When your exercise uses vectors rather than tables,
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
and
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
allows you to apply the same tests that are normally applied to the
columns of a table to a vector. They check that the user’s vector

1.  is the correct class
2.  is the correct length
3.  has the correct factor levels (if the vector is a factor)
4.  contains the correct values
5.  has the correct names (if the vector has names)

Like
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md),
if a problem is detected by any of these checks, the student will see a
single message with the first detected problem, based on the order
described above.

### Usage

To grade an exercise where the solution is a vector, ensure you have a
`-solution` chunk and call
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
in your `-check` chunk

```` markdown
```{r vector-check}
grade_this_vector()
```
````

or add
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
to the grading code in your `-check` chunk, e.g.

```` markdown
```{r vector-check}
grade_this({
  pass_if_equal()
  vec_grade()
  pass()
})
```
````

Just like
[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
and other tblcheck functions,
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
automatically compares the user’s `.result` to the `.solution` when used
in
[`gradethis::grade_this()`](https://pkgs.rstudio.com/gradethis/reference/grade_this.html).

While
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
always returns a passing or failing grade, note that
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
only returns feedback when a problem is detected. Be sure to include
[`gradethis::pass()`](https://pkgs.rstudio.com/gradethis/reference/graded.html)
or
[`gradethis::pass_if_equal()`](https://pkgs.rstudio.com/gradethis/reference/pass_if_equal.html)
when using
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
to ensure that students can get a passing grade.

### Finding problems

If the user’s submitted vector differs from the correct vector,
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
returns a failing grade and a message with an explanation for what went
wrong. If there are multiple problems with a student’s submission,
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
tries to give the most actionable item first.

Suppose an exercise asks a student to create a factor of the sandwich
toppings — *lettuce*, *tomato*, *avocado*.

``` r

factor(c("lettuce", "tomato", "avocado"))
#> [1] lettuce tomato  avocado
#> Levels: avocado lettuce tomato
```

In the R Markdown source of the learnr tutorial, we use an exercise
chunk labelled `toppings`, with a `toppings-solution` chunk with the
expected solution and a `toppings-check` chunk with the exercise
checking code using [gradethis](https://rstudio.github.io/gradethis/)
and tblcheck.

```` markdown
```{r toppings, exercise=TRUE}

```

```{r toppings-solution}
factor(c("lettuce", "tomato", "avocado"))
```

```{r toppings-check}
grade_this_vector()
```
````

For example, if the student submits a vector of the wrong class, that
will be the first message returned by
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md).

``` r

c("lettuce", "tomato", "avocado")
#> [1] "lettuce" "tomato"  "avocado"
```

Your result should be a vector of factors (class `factor`), but it is a
vector of text (class `character`).

If the student submits a factor with the wrong factor levels,
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
will warn the student about their mistake.

``` r

factor(c("lettuce", "tomato", "avocado"), c("lettuce", "tomato", "avocado"))
#> [1] lettuce tomato  avocado
#> Levels: lettuce tomato avocado
```

Your result’s levels were not in the expected order. The first 3 levels
of your result should be `avocado`, `lettuce`, and `tomato`, but they
were `lettuce`, `tomato`, and `avocado`.

## Custom Grading

There are a number of ways to control which mistakes are detected and
how the feedback is given to the students.

- The first is to [enable or disable specific checks](#skipping-tests)
  using the `check_*` arguments of
  [`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
  and
  [`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
  (or their counterparts,
  [`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
  and
  [`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)).

- Both
  [`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
  and
  [`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
  include `pre_check` and `post_check` options that allow you to add
  [additional tests and logic to the grading code](#additional-checks).

- You may also choose to [call specific grading
  functions](#specific-grading-functions) associated with the checks
  underlying
  [`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
  and
  [`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md).

- Or you can [`check` rather than `grade` for specific
  problems](#checking-for-problems-with-custom-feedback) to obtain a
  `problem` object, i.e. a description of the problem found by tblcheck.
  You can then use the problem object to construct a feedback message
  using
  [`gradethis::fail()`](https://pkgs.rstudio.com/gradethis/reference/graded.html).

### Skipping tests

Every test performed by
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
and
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
(or
[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
and
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md))
can be enabled or disabled with an argument. The argument names are
prefixed with `check_` — such as `check_class` or `check_groups` — and
each take a `TRUE` or `FALSE` value.

For example, suppose a student answering our `food` exercise used a
`data.frame` when the exercise expects a `tibble`.

``` r

data.frame(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, FALSE),
    color = c("green", "red"),
    stringsAsFactors = FALSE
)
#>      food vegetable color
#> 1 lettuce      TRUE green
#> 2  tomato     FALSE   red
```

Your table should be a tibble (class `tbl_df`), but it is a data frame
(class `data.frame`).

If you don’t care about the class of the table, you can add
`check_class = FALSE` to
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md).
This will skip checking the table’s class, but still run all other
tests.

```` markdown
```{r food-check}
grade_this_table(check_class = FALSE)
```
````

Super job! Correct!

Since the only problem with the student’s submission was the class of
the table,
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
doesn’t directly return any feedback.

### Additional Checks

Both
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
and
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md)
provide two additional arguments, `pre_check` and `post_check`, that
allow you to add additional checks or modify the `.result` or
`.solution`.

For both functions, the
[`gradethis::grade_this()`](https://pkgs.rstudio.com/gradethis/reference/grade_this.html)
flow is roughly equivalent to the following code sketch:

``` r

grade_this({
  # ... pre_check ...
  
  # if requested
  pass_if_equal()
  
  # grade the table or vector
  tbl_grade()
  
  # ... post_check ...
  
  pass()
})
```

Two examples of reasons why you might want to use these arguments are to
limit the table grading checks to specific columns only, or to include
additional checks after
[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
or
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md).

Suppose we extend our `food` example into an exercise labelled
`food-percentage` that adds a `count` column to our `foods` table and
asks students to add a new column, `pct` with the percentage of our
total food is represented by each food.

```` markdown
```{r food-percentage-setup}
library(dplyr)

foods <- tibble(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, FALSE),
    color = c("green", "red"),
    count = c(5, 3)
)
```

```{r food-percentage, exercise=TRUE}

```

```{r food-percentage-solution}
.solution <-
    foods %>%
    mutate(pct = count / sum(count))
```
````

We expect the final solution to look like this

but a student might decide to store the total food in a temporary
`total` column.

``` r

.result <- foods %>%
    mutate(
        total = sum(count),
        pct = count / total
    )
```

Knowing that we don’t mind the additional column, we can use the
`pre_check` argument to limit `.result` to the columns that also appear
in `.solution`.

```` markdown

``` r
grade_this_table(pre_check = {
    tbl_grade_is_table(.result)
    .result <- .result[intersect(names(.result), names(.solution))]
})
```
````

Awesome! Correct!

### Specific grading functions

[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
and
[`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md)
calls a number of grading functions internally. You can call these
functions directly to perform more specific grading, either in the
`pre_check` or `post_check` arguments of
[`grade_this_table()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_table.md)
or
[`grade_this_vector()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/grade_this_vector.md),
or in standard
[`gradethis::grade_this()`](https://pkgs.rstudio.com/gradethis/reference/grade_this.html)
grading code.

| Function | Grades |
|----|----|
| [`tbl_grade_class()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_class.md) [`vec_grade_class()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_class.md) | the class of an object |
| [`tbl_grade_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md) | applies the tests in [`vec_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check.md) to a single column of a table |
| [`tbl_grade_dimensions()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md) [`vec_grade_dimensions()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_dimensions.md) | the length and dimensions of an object |
| [`tbl_grade_groups()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_groups.md) | the groups of a table |
| [`tbl_grade_names()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md) [`vec_grade_names()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_names.md) | the names of an object |
| [`vec_grade_levels()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check_levels.md) | the levels of a factor |
| [`vec_grade_values()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/vec_check_values.md) | the values of a vector |

Suppose we modified our `food` example, telling students that we have 3
tomatoes and 5 heads of lettuce. We’d like the students to create a
fourth column `count` containing the number of each food item in our
possession. For this example, we’ll use the lower-level functions in
conjunction with
[`gradethis::grade_this()`](https://pkgs.rstudio.com/gradethis/reference/grade_this.html).

```` markdown
```{r food-count-setup}
library(dplyr)

foods <- tibble(
    food = c("lettuce", "tomato"),
    vegetable = c(TRUE, FALSE),
    color = c("green", "red")
)
```


```{r food-count, exercise=TRUE}

```

```{r food-count-solution}
foods %>%
    mutate(count = c(5, 3))
```
````

In our grading code, we may choose to grade only the `count` column of
`foods` using
[`tbl_grade_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md),
ignoring the other columns since they were provided by our setup code.

```` markdown
```{r food-count-check}
grade_this({
  tbl_grade_column("count")
  pass_if_equal()
  fail()
})
```
````

A student who quickly scanned the exercise prompt might reverse the
expected order of the values in the `count` column.

``` r

foods %>%
    mutate(count = c(3, 5))
#> # A tibble: 2 × 4
#>   food    vegetable color count
#>   <chr>   <lgl>     <chr> <dbl>
#> 1 lettuce TRUE      green     3
#> 2 tomato  FALSE     red       5
```

The first 2 values of your `count` column should be `5` and `3`, not `3`
and `5`.

### Custom mistake handling

Sometimes, we want to handle specific circumstance in a special way.
Every `tbl_grade_` and `vec_grade_` function includes a `tbl_check_` or
`vec_check_` counterpart that returns the detected **problem** rather
than converting the problem into feedback for the user (a **grade** in
gradethis terms).

If we replace
[`tbl_grade_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md)
with
[`tbl_check_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md),
we can store and inspect the problem detected by the column checking
function. We’ll experiment in our local R console before writing our
final exercise checking code.

``` r

solution <- foods %>% mutate(count = c(5, 3))
user <- foods %>% mutate(count = c(3, 5))

problem <- tbl_check_column("count", object = user, expected = solution)

problem
#> <tblcheck problem>
#> The first 2 values of your `count` column should be `5` and `3`, not `3` and `5`.
#> $ type    : chr "values"
#> $ expected: num [1:2] 5 3
#> $ actual  : num [1:2] 3 5
#> $ location: chr "column"
#> $ column  : chr "count"
```

Every problem object contains at least three items:

1.  The problem `type` describes the issue discovered by the checking
    function. The help pages for every check function contain a section
    named **Problems** where the problem types detected by the check
    function are enumerated.

    - **Your Turn:** What types of problems are discovered by
      [`tbl_check_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md)?
      Use the help pages to find out.

2.  `actual` contains the value returned by the user’s code and
    inspected by the check function.

3.  `expected` contains the value returned by the solution code and
    inspected by the check function.

Problems also include additional information depending on the problem
type. In the case of a `values` problem detected by
[`tbl_check_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md),
the problem object also includes the `column` name.

tblcheck includes a helper function,
[`is_problem()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_type.md)
that you can use to detect and differentiate between different problem
types.

``` r

is_problem(problem)
#> [1] TRUE
```

We can use the `type` argument of
[`is_problem()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_type.md)
to differentiate between the problem types detected by
[`tbl_check_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md).

``` r

is_problem(problem, type = "length")
#> [1] FALSE
is_problem(problem, type = "values")
#> [1] TRUE
```

In this exercise, we know in advance that our wording is likely to trip
up students, so we may want to create feedback specifically for the case
where a student has reversed the food counts. We can use
[`is_problem()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/problem_type.md)
together with [`all.equal()`](https://rdrr.io/r/base/all.equal.html) to
isolate this specific case.

``` r

if (is_problem(problem, "values") && all.equal(problem$actual, c(3, 5))) {
    feedback <- paste(
        "Make sure that the values in `count` are ordered",
        "to match their respective `food`.",
        "Remember, we have **3** tomatoes and **5** heads of lettuce."
    )
    gradethis::fail(feedback)
}
#> <gradethis_graded: [Incorrect]
#>   Make sure that the values in `count` are ordered to match
#>   their respective `food`. Remember, we have **3** tomatoes
#>   and **5** heads of lettuce.
#> >
```

For problems not handled by your custom grading code, you can pass the
problem to
[`tbl_grade()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check.md)
to create a grade with the default feedback provided by tblcheck’s
`grade` functions. If there are no problems, `problem_grade(problem)`
won’t return anything.

Here’s the default feedback
[`tbl_grade_column()`](https://rstudio.github.io/tblcheck/preview/pr137/reference/tbl_check_column.md)
*would have returned* without our custom grading code.

``` r

problem_grade(problem)
#> <gradethis_graded: [Incorrect]
#>   The first 2 values of your `count` column should be `5` and
#>   `3`, not `3` and `5`.
#> >
```

**Tip**: You can also use `if` statements to ignore differences that you
don’t care about in your grading code.

Putting everything together into our grading `food-count-check` chunk,
our grading code for this exercise would look like this:

```` markdown
```{r food-count-check}
grade_this({
  problem <- tbl_check_column("count")
  
  if (is_problem(problem, "values") && all.equal(problem$actual, c(3, 5))) {
    feedback <- paste(
        "Make sure that the values in `count` are ordered",
        "to match their respective `food`.",
        "Remember, we have **3** tomatoes and **5** heads of lettuce."
    )
    fail(feedback)
  }
  
  problem_grade(problem)
  pass("Great job!")
})
```
````

And the student who reversed the `count` column values

``` r

foods %>%
    mutate(count = c(3, 5))
#> # A tibble: 2 × 4
#>   food    vegetable color count
#>   <chr>   <lgl>     <chr> <dbl>
#> 1 lettuce TRUE      green     3
#> 2 tomato  FALSE     red       5
```

would receive our custom feedback.

Make sure that the values in `count` are ordered to match their
respective `food`. Remember, we have **3** tomatoes and **5** heads of
lettuce.

By following our specific advice, the student revises their code to
correctly create the `count` column.

``` r

foods %>%
    mutate(count = c(5, 3))
#> # A tibble: 2 × 4
#>   food    vegetable color count
#>   <chr>   <lgl>     <chr> <dbl>
#> 1 lettuce TRUE      green     5
#> 2 tomato  FALSE     red       3
```

Great job!
