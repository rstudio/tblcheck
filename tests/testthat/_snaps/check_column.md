# tbl_grade_column() checks classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column should be a vector of integers (class `integer`), but
        it is a vector of text (class `character`).
      >

# tbl_grade_column() checks the first three values

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your `a` column should be `a`, `b`, and `c`.
      >

# tbl_grade_column() checks multiple classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column should be a tibble (class `tbl_df`), but it is a data
        frame (class `data.frame`).
      >

# tbl_grade_column() checks for value differences beyond the first 3

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column contains unexpected values.
      >

# max_diffs modifies the number of values to print

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 5 values of your `a` column should be `z`, `y`, `x`, `w`,
        and `v`.
      >

# max_diffs doesn't overflow

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 2 values of your `a` column should be `b` and `a`.
      >

# checks that columns have the same length

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column should contain 4 values, but it has 3 values. I
        expected your result to include the value `d`.
      >

# checks that the column is present in object

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have a column named `a`.
      >

# number of levels

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column should have 3 levels, but it has 2 levels.
      >

# level labels

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column should have levels named `x`, `y`, and `z`. Your `a`
        column should not have levels named `a`, `b`, or `c`.
      >

# level order

    Code
      grade_reverse
    Output
      <gradethis_graded: [Incorrect]
        The order of the levels in your `a` column are the reverse of the
        expected order.
      >

---

    Code
      grade_diffs
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column's levels were not in the expected order. The first 3
        levels of your `a` column should be `c`, `a`, and `b`, but they were
        `a`, `b`, and `c`.
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column's levels were not in the expected order.
      >

