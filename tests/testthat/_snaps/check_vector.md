# tbl_grade_vector() checks classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of integers (class `integer`), but it
        is a vector of text (class `character`).
      >

# tbl_grade_vector() checks the first three values

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your result should be `a`, `b`, and `c`.
      >

# tbl_grade_vector() checks multiple classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector with classes `test`, `class`, and
        `integer`, but it is a vector of integers (class `integer`).
      >

# tbl_grade_vector() checks for value differences beyond the first 3

    Code
      grade
    Output
      <gradethis_graded: [Incorrect] Your result contains unexpected values.>

# max_diffs modifies the number of values to print

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 5 values of your result should be `z`, `y`, `x`, `w`, and
        `v`.
      >

# max_diffs doesn't overflow

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 2 values of your result should be `b` and `a`.
      >

# checks that vectors have the same length

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should contain 4 values, but it has 3 values.
      >

# checks that vectors have the same names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should have the names `a`, `b`, and `c`. Your result
        should not have the names `x`, `y`, or `z`.
      >

