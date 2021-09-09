# vec_grade_vector() checks classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of integers (class `integer`), but it
        is a vector of text (class `character`).
      >

# vec_grade_vector() checks the first three values

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your result should be `a`, `b`, and `c`.
      >

# vec_grade_vector() checks multiple classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector with classes `test`, `class`, and
        `integer`, but it is a vector of integers (class `integer`).
      >

# vec_grade_vector() checks for value differences beyond the first 3

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

# number of levels

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should have 3 levels, but it has 2 levels.
      >

# level labels

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should have levels named `x`, `y`, and `z`. Your result
        should not have levels named `a`, `b`, or `c`.
      >

# level order

    Code
      grade_reverse
    Output
      <gradethis_graded: [Incorrect]
        Your result's levels were not in the expected order. The order of the
        levels should be reversed.
      >

---

    Code
      grade_diffs
    Output
      <gradethis_graded: [Incorrect]
        Your result's levels were not in the expected order. The first 3
        levels of your result should be `c`, `a`, and `b`.
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result's levels were not in the expected order.
      >

