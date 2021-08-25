# tbl_grade_vector() checks classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of integers (class `integer`), but it
        is a vector of text (class `character`).
      >

---

    structure(list(type = "vector_class", expected = "integer", actual = "character", 
        expected_length = 3L, actual_length = 26L), class = c("vector_class_problem", 
    "class_problem", "tblcheck_problem", "gradethis_problem"))

# tbl_grade_vector() checks the first three values

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your result should be `a`, `b`, and `c`.
      >

---

    structure(list(type = "vector_value_diffs", expected = c("a", 
    "b", "c")), class = c("vector_value_diffs_problem", "value_diffs_problem", 
    "tblcheck_problem", "gradethis_problem"))

# tbl_grade_vector() checks multiple classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector with classes `test`, `class`, and
        `integer`, but it is a vector of integers (class `integer`).
      >

---

    structure(list(type = "vector_class", expected = c("test", "class", 
    "integer"), actual = "integer", expected_length = 10L, actual_length = 10L), class = c("vector_class_problem", 
    "class_problem", "tblcheck_problem", "gradethis_problem"))

# tbl_grade_vector() checks for value differences beyond the first 3

    Code
      grade
    Output
      <gradethis_graded: [Incorrect] Your result contains unexpected values.>

---

    structure(list(type = "vector_values"), class = c("vector_values_problem", 
    "values_problem", "tblcheck_problem", "gradethis_problem"))

# max_diffs modifies the number of values to print

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 5 values of your result should be `z`, `y`, `x`, `w`, and
        `v`.
      >

---

    structure(list(type = "vector_value_diffs", expected = c("z", 
    "y", "x", "w", "v")), class = c("vector_value_diffs_problem", 
    "value_diffs_problem", "tblcheck_problem", "gradethis_problem"
    ))

# max_diffs doesn't overflow

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 2 values of your result should be `b` and `a`.
      >

---

    structure(list(type = "vector_value_diffs", expected = c("b", 
    "a")), class = c("vector_value_diffs_problem", "value_diffs_problem", 
    "tblcheck_problem", "gradethis_problem"))

# checks that vectors have the same length

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should contain 4 values, but it has 3 values.
      >

---

    structure(list(type = "vector_length", expected = 4L, actual = 3L), class = c("vector_length_problem", 
    "length_problem", "tblcheck_problem", "gradethis_problem"))

# checks that vectors have the same names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should have the names `a`, `b`, and `c`. Your result
        should not have the names `x`, `y`, or `z`.
      >

---

    structure(list(type = "vector_names", missing = c("a", "b", "c"
    ), unexpected = c("x", "y", "z")), class = c("vector_names_problem", 
    "names_problem", "tblcheck_problem", "gradethis_problem"))

