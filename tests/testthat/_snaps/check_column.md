# tbl_grade_column() checks classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column should be a vector of integers (class `integer`), but
        it is a vector of text (class `character`).
      >

---

    structure(list(type = "column_class", expected = "integer", actual = "character", 
        expected_length = 3L, actual_length = 26L, column = "a"), class = c("column_class_problem", 
    "vector_class_problem", "class_problem", "tblcheck_problem", 
    "gradethis_problem"))

# tbl_grade_column() checks the first three values

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your `a` column should be `a`, `b`, and `c`.
      >

---

    structure(list(type = "column_value_diffs", expected = c("a", 
    "b", "c"), column = "a"), class = c("column_value_diffs_problem", 
    "vector_value_diffs_problem", "value_diffs_problem", "tblcheck_problem", 
    "gradethis_problem"))

# tbl_grade_column() checks multiple classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column should be a tibble (class `tbl_df`), but it is a data
        frame (class `data.frame`).
      >

---

    structure(list(type = "column_class", expected = c("tbl_df", 
    "tbl", "data.frame"), actual = "data.frame", expected_length = 1L, 
        actual_length = 1L, column = "a"), class = c("column_class_problem", 
    "vector_class_problem", "class_problem", "tblcheck_problem", 
    "gradethis_problem"))

# tbl_grade_column() checks for value differences beyond the first 3

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column contains unexpected values.
      >

---

    structure(list(type = "column_values", column = "a"), class = c("column_values_problem", 
    "vector_values_problem", "values_problem", "tblcheck_problem", 
    "gradethis_problem"))

# max_diffs modifies the number of values to print

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 5 values of your `a` column should be `z`, `y`, `x`, `w`,
        and `v`.
      >

---

    structure(list(type = "column_value_diffs", expected = c("z", 
    "y", "x", "w", "v"), column = "a"), class = c("column_value_diffs_problem", 
    "vector_value_diffs_problem", "value_diffs_problem", "tblcheck_problem", 
    "gradethis_problem"))

# max_diffs doesn't overflow

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 2 values of your `a` column should be `b` and `a`.
      >

---

    structure(list(type = "column_value_diffs", expected = c("b", 
    "a"), column = "a"), class = c("column_value_diffs_problem", 
    "vector_value_diffs_problem", "value_diffs_problem", "tblcheck_problem", 
    "gradethis_problem"))

# checks that columns have the same length

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your `a` column should contain 4 values, but it has 3 values.
      >

---

    structure(list(type = "column_length", expected = 4L, actual = 3L, 
        column = "a"), class = c("column_length_problem", "vector_length_problem", 
    "length_problem", "tblcheck_problem", "gradethis_problem"))

# checks that the column is present in object

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have a column named `a`.
      >

---

    structure(list(type = "column_missing", expected = "a", column = "a"), class = c("column_missing_problem", 
    "missing_problem", "tblcheck_problem", "gradethis_problem"))

