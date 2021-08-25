# tbl_grade_table() class

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should be a tibble (class `tbl_df`), but it is a data
        frame (class `data.frame`).
      >

---

    structure(list(type = "table_class", expected = c("tbl_df", "tbl", 
    "data.frame"), actual = "data.frame", expected_length = 2L, actual_length = 2L), class = c("table_class_problem", 
    "class_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table isn't a grouped data frame, but I was expecting it to be
        grouped. Maybe you need to use `group_by()`?
      >

---

    structure(list(type = "table_class", expected = c("grouped_df", 
    "tbl_df", "tbl", "data.frame"), actual = c("tbl_df", "tbl", "data.frame"
    ), expected_length = 2L, actual_length = 2L), class = c("table_class_problem", 
    "class_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table is a rowwise data frame, but I wasn't expecting it to be
        rowwise. Maybe you need to use `ungroup()`?
      >

---

    structure(list(type = "table_class", expected = c("tbl_df", "tbl", 
    "data.frame"), actual = c("rowwise_df", "tbl_df", "tbl", "data.frame"
    ), expected_length = 2L, actual_length = 2L), class = c("table_class_problem", 
    "class_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table is a rowwise data frame, but I was expecting it to be
        grouped. Maybe you need to use `group_by()`?
      >

---

    structure(list(type = "table_class", expected = c("grouped_df", 
    "tbl_df", "tbl", "data.frame"), actual = c("rowwise_df", "tbl_df", 
    "tbl", "data.frame"), expected_length = 2L, actual_length = 2L), class = c("table_class_problem", 
    "class_problem", "tblcheck_problem", "gradethis_problem"))

# tbl_grade_table() rows

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 25 rows, but it has 26 rows.
      >

---

    structure(list(type = "table_nrow", expected = 25L, actual = 26L), class = c("table_nrow_problem", 
    "nrow_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 1 row, but it has 26 rows.
      >

---

    structure(list(type = "table_nrow", expected = 1L, actual = 26L), class = c("table_nrow_problem", 
    "nrow_problem", "tblcheck_problem", "gradethis_problem"))

# tbl_grade_table() ncol

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 2 columns, but it has 3 columns.
      >

---

    structure(list(type = "table_ncol", expected = 2L, actual = 3L), class = c("table_ncol_problem", 
    "ncol_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 1 column, but it has 3 columns.
      >

---

    structure(list(type = "table_ncol", expected = 1L, actual = 3L), class = c("table_ncol_problem", 
    "ncol_problem", "tblcheck_problem", "gradethis_problem"))

# tbl_grade_table() names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x` and `y`. Your table should
        not have columns named `a` or `b`.
      >

---

    structure(list(type = "table_names", missing = c("x", "y"), unexpected = c("a", 
    "b")), class = c("table_names_problem", "names_problem", "tblcheck_problem", 
    "gradethis_problem"))

# tbl_grade_table() columns

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your `a` column should be `x`, `y`, and `z`.
      >

---

    structure(list(type = "column_value_diffs", expected = c("x", 
    "y", "z"), column = "a"), class = c("column_value_diffs_problem", 
    "vector_value_diffs_problem", "value_diffs_problem", "tblcheck_problem", 
    "gradethis_problem"))

# tbl_grade_table() returns grades with row problems

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 25 rows, but it has 26 rows.
      >

---

    structure(list(type = "table_nrow", expected = 25L, actual = 26L), class = c("table_nrow_problem", 
    "nrow_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 1 row, but it has 26 rows.
      >

---

    structure(list(type = "table_nrow", expected = 1L, actual = 26L), class = c("table_nrow_problem", 
    "nrow_problem", "tblcheck_problem", "gradethis_problem"))

# tbl_grade_table() returns ncol feedback to learnr

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 2 columns, but it has 3 columns.
      >

---

    structure(list(type = "table_ncol", expected = 2L, actual = 3L), class = c("table_ncol_problem", 
    "ncol_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 1 column, but it has 3 columns.
      >

---

    structure(list(type = "table_ncol", expected = 1L, actual = 3L), class = c("table_ncol_problem", 
    "ncol_problem", "tblcheck_problem", "gradethis_problem"))

# tbl_grade_table() returns names feedback to learnr

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x`, `y`, `z`, and 1 more. Your
        table should not have columns named `a`, `b`, `c`, or 1 more.
      >

---

    structure(list(type = "table_names", missing = c("x", "y", "z", 
    "w"), unexpected = c("a", "b", "c", "d")), class = c("table_names_problem", 
    "names_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x`, `y`, `z`, and `w`. Your
        table should not have columns named `a`, `b`, `c`, or `d`.
      >

---

    structure(list(type = "table_names", missing = c("x", "y", "z", 
    "w"), unexpected = c("a", "b", "c", "d")), class = c("table_names_problem", 
    "names_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x` and 3 more. Your table
        should not have columns named `a` or 3 more.
      >

---

    structure(list(type = "table_names", missing = c("x", "y", "z", 
    "w"), unexpected = c("a", "b", "c", "d")), class = c("table_names_problem", 
    "names_problem", "tblcheck_problem", "gradethis_problem"))

