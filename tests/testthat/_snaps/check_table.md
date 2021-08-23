# tbl_grade_table() class

    Code
      grade_tbl_class_df
    Output
      <gradethis_graded: [Incorrect]
        Your table should be a tibble (class `tbl_df`), but it is a data
        frame (class `data.frame`).
      >

---

    Code
      grade_tbl_class_grouped
    Output
      <gradethis_graded: [Incorrect]
        Your table isn't a grouped data frame, but I was expecting it to be
        grouped. Maybe you need to use `group_by()`?
      >

---

    Code
      grade_tbl_class_rowwise
    Output
      <gradethis_graded: [Incorrect]
        Your table is a rowwise data frame, but I wasn't expecting it to be
        rowwise. Maybe you need to use `ungroup()`?
      >

---

    Code
      grade_tbl_class_grouped_rowwise
    Output
      <gradethis_graded: [Incorrect]
        Your table is a rowwise data frame, but I was expecting it to be
        grouped. Maybe you need to use `group_by()`?
      >

# tbl_grade_table() rows

    Code
      grade_tbl_rows_missing_1
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 25 rows, but it has 26 rows.
      >

---

    Code
      grade_tbl_rows_extra_1
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 1 row, but it has 26 rows.
      >

# tbl_grade_table() ncol

    Code
      grade_tbl_cols_extra_1
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 2 columns, but it has 3 columns.
      >

---

    Code
      grade_tbl_cols_extra_2
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 1 column, but it has 3 columns.
      >

---

    Code
      grade_tbl_cols_missing_1
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 2 columns, but it has 1 column.
      >

# tbl_grade_table() names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x` and `y`. Your table should
        not have columns named `a` or `b`.
      >

# tbl_grade_table() columns

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your `a` column should be `x`, `y`, and `z`.
      >

# tbl_grade_table() returns grades with row problems

    Code
      grade_rows_extra
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 25 rows, but it has 26 rows.
      >

---

    Code
      grade_rows_missing
    Output
      <gradethis_graded: [Incorrect]
        Your table should have 1 row, but it has 26 rows.
      >

# tbl_grade_table() returns names feedback to learnr

    Code
      grade_tbl_names_3
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x`, `y`, `z`, and 1 more. Your
        table should not have columns named `a`, `b`, `c`, or 1 more.
      >

---

    Code
      grade_tbl_names_inf
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x`, `y`, `z`, and `w`. Your
        table should not have columns named `a`, `b`, `c`, or `d`.
      >

---

    Code
      grade_tbl_names_1
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x` and 3 more. Your table
        should not have columns named `a` or 3 more.
      >
