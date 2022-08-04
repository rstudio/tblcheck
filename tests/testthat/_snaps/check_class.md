# tbl_grade_class()

    Code
      grade_num_chr_1
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a number (class `numeric`), but it is a text
        string (class `character`).
      >

---

    Code
      grade_num_chr_2
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of numbers (class `numeric`), but it
        is a vector of text (class `character`).
      >

---

    Code
      grade_posixct_1
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a date-time (class `POSIXct`), but it is a text
        string (class `character`).
      >

---

    Code
      grade_posixct_2
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of date-times (class `POSIXlt`), but
        it is a vector of text (class `character`).
      >

# tbl_grade_class() ignore classes

    Code
      grade_glue_chr
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a text string (class `character`), but it is an
        object with classes `glue` and `character`.
      >

---

    Code
      grade_tbl_df
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a tibble (class `tbl_df`), but it is a data
        frame (class `data.frame`).
      >

---

    Code
      grade_py_tbl_df
    Output
      <gradethis_graded: [Incorrect]
        I was only expecting 1 value for each grouping in the table, but you
        have multiple values per grouping. Maybe you are missing a .groupby()
        call?
      >

---

    Code
      grade_py_tbl_df_grouped
    Output
      <gradethis_graded: [Incorrect]
        Your table rows are grouped, and not a numbered sequence. You can
        tell by the extra spacing around the column names. You can fix this
        with .reset_index()
      >

# tbl_grade_class() with paired ignore_class

    Code
      grade_int_dbl
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a number (class `numeric`), but it is an
        integer (class `integer`).
      >

---

    Code
      grade_int_chr_wrong_ignore
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a number (class `numeric`), but it is a text
        string (class `character`).
      >

---

    Code
      grade_posix_ct_lt
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of date-times (class `POSIXlt`), but
        it is a vector of date-times (class `POSIXct`).
      >

# tbl_grade_class() with multiple classes

    Code
      grade_class_solution
    Output
      <gradethis_graded: [Incorrect]
        Your result should be an object with classes `test`, `class`, and
        `integer`, but it is an integer (class `integer`).
      >

---

    Code
      grade_class_result
    Output
      <gradethis_graded: [Incorrect]
        Your result should be an integer (class `integer`), but it is an
        object with classes `test`, `class`, and `integer`.
      >

