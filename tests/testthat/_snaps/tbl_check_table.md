# tbl_check_table() class

    Code
      tbl_message(problem)
    Output
      Your table should be a tibble (class `tbl_df`), but it is a data frame (class `data.frame`).

---

    Code
      tbl_message(problem)
    Output
      [1] "Your table isn't a grouped data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?"

---

    Code
      tbl_message(problem)
    Output
      [1] "Your table is a rowwise data frame, but I wasn't expecting it to be rowwise. Maybe you need to use `ungroup()`?"

---

    Code
      tbl_message(problem)
    Output
      [1] "Your table is a rowwise data frame, but I was expecting it to be grouped. Maybe you need to use `group_by()`?"

# tbl_check_table() rows

    Code
      tbl_message(problem)
    Output
      Your table should have 25 rows, but it has 26 rows.

---

    Code
      tbl_message(problem)
    Output
      Your table should have 1 rows, but it has 26 rows.

# tbl_check_table() ncol

    Code
      tbl_message(problem)
    Output
      Your table should have 2 columns, but it has 3 columns.

---

    Code
      tbl_message(problem)
    Output
      Your table should have 1 columns, but it has 3 columns.

# tbl_check_table() names

    Code
      tbl_message(problem)
    Output
      Your table should have columns named `x` and `y`. Your table should not have columns named `a` or `b`.

# tbl_check_table() columns

    Code
      tbl_message(problem)
    Output
      The first 3 values of your `a` column should be `x`, `y`, and `z`.

# tbl_check_table() returns grades with row problems

    Code
      tbl_message(problem)
    Output
      Your table should have 25 rows, but it has 26 rows.

---

    Code
      tbl_message(problem)
    Output
      Your table should have 1 rows, but it has 26 rows.

# tbl_check_table() returns ncol feedback to learnr

    Code
      tbl_message(problem)
    Output
      Your table should have 2 columns, but it has 3 columns.

---

    Code
      tbl_message(problem)
    Output
      Your table should have 1 columns, but it has 3 columns.

# tbl_check_table() returns names feedback to learnr

    Code
      tbl_message(problem)
    Output
      Your table should have columns named `x`, `y`, `z`, and 1 more. Your table should not have columns named `a`, `b`, `c`, or 1 more.

---

    Code
      tbl_message(problem)
    Output
      Your table should have columns named `x`, `y`, `z`, and 1 more. Your table should not have columns named `a`, `b`, `c`, or 1 more.

---

    Code
      tbl_message(problem)
    Output
      Your table should have columns named `x`, `y`, `z`, and 1 more. Your table should not have columns named `a`, `b`, `c`, or 1 more.

