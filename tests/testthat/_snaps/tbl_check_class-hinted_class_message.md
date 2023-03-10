# tbl_grade_class() with hinted messages

    Code
      grade_ungrouped
    Output
      <gradethis_graded: [Incorrect]
        Your table isn't a grouped data frame, but I was expecting it to be
        grouped. Maybe you need to use `group_by()`?
      >

---

    Code
      grade_grouped
    Output
      <gradethis_graded: [Incorrect]
        Your table is a grouped data frame, but I wasn't expecting it to be
        grouped. Maybe you need to use `ungroup()`?
      >

---

    Code
      grade_ungrouped_int
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a grouped tibble (class `grouped_df`), but it
        is a vector of integers (class `integer`).
      >

---

    Code
      grade_unrowwise_int
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a rowwise tibble (class `rowwise_df`), but it
        is a vector of integers (class `integer`).
      >

