# grade_this_table()

    Code
      grade_pass
    Output
      <tblcheck_graded: [Correct] Correct>

---

    Code
      grade_class
    Output
      <tblcheck_graded: [Incorrect]
        Your table should be a tibble (class `tbl_df`), but it is a data
        frame (class `data.frame`).
      >

---

    Code
      grade_pass_default
    Output
      <tblcheck_graded: [Correct] PASS>

# grade_this_vector()

    Code
      grade_pass
    Output
      <tblcheck_graded: [Correct] Correct>

---

    Code
      grade_class
    Output
      <tblcheck_graded: [Incorrect]
        Your result should be a vector of integers (class `integer`), but it
        is a vector of numbers (class `numeric`).
      >

---

    Code
      grade_pass_default
    Output
      <tblcheck_graded: [Correct] PASS>

# pre_check setup

    Code
      grade_pass
    Output
      <tblcheck_graded: [Correct] Correct>

---

    Code
      grade_class
    Output
      <tblcheck_graded: [Incorrect]
        Your result should be a vector of integers (class `integer`), but it
        is a vector of numbers (class `numeric`).
      >

---

    Code
      grade_pass_default
    Output
      <tblcheck_graded: [Correct] PASS>

# pre_check test

    Code
      grade_fail
    Output
      <tblcheck_graded: [Incorrect] Incorrect>

# post_check test

    Code
      grade_pass
    Output
      <tblcheck_graded: [Correct] Correct>

---

    Code
      grade_fail
    Output
      <tblcheck_graded: [Incorrect] Incorrect>

