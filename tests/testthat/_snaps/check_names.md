# grade missing names

    Code
      grade_missing_1
    Output
      <gradethis_graded: [Incorrect]
        Your table should have a column named `b`.
      >

---

    Code
      grade_missing_2
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `b` and `c`.
      >

# grade unexpected names

    Code
      grade_unexpected_1
    Output
      <gradethis_graded: [Incorrect]
        Your table should not have a column named `b`.
      >

---

    Code
      grade_unexpected_2
    Output
      <gradethis_graded: [Incorrect]
        Your table should not have columns named `b` or `c`.
      >

# grade missing and unexpected names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `x` and `y`. Your table should
        not have columns named `a` or `b`.
      >

# grade names in wrong order

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table's columns were not in the expected order.
      >

# grade names max_diffs()

    Code
      grade_max_diffs_3
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a`, `b`, `c`, and 1 more.
      >

---

    Code
      grade_max_diffs_inf
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a`, `b`, `c`, and `d`.
      >

---

    Code
      grade_max_diffs_1
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a` and 3 more.
      >

