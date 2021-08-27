# grade missing groups

    Code
      grade_ungrouped
    Output
      <gradethis_graded: [Incorrect] Your table should be grouped by `b`. >

---

    Code
      grade_grouped
    Output
      <gradethis_graded: [Incorrect]
        Your table should be grouped by `b` and `c`.
      >

# grade unexpected groups

    Code
      grade_single
    Output
      <gradethis_graded: [Incorrect]
        Your table should not be grouped by `b`.
      >

---

    Code
      grade_multiple
    Output
      <gradethis_graded: [Incorrect]
        Your table should not be grouped by `b` or `c`.
      >

# grade groups max_diffs()

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should be grouped by `a`, `b`, `c`, and 1 more.
      >

---

    Code
      grade_inf
    Output
      <gradethis_graded: [Incorrect]
        Your table should be grouped by `a`, `b`, `c`, and `d`.
      >

---

    Code
      grade_one
    Output
      <gradethis_graded: [Incorrect]
        Your table should be grouped by `a` and 3 more.
      >

