# grade missing names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have a column named `b`.
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `b` and `c`.
      >

# grade unexpected names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should not have a column named `b`.
      >

---

    Code
      grade
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

# grade names max_diffs()

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a`, `b`, `c`, and 1 more.
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a`, `b`, `c`, and `d`.
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a` and 3 more.
      >

