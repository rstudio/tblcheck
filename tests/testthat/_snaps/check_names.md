# grade missing names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have a column named `b`.
      >

---

    structure(list(type = "table_names", missing = "b"), class = c("table_names_problem", 
    "names_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `b` and `c`.
      >

---

    structure(list(type = "table_names", missing = c("b", "c")), class = c("table_names_problem", 
    "names_problem", "tblcheck_problem", "gradethis_problem"))

# grade unexpected names

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should not have a column named `b`.
      >

---

    structure(list(type = "table_names", unexpected = "b"), class = c("table_names_problem", 
    "names_problem", "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should not have columns named `b` or `c`.
      >

---

    structure(list(type = "table_names", unexpected = c("b", "c")), class = c("table_names_problem", 
    "names_problem", "tblcheck_problem", "gradethis_problem"))

# grade missing and unexpected names

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

# grade names max_diffs()

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a`, `b`, `c`, and 1 more.
      >

---

    structure(list(type = "table_names", missing = c("a", "b", "c", 
    "d")), class = c("table_names_problem", "names_problem", "tblcheck_problem", 
    "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a`, `b`, `c`, and `d`.
      >

---

    structure(list(type = "table_names", missing = c("a", "b", "c", 
    "d")), class = c("table_names_problem", "names_problem", "tblcheck_problem", 
    "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your table should have columns named `a` and 3 more.
      >

---

    structure(list(type = "table_names", missing = c("a", "b", "c", 
    "d")), class = c("table_names_problem", "names_problem", "tblcheck_problem", 
    "gradethis_problem"))

