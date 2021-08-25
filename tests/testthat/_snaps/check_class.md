# tbl_grade_class()

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a number (class `numeric`), but it is a text
        string (class `character`).
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of numbers (class `numeric`), but it
        is a vector of text (class `character`).
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a date-time (class `POSIXct`), but it is a text
        string (class `character`).
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of date-times (class `POSIXlt`), but
        it is a vector of text (class `character`).
      >

# tbl_grade_class() ignores inconsequential mismatches

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a number (class `numeric`), but it is an
        integer (class `integer`).
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a text string (class `character`), but it is an
        object with classes `glue` and `character`.
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of date-times (class `POSIXlt`), but
        it is a vector of date-times (class `POSIXct`).
      >

# tbl_grade_class() with multiple classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be an object with classes `test`, `class`, and
        `integer`, but it is an integer (class `integer`).
      >

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be an integer (class `integer`), but it is an
        object with classes `test`, `class`, and `integer`.
      >

