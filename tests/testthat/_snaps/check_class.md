# tbl_grade_class()

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a number (class `numeric`), but it is a text
        string (class `character`).
      >

---

    structure(list(type = "class", expected = "numeric", actual = "character", 
        expected_length = 1L, actual_length = 1L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of numbers (class `numeric`), but it
        is a vector of text (class `character`).
      >

---

    structure(list(type = "class", expected = "numeric", actual = "character", 
        expected_length = 2L, actual_length = 2L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a date-time (class `POSIXct`), but it is a text
        string (class `character`).
      >

---

    structure(list(type = "class", expected = c("POSIXct", "POSIXt"
    ), actual = "character", expected_length = 1L, actual_length = 1L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of date-times (class `POSIXlt`), but
        it is a vector of text (class `character`).
      >

---

    structure(list(type = "class", expected = c("POSIXlt", "POSIXt"
    ), actual = "character", expected_length = 2L, actual_length = 2L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

# tbl_grade_class() ignores inconsequential mismatches

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a number (class `numeric`), but it is an
        integer (class `integer`).
      >

---

    structure(list(type = "class", expected = "numeric", actual = "integer", 
        expected_length = 1L, actual_length = 1L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a text string (class `character`), but it is an
        object with classes `glue` and `character`.
      >

---

    structure(list(type = "class", expected = "character", actual = c("glue", 
    "character"), expected_length = 1L, actual_length = 1L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be a vector of date-times (class `POSIXlt`), but
        it is a vector of date-times (class `POSIXct`).
      >

---

    structure(list(type = "class", expected = c("POSIXlt", "POSIXt"
    ), actual = c("POSIXct", "POSIXt"), expected_length = 2L, actual_length = 2L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

# tbl_grade_class() with multiple classes

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be an object with classes `test`, `class`, and
        `integer`, but it is an integer (class `integer`).
      >

---

    structure(list(type = "class", expected = c("test", "class", 
    "integer"), actual = "integer", expected_length = 1L, actual_length = 1L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

---

    Code
      grade
    Output
      <gradethis_graded: [Incorrect]
        Your result should be an integer (class `integer`), but it is an
        object with classes `test`, `class`, and `integer`.
      >

---

    structure(list(type = "class", expected = "integer", actual = c("test", 
    "class", "integer"), expected_length = 1L, actual_length = 1L), class = c("class_problem", 
    "tblcheck_problem", "gradethis_problem"))

