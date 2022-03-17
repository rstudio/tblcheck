# value differences

    Code
      grade_default
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your result should be `11`, `12`, and `13`, not
        `1`, `2`, and `3`.
      >

---

    Code
      grade_1
    Output
      <gradethis_graded: [Incorrect]
        The first value of your result should be `11`, not `1`.
      >

---

    Code
      grade_5
    Output
      <gradethis_graded: [Incorrect]
        The first 5 values of your result should be `11`, `12`, `13`, `14`,
        and `15`, not `1`, `2`, `3`, `4`, and `5`.
      >

---

    Code
      grade_Inf
    Output
      <gradethis_graded: [Incorrect]
        The first 10 values of your result should be `11`, `12`, `13`, `14`,
        `15`, `16`, `17`, `18`, `19`, and `20`, not `1`, `2`, `3`, `4`, `5`,
        `6`, `7`, `8`, `9`, and `10`.
      >

# NA values

    Code
      grade_na
    Output
      <gradethis_graded: [Incorrect]
        The first 3 values of your result should be `TRUE`, `TRUE`, and
        `TRUE`, not `TRUE`, `TRUE`, and `NA`.
      >

# vec_grade_values() failures

    Code
      grade_attr
    Output
      <gradethis_graded: [Incorrect] Your result contains unexpected values.>

# column values problem messages are created correctly

    Code
      grade_column
    Output
      <gradethis_graded: [Incorrect]
        I didn't expect your `x` column to include the values `0.551`,
        `0.879`, and `0.790`.
      >

---

    Code
      grade_tbl
    Output
      <gradethis_graded: [Incorrect]
        I didn't expect your `x` column to include the values `0.551`,
        `0.879`, and `0.790`.
      >

# floating point differences are ignored by default

    Code
      grade_intolerant
    Output
      <gradethis_graded: [Incorrect]
        The first value of your result should be `2`, not `2`.
      >

