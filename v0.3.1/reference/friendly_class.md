# Generate a human-readable description of an object's class

Generate a human-readable description of an object's class

## Usage

``` r
friendly_class(object)

# S4 method for ANY
friendly_class(object)

# S4 method for character
friendly_class(object)

# S4 method for numeric
friendly_class(object)

# S4 method for integer
friendly_class(object)

# S4 method for logical
friendly_class(object)

# S4 method for complex
friendly_class(object)

# S4 method for raw
friendly_class(object)

# S4 method for factor
friendly_class(object)

# S4 method for Date
friendly_class(object)

# S4 method for POSIXt
friendly_class(object)

# S4 method for Period
friendly_class(object)

# S4 method for data.frame
friendly_class(object)

# S4 method for tbl_df
friendly_class(object)

# S4 method for grouped_df
friendly_class(object)

# S4 method for rowwise_df
friendly_class(object)

# S4 method for list
friendly_class(object)

# S4 method for matrix
friendly_class(object)

# S4 method for array
friendly_class(object)
```

## Arguments

- object:

  An object whose class will be described

## Value

A [character](https://rdrr.io/r/base/character.html) string of length 1,
based on the [class](https://rdrr.io/r/base/class.html) and
[length](https://rdrr.io/r/base/length.html) of `object`.
