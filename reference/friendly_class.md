# Generate a human-readable description of an object's class

Generate a human-readable description of an object's class

## Usage

``` r
friendly_class(object)

# S4 method for class 'ANY'
friendly_class(object)

# S4 method for class 'character'
friendly_class(object)

# S4 method for class 'numeric'
friendly_class(object)

# S4 method for class 'integer'
friendly_class(object)

# S4 method for class 'logical'
friendly_class(object)

# S4 method for class 'complex'
friendly_class(object)

# S4 method for class 'raw'
friendly_class(object)

# S4 method for class 'factor'
friendly_class(object)

# S4 method for class 'Date'
friendly_class(object)

# S4 method for class 'POSIXt'
friendly_class(object)

# S4 method for class 'Period'
friendly_class(object)

# S4 method for class 'data.frame'
friendly_class(object)

# S4 method for class 'tbl_df'
friendly_class(object)

# S4 method for class 'grouped_df'
friendly_class(object)

# S4 method for class 'rowwise_df'
friendly_class(object)

# S4 method for class 'list'
friendly_class(object)

# S4 method for class 'matrix'
friendly_class(object)

# S4 method for class 'array'
friendly_class(object)
```

## Arguments

- object:

  An object whose class will be described

## Value

A [character](https://rdrr.io/r/base/character.html) string of length 1,
based on the [class](https://rdrr.io/r/base/class.html) and
[length](https://rdrr.io/r/base/length.html) of `object`.
