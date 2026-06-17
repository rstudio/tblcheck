# Generate a hint for how to convert one object type to another

Generate a hint for how to convert one object type to another

## Usage

``` r
hinted_class_message(object, expected)

# S4 method for ANY,ANY
hinted_class_message(object, expected)

# S4 method for rowwise_df,grouped_df
hinted_class_message(object, expected)

# S4 method for data.frame,grouped_df
hinted_class_message(object, expected)

# S4 method for grouped_df,data.frame
hinted_class_message(object, expected)

# S4 method for data.frame,rowwise_df
hinted_class_message(object, expected)

# S4 method for rowwise_df,data.frame
hinted_class_message(object, expected)
```

## Arguments

- object:

  An object to be compared to `expected`

- expected:

  An object of the expected class

## Value

A [character](https://rdrr.io/r/base/character.html) string of length 1
