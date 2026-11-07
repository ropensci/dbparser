# Generic fallback: search an object tree for a data frame with required columns

Generic fallback: search an object tree for a data frame with required
columns

## Usage

``` r
.find_table_with_columns(obj, required_cols, max_depth = 5L)
```

## Arguments

- obj:

  A list (possibly nested).

- required_cols:

  Character vector of column names that must all be present.

- max_depth:

  Integer. Maximum recursion depth.

## Value

A tibble or NULL.
