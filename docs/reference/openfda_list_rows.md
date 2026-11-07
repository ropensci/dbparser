# One row per element of each record's list field

One row per element of each record's list field

## Usage

``` r
openfda_list_rows(records, keys, list_field, columns)
```

## Arguments

- records:

  list of records

- keys:

  function(record) giving a named list of the parent's keys

- list_field:

  name of the list field, e.g. "products"

- columns:

  fields to take from each element

## Value

data.frame, the parent key columns first
