# Read one DPD file

No header, every value quoted, UTF-8. Checks the field count against the
read-me's columns and the parsed rows against the file's lines.

## Usage

``` r
read_dpd_file(path, spec)
```

## Arguments

- path:

  file path

- spec:

  one
  [`dpd_table_specs()`](https://docs.ropensci.org/dbparser/reference/dpd_table_specs.md)
  entry

## Value

data.frame with the spec's columns
