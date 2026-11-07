# Parse DPD dates ("22-MAR-2025")

English month abbreviations, whatever the session's locale.

## Usage

``` r
parse_dpd_date(values, label)
```

## Arguments

- values:

  character vector; NA stays NA

- label:

  where the values come from, for the error

## Value

Date; stops when a set value is not a DD-MON-YYYY calendar date
