# Check DPD keys

`drug_code` is unique in `drug_product`, and every other table's
`drug_code` is a product.

## Usage

``` r
check_dpd_tables(tables)
```

## Arguments

- tables:

  named list of DPD tables

## Value

NULL, invisibly; stops on a failed check
