# Read hgnc_complete_set.txt

Strips the double-quote wrapper of multi-valued cells, cleans column
names and types the `date_*` columns.

## Usage

``` r
read_hgnc_complete_set(path)
```

## Arguments

- path:

  file path

## Value

tibble, one row per HGNC ID
