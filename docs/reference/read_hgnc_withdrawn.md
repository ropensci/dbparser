# Read withdrawn.txt

One row per (withdrawn ID, target); targets are `HGNC_ID|SYMBOL|STATUS`
separated by `", "`.

## Usage

``` r
read_hgnc_withdrawn(path)
```

## Arguments

- path:

  file path

## Value

tibble
