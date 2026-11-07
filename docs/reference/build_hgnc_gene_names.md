# Build the HGNC gene_name table

Build the HGNC gene_name table

## Usage

``` r
build_hgnc_gene_names(gene)
```

## Arguments

- gene:

  tibble from
  [`read_hgnc_complete_set()`](https://docs.ropensci.org/dbparser/reference/read_hgnc_complete_set.md)

## Value

tibble with hgnc_id, kind and name
