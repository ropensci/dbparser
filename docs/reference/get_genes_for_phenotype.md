# Get Genes Associated with a Phenotype

Returns the genes associated with a phenotype, given either its HPO
identifier (e.g. `"HP:0001250"`) or its name.

## Usage

``` r
get_genes_for_phenotype(hpo_db, phenotype, exact = FALSE)
```

## Arguments

- hpo_db:

  An HPO `dvobject` returned by
  [`parseHPO`](https://docs.ropensci.org/dbparser/reference/parseHPO.md).

- phenotype:

  Character. An HPO identifier or a phenotype name.

- exact:

  Logical. Used when `phenotype` is a name. If `TRUE`, the name must
  match exactly (ignoring case). If `FALSE` (default), any phenotype
  whose name contains `phenotype` matches.

## Value

A tibble with columns `gene_symbol`, `entrez_id`, `hpo_id`,
`phenotype_name` and `association_weight`, ordered by gene symbol.

## See also

Other utility:
[`add_database_info()`](https://docs.ropensci.org/dbparser/reference/add_database_info.md),
[`get_source_metadata()`](https://docs.ropensci.org/dbparser/reference/get_source_metadata.md),
[`search_phenotypes()`](https://docs.ropensci.org/dbparser/reference/search_phenotypes.md),
[`show_dvobject_metadata()`](https://docs.ropensci.org/dbparser/reference/show_dvobject_metadata.md),
[`subset_drugbank_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_drugbank_dvobject.md),
[`subset_onsides_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_onsides_dvobject.md)

## Examples

``` r
hpo_db <- parseHPO(system.file("extdata", "hpo_gene_attribute_edges.txt",
                               package = "dbparser"))
#> Parsing HPO gene-phenotype associations...
#> Parsed 7 genes, 352 phenotypes and 485 gene-phenotype associations.
get_genes_for_phenotype(hpo_db, "HP:0001250")
#> # A tibble: 3 × 5
#>   gene_symbol entrez_id hpo_id     phenotype_name association_weight
#>   <chr>       <chr>     <chr>      <chr>                       <dbl>
#> 1 KCNQ2       3785      HP:0001250 seizures                        1
#> 2 SCN1A       6323      HP:0001250 seizures                        1
#> 3 VDR         7421      HP:0001250 seizures                        1
get_genes_for_phenotype(hpo_db, "thrombosis")
#> # A tibble: 4 × 5
#>   gene_symbol entrez_id hpo_id     phenotype_name             association_weight
#>   <chr>       <chr>     <chr>      <chr>                                   <dbl>
#> 1 F2          2147      HP:0001977 abnormal thrombosis                         1
#> 2 F2          2147      HP:0002625 deep venous thrombosis                      1
#> 3 F2          2147      HP:0004936 venous thrombosis                           1
#> 4 F2          2147      HP:0005305 cerebral venous thrombosis                  1
```
