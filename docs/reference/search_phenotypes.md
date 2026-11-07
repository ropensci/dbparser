# Search HPO Phenotypes by Keyword

Finds phenotypes whose name contains a keyword and lists their
associated genes. Matching is case-insensitive and treats `keyword` as a
plain string, not a regular expression.

## Usage

``` r
search_phenotypes(hpo_db, keyword, max_results = 50L)
```

## Arguments

- hpo_db:

  An HPO `dvobject` returned by
  [`parseHPO`](https://docs.ropensci.org/dbparser/reference/parseHPO.md).

- keyword:

  Character. Search term, e.g. `"seizure"`.

- max_results:

  Integer. Maximum number of phenotypes to return.

## Value

A tibble with columns `hpo_id`, `phenotype_name`, `n_genes` and `genes`
(comma-separated gene symbols), ordered by decreasing `n_genes`.

## See also

Other utility:
[`add_database_info()`](https://docs.ropensci.org/dbparser/reference/add_database_info.md),
[`get_genes_for_phenotype()`](https://docs.ropensci.org/dbparser/reference/get_genes_for_phenotype.md),
[`get_source_metadata()`](https://docs.ropensci.org/dbparser/reference/get_source_metadata.md),
[`show_dvobject_metadata()`](https://docs.ropensci.org/dbparser/reference/show_dvobject_metadata.md),
[`subset_drugbank_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_drugbank_dvobject.md),
[`subset_onsides_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_onsides_dvobject.md)

## Examples

``` r
hpo_db <- parseHPO(system.file("extdata", "hpo_gene_attribute_edges.txt",
                               package = "dbparser"))
#> Parsing HPO gene-phenotype associations...
#> Parsed 7 genes, 352 phenotypes and 485 gene-phenotype associations.
search_phenotypes(hpo_db, "seizure")
#> # A tibble: 16 × 4
#>    hpo_id     phenotype_name                                       n_genes genes
#>    <chr>      <chr>                                                  <int> <chr>
#>  1 HP:0001250 seizures                                                   3 KCNQ…
#>  2 HP:0002069 generalized tonic-clonic seizures                          2 KCNQ…
#>  3 HP:0002197 generalized seizures                                       2 KCNQ…
#>  4 HP:0002266 focal clonic seizures                                      2 KCNQ…
#>  5 HP:0002373 febrile seizures                                           2 KCNQ…
#>  6 HP:0007359 focal seizures                                             2 KCNQ…
#>  7 HP:0011153 focal motor seizures                                       2 KCNQ…
#>  8 HP:0002121 absence seizures                                           1 SCN1A
#>  9 HP:0002123 generalized myoclonic seizures                             1 SCN1A
#> 10 HP:0002199 hypocalcemic seizures                                      1 VDR  
#> 11 HP:0002384 focal seizures with impairment of consciousness or …       1 SCN1A
#> 12 HP:0006813 hemiclonic seizures                                        1 SCN1A
#> 13 HP:0010818 generalized tonic seizures                                 1 SCN1A
#> 14 HP:0010819 atonic seizures                                            1 SCN1A
#> 15 HP:0011145 symptomatic seizures                                       1 VDR  
#> 16 HP:0011146 dialeptic seizures                                         1 SCN1A
```
