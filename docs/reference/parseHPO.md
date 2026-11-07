# Parse HPO Gene-Phenotype Associations

Reads the Human Phenotype Ontology (HPO) gene-phenotype associations as
distributed by Harmonizome (`gene_attribute_edges.txt`) and returns a
`dvobject` with gene, phenotype and association tables.

## Usage

``` r
parseHPO(
  filepath,
  minimum_weight = 0,
  db_version = NULL,
  db_exported_date = NULL
)
```

## Arguments

- filepath:

  Character. Path to the Harmonizome HPO `gene_attribute_edges.txt`
  file, plain or gzip-compressed.

- minimum_weight:

  Numeric. Minimum association weight to keep. The default `0` keeps all
  associations.

- db_version:

  Character or NULL. Dataset version, stored in the object metadata.

- db_exported_date:

  Character or NULL. Dataset release date, stored in the object
  metadata.

## Value

A `dvobject` with three tibbles:

- associations:

  One row per gene-phenotype pair: `gene_symbol`, `entrez_id`, `hpo_id`,
  `phenotype_name` and `association_weight`.

- genes:

  Unique genes: `gene_symbol`, `entrez_id` and `n_phenotypes`, the
  number of associated phenotypes.

- phenotypes:

  Unique phenotypes: `hpo_id`, `phenotype_name` and `n_genes`, the
  number of associated genes.

## Details

The Harmonizome file is tab separated and has two header lines: the
column names (`source`, `source_desc`, `source_id`, `target`,
`target_desc`, `target_id`, `weight`) followed by a descriptor row
(`GeneSym`, `NA`, `GeneID`, `Phenotype`, `HPOID`, `NA`, `weight`). The
parser drops the descriptor row and renames the columns to
domain-specific names. Columns that carry no information in this dataset
(`source_desc` and the `-666` placeholder in `target_id`) are not kept.

Gzip-compressed files (`.gz`) are read directly.

The resulting object can be linked to DrugBank target genes with
[`merge_drugbank_hpo`](https://docs.ropensci.org/dbparser/reference/merge_drugbank_hpo.md).

## See also

Other parsers:
[`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md),
[`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md),
[`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md),
[`parseHGNC()`](https://docs.ropensci.org/dbparser/reference/parseHGNC.md),
[`parseHealthCanadaDPD()`](https://docs.ropensci.org/dbparser/reference/parseHealthCanadaDPD.md),
[`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md),
[`parseOpenFDA()`](https://docs.ropensci.org/dbparser/reference/parseOpenFDA.md),
[`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md),
[`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)

## Examples

``` r
hpo_path <- system.file("extdata", "hpo_gene_attribute_edges.txt",
                        package = "dbparser")
hpo_db <- parseHPO(hpo_path)
#> Parsing HPO gene-phenotype associations...
#> Parsed 7 genes, 352 phenotypes and 485 gene-phenotype associations.
hpo_db$phenotypes
#> # A tibble: 352 × 3
#>    hpo_id     phenotype_name                          n_genes
#>    <chr>      <chr>                                     <int>
#>  1 HP:0000001 all                                           7
#>  2 HP:0000005 mode of inheritance                           7
#>  3 HP:0000118 phenotypic abnormality                        7
#>  4 HP:0000006 autosomal dominant inheritance                6
#>  5 HP:0000007 autosomal recessive inheritance               5
#>  6 HP:0000119 abnormality of the genitourinary system       4
#>  7 HP:0000152 abnormality of head or neck                   4
#>  8 HP:0000234 abnormality of the head                       4
#>  9 HP:0000707 abnormality of the nervous system             4
#> 10 HP:0000818 abnormality of the endocrine system           4
#> # ℹ 342 more rows
show_dvobject_metadata(hpo_db)
#> === BASIC INFO ===
#>      Class Total_DataFrames Top_Level_Lists Object_Size
#> 1 dvobject                3               0    137.6 Kb
#> 
#> === DATABASE METADATA ===
#>  Database Type Version Export_Date
#>  Original  HPO Unknown     Unknown
#> 
#> === DATA.FRAMES ===
#>          Path Rows Cols    Size
#>  associations  485    5 71.4 Kb
#>         genes    7    3    2 Kb
#>    phenotypes  352    3 58.3 Kb
```
