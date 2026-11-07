# Getting started with dbparser

`dbparser` reads public drug and gene databases from the files you
download and returns each one as a `dvobject`: a list of tables
(tibbles) with a record of where the data came from. This vignette
parses two of the small example files that ship with the package. It
needs no internet connection.

``` r

library(dbparser)
```

## A DrugBank record

DrugBank is distributed as one large XML file, which you download from
DrugBank under your own account and licence. The package includes a
single drug record in the same format.

``` r

drugbank <- parseDrugBank(
  system.file("extdata", "drugbank_record_biotech.xml", package = "dbparser"))
```

[`show_dvobject_metadata()`](https://docs.ropensci.org/dbparser/reference/show_dvobject_metadata.md)
lists the tables in the object and their sizes:

``` r

show_dvobject_metadata(drugbank)
#> === BASIC INFO ===
#>      Class Total_DataFrames Top_Level_Lists Object_Size
#> 1 dvobject                1               1     10.7 Kb
#> 
#> === DATABASE METADATA ===
#>  Database     Type Version Export_Date
#>  Original DrugBank     5.1  2018-07-03
#> 
#> === DATA.FRAMES ===
#>                       Path Rows Cols   Size
#>  drugs$general_information    1   15 4.5 Kb
```

Every parser also records where its data came from.
[`get_source_metadata()`](https://docs.ropensci.org/dbparser/reference/get_source_metadata.md)
returns that record as a tibble, one row per source:

``` r

metadata <- get_source_metadata(drugbank)
fields   <- c("source", "release", "release_date", "source_url",
              "licence_url", "parser")
data.frame(field = fields,
           value = vapply(fields, function(f) as.character(metadata[[f]]), ""),
           row.names = NULL)
#>          field
#> 1       source
#> 2      release
#> 3 release_date
#> 4   source_url
#> 5  licence_url
#> 6       parser
#>                                                                        value
#> 1                                                                   drugbank
#> 2                                                                        5.1
#> 3                                                                 2018-07-03
#> 4                                                   https://go.drugbank.com/
#> 5 https://trust.drugbank.com/drugbank-trust-center/drugbank-terms-of-service
#> 6                                                             dbparser 2.3.0
```

The citation DrugBank requires is in `attribution`:

``` r

cat(strwrap(metadata$attribution), sep = "\n")
#> Knox C, Wilson M, Klinger CM, et al. DrugBank 6.0: the DrugBank
#> Knowledgebase for 2024. Nucleic Acids Res. 2024 Jan
#> 5;52(D1):D1265-D1275. doi: 10.1093/nar/gkad976.
```

`files` holds each input file with its SHA-256 hash (on R 4.5.0 or
later), so you can tell later which download a result came from:

``` r

metadata$files[[1]]
#>                          name
#> 1 drugbank_record_biotech.xml
#>                                                             sha256
#> 1 4c2d9e0f8a9b9f0faa12e52a8e2e884e21f9224548083164b83c83dee6ed4016
```

## The HGNC gene nomenclature

[`parseHGNC()`](https://docs.ropensci.org/dbparser/reference/parseHGNC.md)
reads the HUGO Gene Nomenclature Committee’s complete set and its list
of withdrawn entries. The example files hold 24 genes and 7 withdrawn
entries.

``` r

hgnc_dir <- system.file("extdata", "hgnc", package = "dbparser")
hgnc <- parseHGNC(file.path(hgnc_dir, "hgnc_complete_set.txt"),
                  withdrawn_path = file.path(hgnc_dir, "withdrawn.txt"),
                  release        = "2026-10",
                  retrieved_at   = "2026-10-16")

sapply(hgnc, dim)
#>      gene gene_name withdrawn
#> [1,]   24       215        14
#> [2,]   53         3         7
```

`gene_name` has one row per symbol or name of a gene, so a name that
belongs to several genes is easy to spot. “PD1” is an alias of three
genes:

``` r

pd1   <- hgnc$gene_name[hgnc$gene_name$name == "PD1", ]
genes <- hgnc$gene[, c("hgnc_id", "symbol", "name")]
names(genes)[3] <- "approved_name"
merge(pd1, genes, by = "hgnc_id")
#>      hgnc_id         kind name symbol                approved_name
#> 1 HGNC:11138 alias_symbol  PD1   SNCA              synuclein alpha
#> 2 HGNC:14681 alias_symbol  PD1 SPATA2 spermatogenesis associated 2
#> 3  HGNC:8760 alias_symbol  PD1  PDCD1      programmed cell death 1
```

`withdrawn` shows where a withdrawn symbol went; an entry that was split
has one row per target:

``` r

hgnc$withdrawn[hgnc$withdrawn$withdrawn_symbol == "CBBM",
               c("hgnc_id", "withdrawn_symbol", "merged_into_symbol")]
#> # A tibble: 2 × 3
#>   hgnc_id   withdrawn_symbol merged_into_symbol
#>   <chr>     <chr>            <chr>             
#> 1 HGNC:1531 CBBM             OPN1MW            
#> 2 HGNC:1531 CBBM             OPN1LW
```

The source record has the same columns for every parser. Here it carries
the release and download date passed to
[`parseHGNC()`](https://docs.ropensci.org/dbparser/reference/parseHGNC.md),
HGNC’s CC0 licence and the citation HGNC asks for:

``` r

hgnc_metadata <- get_source_metadata(hgnc)
fields        <- c("source", "release", "retrieved_at", "licence",
                   "licence_url")
data.frame(field = fields,
           value = vapply(fields,
                          function(f) as.character(hgnc_metadata[[f]]), ""),
           row.names = NULL)
#>          field                                    value
#> 1       source                                     hgnc
#> 2      release                                  2026-10
#> 3 retrieved_at                               2026-10-16
#> 4      licence                                  CC0-1.0
#> 5  licence_url https://www.genenames.org/about/license/
cat(strwrap(hgnc_metadata$attribution), sep = "\n")
#> Seal RL, Braschi B, Gray K, McClay J, Tweedie S, Bruford EA.
#> Genenames.org: the HGNC and PGNC resources in 2026. Nucleic Acids
#> Research (Database issue). HGNC (RRID:SCR_002827).
hgnc_metadata$files[[1]]
#>                    name
#> 1 hgnc_complete_set.txt
#> 2         withdrawn.txt
#>                                                             sha256
#> 1 c3289254858cdae91c28c5a6139ef75253359fbe3be8ccb10cc8a96679841612
#> 2 0d869c95057eb960ec7b7ce7844a896abfda755eadedef3051e51b767925b85b
```

## More

The other parsers follow the same pattern:
[`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md),
[`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md),
[`parseHPO()`](https://docs.ropensci.org/dbparser/reference/parseHPO.md),
[`parseHealthCanadaDPD()`](https://docs.ropensci.org/dbparser/reference/parseHealthCanadaDPD.md)
and
[`parseOpenFDA()`](https://docs.ropensci.org/dbparser/reference/parseOpenFDA.md).
The worked examples with charts are on the package website:

- [Unlocking DrugBank: parsing and visualizing mechanistic
  data](https://docs.ropensci.org/dbparser/articles/dbparser.html)
- [Integrated pharmacovigilance: linking DrugBank, OnSIDES and
  TWOSIDES](https://docs.ropensci.org/dbparser/articles/drugbank_nside.html)
- [Anticipating on-target drug effects with HPO gene-phenotype
  data](https://docs.ropensci.org/dbparser/articles/hpo_target_phenotypes.html)
