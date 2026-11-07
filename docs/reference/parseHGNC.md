# Parse the HGNC Gene Nomenclature Files

Reads the HUGO Gene Nomenclature Committee (HGNC) complete set
(`hgnc_complete_set.txt`) and, optionally, the withdrawn entries
(`withdrawn.txt`) and returns a `dvobject` with gene, gene name and
withdrawn tables.

## Usage

``` r
parseHGNC(
  complete_set_path,
  withdrawn_path = NULL,
  release = NULL,
  release_date = NULL,
  retrieved_at = NULL
)
```

## Arguments

- complete_set_path:

  Character. Path to `hgnc_complete_set.txt`.

- withdrawn_path:

  Character or NULL. Path to `withdrawn.txt`. When NULL the `withdrawn`
  table is not built.

- release:

  Character or NULL. Release label, e.g. `"2026-10"`.

- release_date:

  Date, `"YYYY-MM-DD"` or NULL. Release date.

- retrieved_at:

  Date, `"YYYY-MM-DD"` or NULL. Date the files were downloaded.

## Value

A `dvobject` with tibbles:

- gene:

  One row per approved gene, keyed by `hgnc_id`, with the file's
  columns. Multi-valued cells keep the file's `a|b` text.

- gene_name:

  One row per (`hgnc_id`, `kind`, `name`): each gene's approved symbol
  (`kind` "symbol") and name ("name"), and every alias and previous
  symbol and name ("alias_symbol", "prev_symbol", "alias_name",
  "prev_name"), split on `|` and trimmed. Empty values and exact
  duplicates are dropped; names that differ only in case are kept.

- withdrawn:

  Only when `withdrawn_path` is given. One row per withdrawn ID and the
  ID it was merged into or split to (`merged_into_hgnc_id`,
  `merged_into_symbol`, `merged_into_status`); an entry without a target
  has one row with these `NA`. `merged_into_report` keeps the original
  cell. Rows are as written in HGNC's file, including any repeated
  target.

## Details

Both files are tab separated and UTF-8. In the complete set a cell
holding several values is pipe separated (`a|b`) and wrapped in double
quotes; the parser strips the wrapper and keeps the pipes. Column names
are lower-cased with `.` and `-` replaced by `_` (`pseudogene.org`
becomes `pseudogene_org`), and the `date_*` columns become dates. Every
other column stays text as written.

HGNC publishes no release number or date for the current files, so
`release` and `release_date` are `NA` unless given (e.g. the month of
the download, or the date of an HGNC archive file).

HGNC data are released under CC0; HGNC recommends citing its latest
paper, which is recorded in the object's metadata (see
[`get_source_metadata`](https://docs.ropensci.org/dbparser/reference/get_source_metadata.md)).

## See also

Other parsers:
[`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md),
[`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md),
[`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md),
[`parseHPO()`](https://docs.ropensci.org/dbparser/reference/parseHPO.md),
[`parseHealthCanadaDPD()`](https://docs.ropensci.org/dbparser/reference/parseHealthCanadaDPD.md),
[`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md),
[`parseOpenFDA()`](https://docs.ropensci.org/dbparser/reference/parseOpenFDA.md),
[`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md),
[`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)

## Examples

``` r
hgnc_dir <- system.file("extdata", "hgnc", package = "dbparser")
hgnc <- parseHGNC(file.path(hgnc_dir, "hgnc_complete_set.txt"),
                  withdrawn_path = file.path(hgnc_dir, "withdrawn.txt"),
                  release        = "2026-10",
                  retrieved_at   = "2026-10-16")
#> Parsing HGNC complete set...
#> Parsing HGNC withdrawn entries...
#> Parsed 24 genes and 215 gene names.
hgnc$gene_name[hgnc$gene_name$name == "PD1", ]
#> # A tibble: 3 × 3
#>   hgnc_id    kind         name 
#>   <chr>      <chr>        <chr>
#> 1 HGNC:8760  alias_symbol PD1  
#> 2 HGNC:11138 alias_symbol PD1  
#> 3 HGNC:14681 alias_symbol PD1  
get_source_metadata(hgnc)
#> # A tibble: 1 × 12
#>   metadata_version source source_name          release release_date retrieved_at
#>   <chr>            <chr>  <chr>                <chr>   <date>       <date>      
#> 1 1                hgnc   HUGO Gene Nomenclat… 2026-10 NA           2026-10-16  
#> # ℹ 6 more variables: source_url <chr>, licence <chr>, licence_url <chr>,
#> #   attribution <chr>, files <list>, parser <chr>
```
