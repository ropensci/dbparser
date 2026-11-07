# Parse the Health Canada Drug Product Database (DPD) Data Extract

Reads Health Canada's Drug Product Database data extract and returns a
`dvobject` with its 13 tables. Each of the four product sets (marketed,
approved, cancelled, dormant) is read from its unzipped folder or
straight from its zip; the rows of every set go in one table with an
`extract` column.

## Usage

``` r
parseHealthCanadaDPD(
  dpd_dir,
  extracts = c("marketed", "approved", "cancelled", "dormant"),
  release = NULL,
  release_date = NULL,
  retrieved_at = NULL
)
```

## Arguments

- dpd_dir:

  Character. Folder holding the extract's zips or unzipped folders.

- extracts:

  Character. Product sets to read, any of `"marketed"`, `"approved"`,
  `"cancelled"` and `"dormant"` (all by default).

- release:

  Character or NULL. Release label, e.g. `"2026-10"`. DPD publishes no
  release number.

- release_date:

  Date, `"YYYY-MM-DD"` or NULL. Release date.

- retrieved_at:

  Date, `"YYYY-MM-DD"` or NULL. Date the files were downloaded.

## Value

A `dvobject` with 13 tibbles, each keyed by `drug_code`: `drug_product`,
`active_ingredients`, `companies`, `form`, `packaging`,
`pharmaceutical_std`, `route`, `schedule`, `status`,
`therapeutic_class`, `veterinary_species`, `biosimilars` (each with
`extract`) and `inactive_products`. `inactive_products` lists cancelled
products, so it is built only when `extracts` includes `"cancelled"` and
an inactive products file is found.

## Details

`dpd_dir` holds the extract as Health Canada distributes it, each
product set either as its zip or unzipped into a folder of the same
name: `allfiles` (marketed), `allfiles_ap` (approved), `allfiles_ia`
(cancelled) and `allfiles_dr` (dormant), plus `inactive` (the inactive
products list). Zips are extracted to a temporary folder that is removed
after reading. When neither `inactive` nor `inactive.zip` is there,
`inactive.txt` is read from the cancelled set, which holds the same
file.

The files have no header: column names are those of Health Canada's
read-me ("Read Me File - Drug Product Database (DPD) Data Extract"),
lower case, and table names are the read-me's without `QRYM_`. Every
value is quoted and the files are UTF-8; empty values become `NA`. Code
columns (`drug_code`, `active_ingredient_code`, `company_code`,
`pharm_form_code`, `route_of_administration_code`, `si_code`) are
integers and the `DD-MON-YYYY` dates are dates; every other column stays
text as written. Dates are kept as written, including two lot expiry
dates in the cancelled set written with years 0030 and 0031.

The therapeutic class files hold four fields where the read-me lists
seven columns: the three certain ones are kept and the parser stops if
the fourth ever holds a value.

The parser groups nothing: `status` holds every status row as written,
with `current_status_flag` marking each product's current one.

DPD data are published under the Open Government Licence - Canada
(version 2.0); the attribution it asks for is recorded in the object's
metadata (see
[`get_source_metadata`](https://docs.ropensci.org/dbparser/reference/get_source_metadata.md)).
The DPD extract page also links to Health Canada's website terms; the
dataset record on open.canada.ca states the Open Government Licence –
Canada.

## See also

Other parsers:
[`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md),
[`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md),
[`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md),
[`parseHGNC()`](https://docs.ropensci.org/dbparser/reference/parseHGNC.md),
[`parseHPO()`](https://docs.ropensci.org/dbparser/reference/parseHPO.md),
[`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md),
[`parseOpenFDA()`](https://docs.ropensci.org/dbparser/reference/parseOpenFDA.md),
[`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md),
[`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)

## Examples

``` r
dpd_dir <- system.file("extdata", "health_canada_dpd", package = "dbparser")
dpd <- parseHealthCanadaDPD(dpd_dir, release = "2026-10",
                            retrieved_at = "2026-10-21")
#> Parsed 12 DPD products from marketed, approved, cancelled, dormant.
dpd$drug_product[, c("drug_code", "brand_name", "extract")]
#> # A tibble: 12 × 3
#>    drug_code brand_name                                                  extract
#>        <int> <chr>                                                       <chr>  
#>  1        15 AVENTYL                                                     market…
#>  2        85 ATRAVET 10 MG INJECTABLE                                    market…
#>  3       129 GERME ZONE                                                  market…
#>  4     18349 ASAPHEN                                                     market…
#>  5     81048 OMNITROPE                                                   market…
#>  6     14535 APO-CLOMIPRAMINE                                            approv…
#>  7     99581 NOROMBY                                                     approv…
#>  8         2 ORINASE 0.5GM                                               cancel…
#>  9     80771 MEDROXYPROGESTERONE ACETATE INJECTABLE SUSPENSION USP       cancel…
#> 10     94947 50+ DRY TOUCH CRÈME PROTECTION SOLAIRE LARGE SPECTRE FPS 5… cancel…
#> 11        31 NEO-BEX INJECTABLE                                          dormant
#> 12       126 DEWORMER LIQUID 12.4%                                       dormant
get_source_metadata(dpd)
#> # A tibble: 1 × 12
#>   metadata_version source          source_name release release_date retrieved_at
#>   <chr>            <chr>           <chr>       <chr>   <date>       <date>      
#> 1 1                health_canada_… Health Can… 2026-10 NA           2026-10-21  
#> # ℹ 6 more variables: source_url <chr>, licence <chr>, licence_url <chr>,
#> #   attribution <chr>, files <list>, parser <chr>
```
