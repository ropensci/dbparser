# Parse the openFDA Drugs@FDA and NDC Directory Downloads

Reads openFDA's bulk download files for Drugs@FDA (`drug/drugsfda`) and
the NDC directory (`drug/ndc`) and returns a `dvobject` with one table
per record level, field names as in the files.

## Usage

``` r
parseOpenFDA(
  drugsfda_path = NULL,
  ndc_path = NULL,
  release = NULL,
  release_date = NULL,
  retrieved_at = NULL
)
```

## Arguments

- drugsfda_path:

  Character or NULL. Path to the Drugs@FDA download (zip or JSON).

- ndc_path:

  Character or NULL. Path to the NDC directory download (zip or JSON).

- release:

  Character or NULL. Release label, e.g. `"2026-10"`.

- release_date:

  Date, `"YYYY-MM-DD"` or NULL. Release date; by default the files'
  export date.

- retrieved_at:

  Date, `"YYYY-MM-DD"` or NULL. Date the files were downloaded.

## Value

A `dvobject` with tibbles. From Drugs@FDA: `drugsfda_application` (key
`application_number`), `drugsfda_product` (key application and
`product_number`), `drugsfda_active_ingredient`, `drugsfda_submission`
(key application, `submission_type` and `submission_number`),
`drugsfda_submission_property_type`, `drugsfda_application_doc` and
`drugsfda_openfda`. From the NDC directory: `ndc_product` (key
`product_id`), `ndc_active_ingredient`, `ndc_route`, `ndc_pharm_class`,
`ndc_packaging` and `ndc_openfda`. Only the tables of the files given
are built.

## Details

Each download is one JSON object, `{meta, results}`, usually zipped
(`drug-drugsfda-0001-of-0001.json.zip`,
`drug-ndc-0001-of-0001.json.zip`). Zips are read directly, without
extracting them to disk; an unzipped `.json` file works too. The parser
checks the number of records against `meta.results.total`.

Drugs@FDA lists FDA application products (NDA, ANDA, BLA); the NDC
directory lists products registered for sale, including products sold
under an OTC monograph without an application. Every application,
product, submission and listing is kept as the files have it, including
tentative approvals (`submission_status` "TA"); the parser does no
status grouping and no matching across sources.

`YYYYMMDD` dates become dates. NDC packaging dates are written two ways
(`20220525` and `04-DEC-09`) and are kept as text, as written.
`finished` and `sample` are logical. Every other field stays text. The
`openfda` block of each record is kept as (key, field, value) rows; it
is openFDA's annotation, and it can belong to another drug (for example
ANDA202111).

`release_date` defaults to the files' `meta.last_updated` (the export
date) when every file given has the same one.

Parsing the full Drugs@FDA and NDC files takes about a minute and needs
roughly 300 MB of memory.

openFDA data are released under CC0 1.0. openFDA asks for credit, which
is recorded in the object's metadata (see
[`get_source_metadata`](https://docs.ropensci.org/dbparser/reference/get_source_metadata.md)),
and states: "Do not rely on openFDA to make decisions regarding medical
care."

## See also

Other parsers:
[`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md),
[`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md),
[`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md),
[`parseHGNC()`](https://docs.ropensci.org/dbparser/reference/parseHGNC.md),
[`parseHPO()`](https://docs.ropensci.org/dbparser/reference/parseHPO.md),
[`parseHealthCanadaDPD()`](https://docs.ropensci.org/dbparser/reference/parseHealthCanadaDPD.md),
[`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md),
[`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md),
[`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)

## Examples

``` r
openfda_dir <- system.file("extdata", "openfda", package = "dbparser")
openfda <- parseOpenFDA(
  drugsfda_path = file.path(openfda_dir,
                            "drug-drugsfda-0001-of-0001.json.zip"),
  ndc_path      = file.path(openfda_dir, "drug-ndc-0001-of-0001.json.zip"),
  release       = "2026-10")
#> Parsing openFDA Drugs@FDA...
#> Parsing openFDA NDC directory...
#> Parsed 9 Drugs@FDA applications and 9 NDC listings.
openfda$drugsfda_product[, c("application_number", "brand_name",
                             "marketing_status")]
#> # A tibble: 21 × 3
#>    application_number brand_name                           marketing_status
#>    <chr>              <chr>                                <chr>           
#>  1 ANDA076219         MIRTAZAPINE                          Prescription    
#>  2 ANDA076219         MIRTAZAPINE                          Prescription    
#>  3 ANDA076219         MIRTAZAPINE                          Prescription    
#>  4 ANDA076220         OFLOXACIN                            Discontinued    
#>  5 ANDA076220         OFLOXACIN                            Discontinued    
#>  6 ANDA076220         OFLOXACIN                            Discontinued    
#>  7 NDA007245          AEROLONE                             Discontinued    
#>  8 ANDA085638         CODEINE, ASPIRIN, APAP FORMULA NO. 4 Discontinued    
#>  9 BLA018780          HUMULIN R PEN                        Over-the-counter
#> 10 BLA018780          HUMULIN R                            Over-the-counter
#> # ℹ 11 more rows
get_source_metadata(openfda)
#> # A tibble: 1 × 12
#>   metadata_version source  source_name         release release_date retrieved_at
#>   <chr>            <chr>   <chr>               <chr>   <date>       <date>      
#> 1 1                openfda openFDA (U.S. Food… 2026-10 2026-10-20   NA          
#> # ℹ 6 more variables: source_url <chr>, licence <chr>, licence_url <chr>,
#> #   attribution <chr>, files <list>, parser <chr>
```
