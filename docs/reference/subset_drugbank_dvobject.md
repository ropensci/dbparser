# Subset a DrugBank dvobject by a vector of DrugBank IDs

Subset a DrugBank dvobject by a vector of DrugBank IDs

## Usage

``` r
subset_drugbank_dvobject(dvobject, drug_ids)
```

## Arguments

- dvobject:

  The dvobject from \`parseDrugBank()\`.

- drug_ids:

  A character vector of \`drugbank_id\` values to keep.

## Value

A new, smaller dvobject with the same structure and attributes.

## Details

Intelligently filters a DrugBank dvobject to retain only the data
associated with a specified list of drugbank_ids. It correctly handles
the deep, multi-level nested structure of the entire object, including
the complex relationships within the \`cett\` list.

## See also

Other utility:
[`add_database_info()`](https://docs.ropensci.org/dbparser/reference/add_database_info.md),
[`show_dvobject_metadata()`](https://docs.ropensci.org/dbparser/reference/show_dvobject_metadata.md),
[`subset_onsides_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_onsides_dvobject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(dbparser)
one_drug <- subset_drugbank_dvobject(dvobject = dbdataset::drugbank,
                                     drug_ids = "DB00001")
} # }
```
