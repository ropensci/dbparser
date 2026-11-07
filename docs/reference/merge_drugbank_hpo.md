# Merge DrugBank and HPO Gene-Phenotype Database Objects

Creates an integrated dvobject by linking DrugBank target gene data with
HPO gene-phenotype associations using gene symbols as the bridge.

## Usage

``` r
merge_drugbank_hpo(db_object, hpo_db)
```

## Arguments

- db_object:

  A dvobject from
  [`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md)
  OR an existing merged dvobject (containing `$drugbank`).

- hpo_db:

  A dvobject produced by
  [`dbparser::parseHPO()`](https://docs.ropensci.org/dbparser/reference/parseHPO.md).

## Value

A new dvobject containing the integrated data.

## Details

This function performs the following key steps: 1. Extracts gene names
from DrugBank polypeptide data
(`$cett$targets$polypeptides$general_information`). 2. Links gene names
to DrugBank IDs via the target_id bridge
(`$cett$targets$general_information`). 3. Optionally resolves DrugBank
IDs to drug names. 4. Enriches the HPO gene-phenotype association table
by adding `drugbank_id` and `drug_name` columns. 5. Assembles a merged
object containing all original data plus the enriched table and the gene
mapping bridge.

Supports piping and chaining with other merge functions.

## See also

Other mergers:
[`merge_drugbank_onsides()`](https://docs.ropensci.org/dbparser/reference/merge_drugbank_onsides.md),
[`merge_drugbank_twosides()`](https://docs.ropensci.org/dbparser/reference/merge_drugbank_twosides.md)

## Examples

``` r
if (FALSE) { # \dontrun{
drugbank <- parseDrugBank("path/to/drugbank.xml")
hpo      <- parseHPO("path/to/gene_attribute_edges.tsv")

# Standalone merge
merged_db <- merge_drugbank_hpo(drugbank, hpo)

# Chained with other merges (any order)
full_db <- drugbank %>%
  merge_drugbank_hpo(hpo) %>%
  merge_drugbank_onsides(onsides) %>%
  merge_drugbank_twosides(twosides)

# --- Example: Find drugs targeting genes associated with seizures ---
seizure_drugs <- full_db$integrated_data$gene_phenotype_enriched %>%
  filter(grepl("seizure", phenotype_name, ignore.case = TRUE)) %>%
  filter(!is.na(drugbank_id)) %>%
  select(drug_name, drugbank_id, gene_symbol, phenotype_name) %>%
  distinct()
} # }
```
