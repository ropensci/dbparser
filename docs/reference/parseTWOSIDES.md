# Parse the TWOSIDES Drug-Drug Interaction Database

Reads the
[TWOSIDES](https://tatonettilab-resources.s3.amazonaws.com/nsides/TWOSIDES.csv.gz)
data file, which contains adverse event data for pairs of drugs taken
concurrently (N=2 interactions).

## Usage

``` r
parseTWOSIDES(twosides_file_path, db_version = NULL, db_exported_date = NULL)
```

## Arguments

- twosides_file_path:

  Path to the TWOSIDES data file (e.g., 'TWOSIDES.csv.gz').

- db_version:

  used twoside version (default = NULL)

- db_exported_date:

  used twoside release date (default = NULL)

## Value

A dvobject of class \`TWOSIDESDB\` containing the
\`drug_drug_interactions\` data frame and associated metadata.

## Details

[TWOSIDES](https://tatonettilab-resources.s3.amazonaws.com/nsides/TWOSIDES.csv.gz)
is a database of drug-drug interaction safety signals mined from the
FDA's Adverse Event Reporting System using the same approach as is used
to generate OffSIDES.

Database fields as follow:

- drug_1_rxnorn_id:

  RxNORM identifier for drug 1

- drug_1_concept_name:

  RxNORM name string for drug 1

- drug_2_rxnorm_id:

  RxNORM identifier for drug 2

- drug_2_concept_name:

  RxNORM name string for drug 3

- condition_meddra_id:

  MedDRA identifier for the side effect

- condition_concpet_name:

  MedDRA name string for the side effect

- A:

  The number of reports for the pair of drugs that report the side
  effect

- B:

  The number of reports for the pair of drugs that do not report the
  side effect

- C:

  The number of reports for other PSM matched drugs (including perhaps
  the single versions of drug 1 or drug 2) that report the side effect

- D:

  The number of reports for other PSM matched drugs and other side
  effects

- PRR:

  Proportional reporting ratio, PRR=(A/(A+B))/(C/(C+D))

- PRR_error:

  Error estimate of the PRR

- mean_reporting_frequency:

  Proportion of reports for the drug that report the side effect,
  A/(A+B)

## See also

Other parsers:
[`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md),
[`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md),
[`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md),
[`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md),
[`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)
