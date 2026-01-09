# Drug Pathway parser

Metabolic, disease, and biological pathways that the drug is involved
in, as identified by the Small Molecule Protein Database (SMPDB).

## Usage

``` r
drug_pathway()
```

## Value

a tibble with the following variables:

- smpdb-id:

  Small Molecule Pathway Database identifier for this pathway.

- name:

  Pathway name

- category:

  Pathway category

- *drugbank_id*:

  drugbank id
