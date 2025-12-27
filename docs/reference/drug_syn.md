# Drug Synonyms parser

Other names or identifiers that are associated with this drug.

## Usage

``` r
drug_syn()
```

## Value

a tibble with 3 variables:

- language:

  Names of the drug in languages other than English.

- coder:

  Organization or source providing the synonym. For example, INN
  indicates the synonym is an International Nonproprietary Name, while
  IUPAC indicates the synonym is the nomenclature designated by the
  International Union of Pure and Applied Chemistry.

- *drugbank_id*:

  drugbank id
