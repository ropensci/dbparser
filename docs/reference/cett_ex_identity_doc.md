# Carriers/ Enzymes/ Targets/ Transporters Polypeptide External Identifiers parsers

Extract descriptions of identified polypeptide external identifiers for
targets, enzymes, carriers, or transporters.

## Usage

``` r
carriers_polypep_ex_ident()

enzymes_polypep_ex_ident()

targets_polypep_ex_ident()

transporters_polypep_ex_ident()
```

## Value

a tibble with 3 variables:

- resource:

  Name of the source database.

- identifier:

  Identifier for this drug in the given resource.

- parent_key:

  polypeptide id
