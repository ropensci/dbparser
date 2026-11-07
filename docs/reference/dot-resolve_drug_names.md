# Resolve drug name lookup from a DrugBank dvobject

Returns a two-column tibble: drugbank_id, drug_name. Navigates:
\$drugs\$general_information

## Usage

``` r
.resolve_drug_names(drugbank_db)
```

## Arguments

- drugbank_db:

  A DrugBank dvobject.

## Value

A tibble or NULL.
