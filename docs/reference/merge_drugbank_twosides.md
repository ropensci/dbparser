# Merge a DrugBank dvobject with a TWOSIDES dvobject

Integrates drug-drug interaction data from TWOSIDES with the rich
mechanistic information from DrugBank. This function is chainable and
can accept a raw DrugBank object or an already-merged dvobject.

## Usage

``` r
merge_drugbank_twosides(db_object, twosides_db)
```

## Arguments

- db_object:

  A dvobject from \`parseDrugBank()\` or an existing merged dvobject.

- twosides_db:

  A dvobject from \`parseTWOSIDES()\`.

## Value

A new, nested dvobject with the TWOSIDES data added.

## See also

Other mergers:
[`merge_drugbank_onsides()`](https://docs.ropensci.org/dbparser/reference/merge_drugbank_onsides.md)
