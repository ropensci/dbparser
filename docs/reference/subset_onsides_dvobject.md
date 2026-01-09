# Subset an OnSIDES dvobject by a vector of RxNorm Ingredient IDs (Schema-Aware)

Intelligently filters an OnSIDES dvobject by cascading filters through
the relational tables, ensuring the final subset is self-consistent.

## Usage

``` r
subset_onsides_dvobject(dvobject, ingredient_ids)
```

## Arguments

- dvobject:

  A dvobject from \`parseOnSIDES()\`.

- ingredient_ids:

  A character vector of RxNorm CUIs (ingredients) to keep.

## Value

A new, smaller dvobject with the same structure.

## See also

Other utility:
[`add_database_info()`](https://docs.ropensci.org/dbparser/reference/add_database_info.md),
[`show_dvobject_metadata()`](https://docs.ropensci.org/dbparser/reference/show_dvobject_metadata.md),
[`subset_drugbank_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_drugbank_dvobject.md)
