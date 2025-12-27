# add_database_info Assign passed databases db metadata to passed dvobject

add_database_info Assign passed databases db metadata to passed dvobject

## Usage

``` r
add_database_info(
  dvobject,
  db_type = "DrugBank",
  db_version = NULL,
  db_exported_date = NULL
)
```

## Arguments

- dvobject:

  dvobject to assign metadata to it

- db_type:

  database type (defualt="DrugBank")

- db_version:

  database version as string

- db_exported_date:

  database official export date

## Value

dvobject

## See also

Other utility:
[`show_dvobject_metadata()`](https://docs.ropensci.org/dbparser/reference/show_dvobject_metadata.md),
[`subset_drugbank_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_drugbank_dvobject.md),
[`subset_onsides_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_onsides_dvobject.md)
