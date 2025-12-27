# Display dvobject Metadata

Displays information about passed dbobject object including basic info,
database metadata, and all data.frames contained within nested lists.

## Usage

``` r
show_dvobject_metadata(obj, return_df = FALSE)
```

## Arguments

- obj:

  A dvobject

- return_df:

  Logical. If TRUE, returns metadata data.frame without printing.
  Default is FALSE.

## Value

Invisibly returns a data.frame containing dvobject metadata

## See also

Other utility:
[`add_database_info()`](https://docs.ropensci.org/dbparser/reference/add_database_info.md),
[`subset_drugbank_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_drugbank_dvobject.md),
[`subset_onsides_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_onsides_dvobject.md)

## Examples

``` r
if (FALSE) { # \dontrun{
display_merged_db_attrs(drugbank)
metadata <- display_merged_db_attrs(drugbank, return_df = TRUE)
} # }
```
