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
[`get_genes_for_phenotype()`](https://docs.ropensci.org/dbparser/reference/get_genes_for_phenotype.md),
[`get_source_metadata()`](https://docs.ropensci.org/dbparser/reference/get_source_metadata.md),
[`search_phenotypes()`](https://docs.ropensci.org/dbparser/reference/search_phenotypes.md),
[`subset_drugbank_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_drugbank_dvobject.md),
[`subset_onsides_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_onsides_dvobject.md)

## Examples

``` r
drugbank <- parseDrugBank(
  system.file("extdata", "drugbank_record_biotech.xml",
              package = "dbparser"))
#> Loading DrugBank DB from path: /private/var/folders/dx/_x2tl39x3slc9hp9_07j57ym0000gn/T/RtmpISG2b8/temp_libpath8978285440e9/dbparser/extdata/drugbank_record_biotech.xml
#> Completed loading DrugBank DB into memory
#> ...........................................
#> parsing drugs elements
#> drugs information parsing has started
#> parsing drugs general information
show_dvobject_metadata(drugbank)
#> === BASIC INFO ===
#>      Class Total_DataFrames Top_Level_Lists Object_Size
#> 1 dvobject                1               1     10.7 Kb
#> 
#> === DATABASE METADATA ===
#>  Database     Type Version Export_Date
#>  Original DrugBank     5.1  2018-07-03
#> 
#> === DATA.FRAMES ===
#>                       Path Rows Cols   Size
#>  drugs$general_information    1   15 4.5 Kb
metadata <- show_dvobject_metadata(drugbank, return_df = TRUE)
```
