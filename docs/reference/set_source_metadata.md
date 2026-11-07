# set_source_metadata Attach a metadata block to a dvobject

Blocks are kept in one attribute, `source_metadata`, as a list named by
each block's `source`. A block for a source already present replaces it.

## Usage

``` r
set_source_metadata(dvobject, metadata)
```

## Arguments

- dvobject:

  dvobject to assign metadata to it

- metadata:

  block from
  [`source_metadata()`](https://docs.ropensci.org/dbparser/reference/source_metadata.md),
  or a named list of blocks (as stored on another dvobject)

## Value

dvobject
