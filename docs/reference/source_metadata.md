# source_metadata Build the source metadata block that every parser attaches to its dvobject.

Unknown fields stay `NA`; nothing is guessed. `retrieved_at` is never
taken from file modification dates. File hashes are SHA-256 and need R
\>= 4.5.0
([`tools::sha256sum()`](https://rdrr.io/r/tools/sha256sum.html)); on
older R they are `NA`.

## Usage

``` r
source_metadata(
  source,
  source_name = source,
  files = character(),
  release = NA,
  release_date = NA,
  retrieved_at = NA,
  source_url = NA,
  licence = NA,
  licence_url = NA,
  attribution = NA
)
```

## Arguments

- source:

  short source id, e.g. "hgnc"

- source_name:

  full source name

- files:

  paths of the input files that were parsed

- release:

  source release label as published, e.g. "5.1"

- release_date:

  source release date (Date or "YYYY-MM-DD")

- retrieved_at:

  date the files were downloaded (Date or "YYYY-MM-DD")

- source_url:

  source download or home page

- licence:

  SPDX licence id, e.g. "CC0-1.0"

- licence_url:

  licence or terms page

- attribution:

  credit or citation sentence the source asks for

## Value

list, the metadata block
