# Read an openFDA download (zip or JSON)

A zip is read through a connection, never extracted to disk.

## Usage

``` r
read_openfda_json(path)
```

## Arguments

- path:

  file path

## Value

list(meta, results), parsed without simplification
