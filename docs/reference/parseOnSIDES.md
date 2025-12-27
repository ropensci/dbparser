# Parse the OnSIDES Core Relational Database

Parses the core relational tables from the OnSIDES database.

## Usage

``` r
parseOnSIDES(
  dataDir,
  include_high_confidence = TRUE,
  db_version = NULL,
  db_exported_date = NULL
)
```

## Arguments

- dataDir:

  A string specifying the path to the directory containing the OnSIDES
  CSV files.

- include_high_confidence:

  Logical. If TRUE (the default), the function will also parse the
  \`high_confidence.csv\` file, which is a pre-aggregated summary of
  ingredient-to-effect relationships. If the file is not found, a
  warning is issued.

- db_version:

  used onside version (default = NULL)

- db_exported_date:

  used onside release date (default = NULL)

## Value

dvobject

## See also

Other parsers:
[`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md),
[`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md),
[`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md),
[`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md),
[`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)

Other parsers:
[`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md),
[`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md),
[`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md),
[`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md),
[`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)
