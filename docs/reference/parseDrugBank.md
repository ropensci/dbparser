# parseDrugBank

parses given DrugBank XML database into a dvobject. dvobject is a list
of data.frames in which each data.frame represents a part of parsed data
(i.e drugs, prices, carriers, ...)

## Usage

``` r
parseDrugBank(
  db_path,
  drug_options = NULL,
  parse_salts = FALSE,
  parse_products = FALSE,
  references_options = NULL,
  cett_options = NULL
)
```

## Arguments

- db_path:

  **string**, full path for the **DrugBank** xml or zip file.

- drug_options:

  **character vector**, list of sub drug related nodes names options to
  parse (default = NULL). Check
  [`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md)
  for all available options. If its value is \`NULL\` ONLY
  \`drug_general_information\` will be placed in the returned dvobject.

- parse_salts:

  **boolean**, parse salts info (default = FALSE)

- parse_products:

  **boolean**, parse products info (default = FALSE)

- references_options:

  **character vector**, list of sub references related nodes names
  options to parse (default = NULL). Check
  [`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)
  for all available options.

- cett_options:

  **character vector**, list of sub cett related nodes names options to
  parse (default = NULL). Check
  [`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md)
  for all available options.

## Value

dvobject

## See also

Other parsers:
[`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md),
[`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md),
[`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md),
[`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md),
[`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)
