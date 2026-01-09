# Package index

## Drugs Databases Parser

Parse public drug databases and converted them into dvobject

- [`cett_nodes_options()`](https://docs.ropensci.org/dbparser/reference/cett_nodes_options.md)
  : returns carriers, enzymes,targets and transporters node valid
  options.
- [`drug_node_options()`](https://docs.ropensci.org/dbparser/reference/drug_node_options.md)
  : returns drug node valid options.
- [`parseDrugBank()`](https://docs.ropensci.org/dbparser/reference/parseDrugBank.md)
  : parseDrugBank
- [`parseOnSIDES()`](https://docs.ropensci.org/dbparser/reference/parseOnSIDES.md)
  : Parse the OnSIDES Core Relational Database
- [`parseTWOSIDES()`](https://docs.ropensci.org/dbparser/reference/parseTWOSIDES.md)
  : Parse the TWOSIDES Drug-Drug Interaction Database
- [`references_node_options()`](https://docs.ropensci.org/dbparser/reference/references_node_options.md)
  : returns references node valid options.

## Utility functions

Different functions to handle dvobject

- [`add_database_info()`](https://docs.ropensci.org/dbparser/reference/add_database_info.md)
  : add_database_info Assign passed databases db metadata to passed
  dvobject
- [`show_dvobject_metadata()`](https://docs.ropensci.org/dbparser/reference/show_dvobject_metadata.md)
  : Display dvobject Metadata
- [`subset_drugbank_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_drugbank_dvobject.md)
  : Subset a DrugBank dvobject by a vector of DrugBank IDs
- [`subset_onsides_dvobject()`](https://docs.ropensci.org/dbparser/reference/subset_onsides_dvobject.md)
  : Subset an OnSIDES dvobject by a vector of RxNorm Ingredient IDs
  (Schema-Aware)

## Merge functions

Different functions to merge different types of dvobjects

- [`merge_drugbank_onsides()`](https://docs.ropensci.org/dbparser/reference/merge_drugbank_onsides.md)
  : Merge DrugBank and OnSIDES Database Objects
- [`merge_drugbank_twosides()`](https://docs.ropensci.org/dbparser/reference/merge_drugbank_twosides.md)
  : Merge a DrugBank dvobject with a TWOSIDES dvobject
