# dbparser

# dbparser 2.2.0

This is a major feature release focused on expanding `dbparser`'s capabilities into real-world pharmacovigilance and drug-drug interaction analysis. The integration engine has been formalized around a "Hub and Spoke" model, with DrugBank acting as the central hub.

## NEW FEATURES

*   **New Parser: `parseOnSIDES()`**
    *   Parses the relational CSV files from the OnSIDES database, a modern resource for adverse drug events extracted from FDA labels.
    *   Returns a `dvobject` containing the 7 core relational tables and an optional `high_confidence` summary table.

*   **New Parser: `parseTWOSIDES()`**
    *   Parses the TWOSIDES database, the leading resource for drug-drug interaction (DDI) adverse event signals from real-world data.
    *   Returns a `dvobject` containing the `drug_drug_interactions` table.
    *   The parser correctly handles known column name misspellings in the source data (e.g., `drug_1_rxnorn_id`).

*   **New Integration Function: `merge_drugbank_onsides()`**
    *   Merges a DrugBank `dvobject` with an OnSIDES `dvobject`.
    *   Automatically creates an enriched `integrated_data` list, linking OnSIDES data to DrugBank IDs via RxNorm CUIs.
    *   The function is **chainable**, meaning it can be used in a `%>%` pipeline after other merge functions.

*   **New Integration Function: `merge_drugbank_twosides()`**
    *   Merges a DrugBank `dvobject` with a TWOSIDES `dvobject`.
    *   Performs a "double join" to enrich both drugs in an interaction pair with their DrugBank IDs and names.
    *   Uses a robust "union" logic to keep interactions even if only one of the two drugs is present in the input DrugBank object.
    *   The function is also **chainable**.
*   **Subset a DrugBank dvobject function: `subset_drugbank_dvobject()`**
    * Intelligently filters a DrugBank dvobject to retain only the data associated with a specified list of drugbank_ids. It correctly handles the deep,
multi-level nested structure of the entire object, including the complex relationships within the `cett` list.
*   **Subset an OnSIDES dvobject function: `subset_onsides_dvobject()`**
    * Intelligently filters an OnSIDES dvobject by cascading filters through the
relational tables, ensuring the final subset is self-consistent.
*   **Adding metadata to existing `dvobject` objects using function: `add_database_info()`**
*   **Major enhancements to function `show_dvobject_metadata()`**
    * Displays information about passed dbobject object including basic info, 
    database metadata, and all data.frames contained within nested lists.

* Run `vignette("dbparser_2_2", package = "dbparser")` for more info

## DOCUMENTATION

*   **New Vignette: "Integrated Pharmacovigilance"**
    *   A comprehensive new tutorial demonstrating a full, three-way integration of DrugBank, OnSIDES, and TWOSIDES.
    *   Includes a complete scientific case study analyzing single-drug vs. polypharmacy risks.
    *   Introduces a reproducible example workflow using a pre-computed RDS data object hosted externally to keep the package lightweight.
    *   Run `vignette("drugbank_nside", package = "dbparser")` for more info
*  Updated existing vignette and package Readme with enhanced examples

## BUG FIXES & MINOR IMPROVEMENTS

*   Several minor fixes are done

---

# dbparser 2.0.3

## Enhancements
- Updated unit tests with latest DrugBank DB info v 5.1.12

## Bugs Fixes

- Fixed "Drug Targets has two `drugbank_id` columns" bug (#163)


----

# dbparser 2.0.2

## Enhancements
- Replaced "-" with "_" in tibbles column names (#111)
- Renamed "primary_key" to "drugbank_id" in parsed drugs "general_information" tibble (#111)
- Replaced "parent_key" keywords in different tibbles column names with original parent name (#111) 
- Updated Vignette to use interactive [canvasXpress](https://canvasxpress.org) package plots

----

# dbparser 2.0.1

## Bugs Fixes
* Fixed output references (#147)
* Updated `parseDrugBanK`parameters default values (#146)
* Fixed package documentation references (#144)
* Fixed CRAN error on some of linux info (#145)

-----------

# dbparser 2.0.0

## Breaking changes
* Deprecated saving parsed data into given database (#140)
* Deprecated saving parsed data into a csv file (#140)
* Deprecated old structure public methods (#141)
* Updated minimum R required version to 3.5 (#143)

## New features
* Introduced new methods for paring DrugBank DB and returning dvobject (#141)

## Major Updates
* Updated unit tests to work with new data structure (#141)

## Minor Fixes
* Removed RMariaDB dependency (#129)
* Fix pkgdown configuration for reference (#136)

-----------

# dbparser 1.2.0

## UI Changes
* Introduce progress bar in parser functions

## New Parsers
### Collective Parsers
* `drugs`, `cett` and `References` Parsers

### Elements Parsers
* `attachments` parsers for drugs and CETT
* `drug_pharmacology` parser
* Rename `drugs_books` parser to `drugs_textbooks`
* Rename `drug_all` parser to `run_all_parsers`
* Rename `drug` parser to `drug_general_information`

## Documentation Update:
* Add returned parsed data structure 
* Explain the returned data functionality as a whole and for each elements
* Point out to related/similar parsers

## Package design
For those who thinking to contribute in `dbparser`, now parsers are implemented
as R6 classes.

## Minor Fixes
* Update database saver functions to accommodate new DrugBank data size.

-----------

# dbparser 1.1.2

### Major Changes
* Enhance many memory and performance issues for many parsers.
* Change the drug classification representations to extract more useful
information.
### Minor Changes
* Change some drug tibbles features names
### DEFUNCT
* Size columns in `drugs` main table is no longer exist, will do full 
statistical analysis later using dvminer package.

-----------

# dbparser 1.1.1

* Fix column size issue while importing into SQL Server (#91)
* Fix dbparser and upcoming CRAN release of dplyr issues (#92)
* Fix CRAN Notes (#93)
* Fix package documentation and site references

-----------

# dbparser 1.1.0
### Major Changes
* Functions have been splitted into 6 categories *DrugBank Database Loading,
Carriers, Targets, Transporters, Drug and common parsers*. All function names
are changed to reflect the function family. The related documentation is also
updated (#66, #75).
* `dbparser` now can cite the package by calling `citation("dbparser")` (#71).
* Adding more user friendly error messages (#76, #81).
* User can now pass `DBI` database connection to parser functions as an 
argument beside *SQLite* and *MariaDB* (#87).

### DEFUNCT
* `open_db`, `open_mdb` and `close_db` functions are no longer supported. 
Creating and maintaining database is completely user responsibility and the 
database connection can be passed to parser functions (#87).

### DOCUMENTATION FIXES
* New tutorials for how to use `dbparser` have been created (#78, #79).
* Contribution guide has been added.
* Code of conduct has been added (#70).
* Enhance function reference documentation to include section for each type (#68).

-----------

# dbparser 1.0.4
* Fix save drugs tibbles as csv several issues.
* Update SQL database tibbles saver functions.
* Update SQL database saver functions documentations.
* Support MariaDB and introduce related functionalities.

-----------

# dbparser 1.0.3
* Fix CRAN errors and notes

-----------

# dbparser 1.0.2
* Fix zip file location issue
* Replace Secondary and third keys columns from drug framework with *other_keys* column that contains any other keys that might exist in addition to the primary key
* Add **average-mass**, **monoisotopic-mass** and **calculated-properties** parsers.
* Support saving parsed drugs related parsed database as csv

-----------

# dbparser 1.0.1
* Fix CRAN Note
* Improve documentation
* Refactor unused functions
* Remove *Count* features from drug data set
* Fix several typos in documentation and code
* Fix consistency issue of CLASS of tibbles Returned by dbparser
* Check if DrugBank database exist before parsing
* Add support for *international_brands* and *salts* elements
* Properly rename some features to have clear names
* Reduce datasets size by getting unique rows only
* Support reading zip file containing DrugBank xml database

-----------

# dbparser 1.0.0

* Initial release that contains core functionalities
