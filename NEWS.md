# dbparser 1.1.0.9000

- Same as previous version.


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

# dbparser 1.0.4
* Fix save drugs tibbles as csv several issues.
* Update sql database tibbles saver functions.
* Update sql database saver functions documentations.
* Support MariaDB and introduce related functionalities.

# dbparser 1.0.3
* Fix CRAN errors and notes

# dbparser 1.0.2
* Fix zip file location issue
* Replace Secondary and third keys columns from drug framework with *other_keys* column that contains any other keys that might exist in addition to the primary key
* Add **average-mass**, **monoisotopic-mass** and **calculated-properties** parsers.
* Support saving parsed drugs related parsed database as csv

# dbparser 1.0.1
* Fix CRAN Note
* Improve documentation
* Refactor unused functions
* Remove *Count* features from drug data set
* Fix several typos in documentation and code
* Fix consistency issue of CLASS of tibbles Returned by dbparser
* Check if drugbank database exist before parsing
* Add support for *international_brands* and *salts* elements
* Properly rename some features to have clear names
* Reduce datasets size by getting unique rows only
* Support reading zip file containing DrugBank xml database

# dbparser 1.0.0

* Initial release that contains core functionalities
