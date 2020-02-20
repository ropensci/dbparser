# dbparser 1.0.5.9001

- Internal changes only.


# dbparser 1.0.4
* Fix save drugs tibbles as csv several issues
* Update sql database tibbles saver functions
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
