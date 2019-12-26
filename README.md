
<!-- README.md is generated from README.Rmd. Please edit attributes file -->

# dbparser

[![Build
Status](https://travis-ci.org/Dainanahan/dbparser.svg?branch=master)](https://travis-ci.org/Dainanahan/dbparser)
[![Build
status](https://ci.appveyor.com/api/projects/status/k18sqp55n39f3y5w?svg=true)](https://ci.appveyor.com/project/MohammedFCIS/dbparser)
[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/dbparser)](https://cran.r-project.org/package=dbparser)
[![codecov](https://codecov.io/gh/Dainanahan/dbparser/branch/master/graph/badge.svg)](https://codecov.io/gh/Dainanahan/dbparser)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/grand-total/dbparser)](https://cran.r-project.org/package=dbparser)
[![Rdoc](http://www.rdocumentation.org/badges/version/dbparser)](http://www.rdocumentation.org/packages/dbparser)
[![CII Best
Practices](https://bestpractices.coreinfrastructure.org/projects/3311/badge)](https://bestpractices.coreinfrastructure.org/projects/3311)
[![](https://badges.ropensci.org/347_status.svg)](https://github.com/ropensci/software-review/issues/347)

The main purpose of the `dbparser` package is to parse the
[DrugBank](http://drugbank.ca/) database which is downloadable in XML
format from [this link](https://www.drugbank.ca/releases/latest). The
parsed data can then be explored and analyzed as desired by the user.
The `dbparser` package further provides the facility of saving the
parsed data into a given database.

## Installation

You can install the released version of dbparser from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dbparser")
```

or you can install the latest updates directly from the repo

``` r
library(devtools)
devtools::install_github("Dainanahan/dbparser")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
## parse data from XML and save it to memory
dbparser::read_drugbank_xml_db(
  "D:/DS Projects/Drug_Interaction/drugbank.xml"
              #system.file("extdata", "drugbank_record.xml", package = "dbparser")
            )
#> [1] TRUE

## load drugs data
drugs <- dbparser::drug()

## load drug groups data
drug_groups <- dbparser::drug_groups()

## load drug targets actions data
drug_targets_actions <- dbparser::targets_actions()
```

## Saving into a database

The parsed data may be saved into a given database. Databases supported
by `dbparser` include MS SQL Server, MySQL and any database supported by
`DBI` package. Following is an example of saving the parsed data into a
MySQL database.

``` r
library(dbparser)

## open a connection to the desired database engine with an already
## created database
 open_db(xml_db_name =  "drugbank.xml", driver = "SQL Server",
 server = "ServerName\\\\SQL2016", output_database = "drugbank")

## save 'drugs' dataframe to DB
 parse_drug(TRUE)

## save 'drug_groups' dataframe to DB
 parse_drug_groups(TRUE)

## save 'drug_targets_actions' dataframe to DB
 parse_drug_targets_actions(TRUE)

## finally close db connection 
 close_db()
```

## Code of Conduct

Please note that the ‘dbparser’ project is released with a [Contributor
Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project,
you agree to abide by its terms.

## Contributing Guide

👍🎉 First off, thanks for taking the time to contribute\! 🎉👍 Please
review our [Contributing Guide](CONTRIBUTING.md).

## Share the love ❤️

Think **dbparser** is useful? Let others discover it, by telling them in
person, via Twitter or a blog post.

Using **dbparser** for a paper you are writing? Consider citing it

``` r
citation("dbparser")
#> 
#> To cite dbparser in publications use:
#> 
#>   Mohammed Ali, Ali Ezzat (). dbparser: DrugBank Database XML
#>   Parser. R package version 1.0.5.9000.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {DrugBank Database XML Parser},
#>     author = {Mohammed Ali and Ali Ezzat},
#>     organization = {Dainanahan},
#>     note = {R package version 1.0.5.9000},
#>     url = {https://CRAN.R-project.org/package=drugverse},
#>   }
```
