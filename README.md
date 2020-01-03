
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

## Introduction

[DrugBank](http://drugbank.ca/) database is a comprehensive, freely
accessible, online database containing information on drugs and drug
targets. As both a bioinformatics and a cheminformatics resource,
DrugBank combines detailed drug (i.e. chemical, pharmacological and
pharmaceutical) data with comprehensive drug target (i.e. sequence,
structure, and pathway) information, more information about DrugBank can
be found [here](https://www.drugbank.ca/about/).

Using DrugBank database is not an easy task. As it is stored in a single
XML file that can be viewed using XMLviewer as stated
[here](http://shorturl.at/erEM7).

Here come the importance of `dbparser` package that parses DrugBank
database into different `R` tibbles that can be then be explored and
analyzed as desired by the user, check this
[tutorial](https://dainanahan.github.io/dbparser/articles/dbparser.html).

Moreover; user can save these tibbles in different databases like **SQL
Server DDB** and **Maria DB** using `dbparser`.

`dbparser` is tested against DrugBank versions *5.1.0* through *5.1.4*
successfully. If you found errors with these versions or any other
version please submit an issue
[here](https://github.com/Dainanahan/dbparser/issues).

Final note, in order to download DrugBank database you have to create a
count [here](https://www.drugbank.ca/public_users/sign_up) and wait a
little for registration confirmation.

While you are waiting you can make use from `dbdataset` data package
[here](https://dainanahan.github.io/dbdataset/index.html). It contains
parsed DrugBank database `R` tibbles using `dbparser`.

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
