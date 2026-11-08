## Resubmission

This is a resubmission. In this version I have:

* Fixed an invalid relative link (drugbank_nside.html) in a vignette; links to
  articles that moved to the package website are now absolute URLs.

## Maintainer email

The maintainer email changes from moh_fcis@yahoo.com to
mohammed.ali@interstellar-egypt.com; the old address will confirm.

## Submission

This is a minor release (2.2.1 -> 2.3.0). It adds parsers for the Human Phenotype
Ontology (HPO) gene-phenotype associations, the HGNC gene nomenclature files,
Health Canada's Drug Product Database and the openFDA
Drugs@FDA and NDC directory downloads; records source metadata on every
parsed object; and fixes zip inputs being extracted next to the input file
(for the bundled example, into the installed package). Examples, tests and
vignettes use small bundled files only and download nothing.

## Test environments

* local macOS 27.2 (aarch64), R 4.6.1: 0 errors | 0 warnings | 1 note
  (new maintainer)
* win-builder, R-devel: 0 errors | 0 warnings | 1 note (new maintainer)
* win-builder, R-release: 0 errors | 0 warnings | 1 note (new maintainer)
* win-builder, R-oldrelease: 0 errors | 0 warnings | 1 note (new maintainer)
* macOS builder (mac.r-project.org): not usable; on 2026-11-07 every upload
  was refused with HTTP 502, and on 2026-11-08 the check stopped at
  "checking package dependencies" because the builder had none of the CRAN
  Imports installed (data.table, dplyr, jsonlite, ...). macOS is covered by
  the local check and R-hub.
* R-hub (GitHub Actions), R-devel: linux, windows, macos, macos-arm64: PENDING

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  New maintainer: Mohammed Ali <mohammed.ali@interstellar-egypt.com>
  Old maintainer(s): Mohammed Ali <moh_fcis@yahoo.com>

  The maintainer's email changes, as stated above.

## URLs

canada.ca may reject automated HTTP/2 requests; the URL is valid in a browser.

## Installed size

The installed package is 2.6 MB. During development it had grown to
14 MB, of which 11.7 MB was three vignettes with interactive charts; these
are now articles on the package website only (pkgdown), and a short
chart-free vignette replaces them on CRAN. 2.2.1 shows no installed-size
note on any CRAN check flavor.

## Reverse dependencies

There are no reverse dependencies on CRAN
(`tools::package_dependencies("dbparser", reverse = TRUE)`).
