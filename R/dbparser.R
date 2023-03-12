#' dbparser: A package for reading and parsing \strong{DrugBank} xml database.
#'
#' The main purpose of the `dbparser` package is to parse
#' [DrugBank](https://go.drugbank.com/) database which is downloadable in XML format
#' from [this link](https://go.drugbank.com/releases/latest).
#'
#' The parsed data can then be explored and analyzed.
#'
#'
#' To achieve this purpose, `dbparser`` package provides three main categories
#'  of functions:
#'
#' - xml db reader,
#'
#' - \strong{DrugBank} elements parsers,
#'
#' For more information kindly check the
#' reference/index (https://docs.ropensci.org/dbparser/reference/index.html)
#'
#'
#' @section xml db reader functions:
#'  Reads \strong{DrugBank} xml database and build drug elements full tree in
#'  memory
#'
#' @section parsers functions:
#'  Each parser function is responsible of parsing certain drug element and
#'  returning its tibble.
#'
#'  Check this tutorial
#'  (https://docs.ropensci.org/dbparser/articles/dbparser.html)
#'
#' @docType package
#' @keywords internal
#' @name dbparser
NULL
