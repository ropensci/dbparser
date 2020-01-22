#' dbparser: A package for reading and parsing \strong{DrugBank} xml database.
#'
#' The main purpose of the `dbparser` package is to parse
#' [DrugBank](http://drugbank.ca/) database which is downloadable in XML format
#' from [this link](https://www.drugbank.ca/releases/latest).
#'
#' The parsed data can then be explored and analyzed as desired by the user
#'  with the ability to save parsed data into desired database as well.
#'
#'
#' To achieve this purpose, `dbparser`` package provides three main categories
#'  of functions:
#'
#' - xml db reader,
#'
#' - \strong{DrugBank} elements parsers,
#'
#' - and database related methods.
#'
#' For more infomration kindly check the
#' reference/index (https://dainanahan.github.io/dbparser/reference/index.html)
#'
#'
#' @section xml db reader functions:
#'  Reads \strong{DrugBank} xml database and build drug elements full tree in
#'  memory
#'
#' @section parsers functions:
#'  Each parser function is responsible of parsing certain drug element and
#'  returning its tibble with the ability to save it in a predefined database.
#'
#'  Check this tutorial
#'  (https://dainanahan.github.io/dbparser/articles/dbparser.html)
#'
#' @section database functions:
#'  To open a connection to given database in order to store parsed
#'  \strong{DrugBank} elements database.
#'
#'  Check this tutorial
#'  (https://dainanahan.github.io/dbparser/articles/Database_Saving.html)
#'
#' @docType package
#' @name dbparser
NULL
