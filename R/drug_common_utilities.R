# required librries
library(XML)
library(purrr)
library(tibble)
library(magrittr)
library(DBI)

drug_sub_df <- function(rec, main_node, seconadary_node = NULL, id = "drugbank-id", byValue = FALSE) {
    parent_key <- NULL
    if (!is.null(id)) {
        parent_key <- xmlValue(rec[id][[1]])
    }

    if (byValue) {
        df <- map_df(rec[main_node], xmlValue)
    } else {
        if (is.null(seconadary_node) && !is.null(rec[[main_node]])) {
            df <- xmlToDataFrame(rec[[main_node]])
        } else {
            df <- xmlToDataFrame(rec[[main_node]][[seconadary_node]])
        }

    }

    if (nrow(df) > 0 && !is.null(parent_key)) {
        df$parent_key <- parent_key
    }
    return(df)
}



#' Reads drug bank xml database and set it in memory.
#'
#' \code{get_xml_db_rows} sets drug bank db xml full tree in memory
#'
#' This functions reads drug bank xml database and sets the full tree
#'  save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' in memory directly wihtout returning it.
#' It must be called one before using parser functions, and once it is called
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again.
#'
#' @param db_full_path string, full path for the drug bank xml.
#' @return nothing but sets the db tree in memory to be used by parser
#' methods
#'
#' @examples
#' get_xml_db_rows("db_full_path")
#' get_xml_db_rows(xml_db_name = "db_full_path")
get_xml_db_rows <- function(xml_db_name) {
    drugbank_db <- xmlParse(xml_db_name)
    top <- xmlRoot(drugbank_db)
    children <<- xmlChildren(top)
}
