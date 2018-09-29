# required librries
library(XML)
library(purrr)
library(tibble)
library(magrittr)
library(DBI)

drug_sub_df <- function(rec,
                        main_node,
                        seconadary_node = NULL,
                        id = "drugbank-id",
                        byValue = FALSE) {
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



# read and parse the xml database
get_xml_db_rows <- function (xml_db_name) {
  drugbank_db <- xmlParse(xml_db_name)
  top <- xmlRoot(drugbank_db)
  children <<- xmlChildren(top)
}
