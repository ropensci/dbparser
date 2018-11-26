pkg.env <- new.env(parent = emptyenv())
pkg.env$children  <- NULL
pkg.env$con <- NULL

drug_sub_df <- function(rec, main_node, seconadary_node = NULL, id = "drugbank-id", byValue = FALSE) {
    parent_key <- NULL
    if (!is.null(id)) {
        parent_key <- xmlValue(rec[id][[1]])
    }

    if (byValue) {
        df <- map_df(rec[main_node], xmlValue)
    } else {
        if (is.null(seconadary_node) && !is.null(rec[[main_node]])) {
            df <- xmlToDataFrame(rec[[main_node]], stringsAsFactors = FALSE)
        } else {
            df <- xmlToDataFrame(rec[[main_node]][[seconadary_node]], stringsAsFactors = FALSE)
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
#' @param xml_db_name string, full path for the drug bank xml.
#' @return nothing but sets the db tree in memory to be used by parser
#' methods
#'
#' @examples
#' get_xml_db_rows("db_full_path")
#' get_xml_db_rows(xml_db_name = "db_full_path")
#' @export
get_xml_db_rows <- function(xml_db_name) {
    drugbank_db <- xmlParse(xml_db_name)
    top <- xmlRoot(drugbank_db)
    pkg.env$children  <- xmlChildren(top)
    return(TRUE)
}

#' Establish connection to given data base
#'
#' \code{open_db} opens connection to given database.
#'
#' This function establishes connection to given database
#' to store, \emph{optionally}, the parsed drug bank elements.
#'
#' @param driver odbc object to define database drivr.
#' @param server string, indicated the db server name.
#' @param output_database string, the database name to be used,
#' it has to be created before using it
#' @param trusted_connection boolean, is the connection secure
#' @return sets the open connection in memory to be used by other functions
#'
#' @examples
#' open_db(xml_db_name =  "drugbank.xml", driver = "SQL Server",
#' server = "MOHAMMED\\\\SQL2016", output_database = "drugbank2")
#' @export
open_db <-
  function(driver,
           server,
           output_database,
           trusted_connection = TRUE) {
    # db connection
    pkg.env$con <- dbConnect(
      odbc(),
      Driver = driver,
      Server = server,
      Database = output_database,
      Trusted_Connection = trusted_connection
    )

  }

#' Close open drug bank sql database
#'
#' \code{close_db} closes connection to pre-given database.
#'
#' This function closes connection to pre-given databas.
#'
#' @examples
#' close_db()
#' @export
close_db <- function() {
  dbDisconnect(conn = pkg.env$con)
}
