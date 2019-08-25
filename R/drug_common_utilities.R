pkg.env <- new.env(parent = emptyenv())
pkg.env$children  <- NULL
pkg.env$con <- NULL
pkg.env$version <- NULL
pkg.env$exported_date <- NULL

get_dataset_full_path <- function(data, csv_path = ".") {
  return(ifelse(csv_path == ".", file.path(getwd(), paste0(data, ".csv")),
                file.path(csv_path, paste0(data, ".csv"))))
}

write_csv <- function(data, save_csv = FALSE, csv_path = ".") {
  if (save_csv) {
    path <- get_dataset_full_path(deparse(substitute(data)), csv_path)
    readr::write_csv(data, path)
  }
}

drug_sub_df <-
  function(rec,
           main_node,
           seconadary_node = NULL,
           id = "drugbank-id") {
    if (is.null(rec[[main_node]])) {
      return()
    }
    parent_key <- NULL
    if (!is.null(id)) {
      parent_key <- xmlValue(rec[id][[1]])
    }

    if (is.null(seconadary_node) &&
        !is.null(rec[[main_node]])) {
      df <- xmlToDataFrame(rec[[main_node]], stringsAsFactors = FALSE)
    } else {
      df <-
        xmlToDataFrame(rec[[main_node]][[seconadary_node]], stringsAsFactors = FALSE)
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
#' in memory directly without returning it.
#' It must be called one before using parser functions, and once it is called
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again.
#'
#' @param xml_db_name string, full path for the drug bank xml or zip file.
#' @return TRUE when sets the db tree in memory to be used by parser
#' methods and FALSE otherwise
#'
#' @examples
#' \donttest{
#' get_xml_db_rows("db_full_path")
#' get_xml_db_rows(xml_db_name = "db_full_path")
#' }
#' @export
get_xml_db_rows <- function(xml_db_name) {
  ext <- tools::file_ext(xml_db_name)
  dir_name <- dirname(xml_db_name)
  if (!ext %in% c("zip", "xml")) {
    stop("Unsupported file format, Kindly use an XML or zip file.")
  }

  if (ext == "zip") {
    tryCatch(
      {
        db <- unzip(xml_db_name, exdir = dir_name)
        db <- unzip(xml_db_name, list = TRUE)
        xml_db_name <- paste0(dir_name, "/", db$Name[1])
        message(xml_db_name)
      },
      error = function(e) {
        stop(e)
      }
    )
  }

  if (file.exists(xml_db_name)) {
    drugbank_db <- xmlParse(xml_db_name)
    top <- xmlRoot(drugbank_db)
    pkg.env$version <- XML::xmlAttrs(top)[["version"]]
    pkg.env$exported_date <- XML::xmlAttrs(top)[["exported-on"]]
    pkg.env$children  <- xmlChildren(top)
    return(TRUE)
  } else {
    stop(
      paste(
        "Could not find the file:",
        xml_db_name,
        ".Please ensure",
        "that the file name is entered correctly",
        "and that it exists at the specified location."
      )
    )
    return(FALSE)
  }
}

#' Establish connection to given data base
#'
#' \code{open_db} opens connection to given database.
#'
#' This function establishes connection to given database
#' to store, \emph{optionally}, the parsed drug bank elements.
#'
#' @param driver odbc object to define database driver.
#' @param server string, indicated the db server name.
#' @param output_database string, the database name to be used,
#' it has to be created before using it
#' @param trusted_connection boolean, is the connection secure
#' @return sets the open connection in memory to be used by other functions
#'
#' @examples
#' \donttest{
#' open_db(driver = "SQL Server",
#' server = "MOHAMMED\\\\SQL2016", output_database = "drugbank2")
#' }
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

#' Establish connection to given data base
#'
#' \code{open_mdb} opens connection to given database.
#'
#' This function establishes connection to given database
#' to store, \emph{optionally}, the parsed drug bank elements.
#'
#' @param driver odbc object to define database driver.
#' @param server string, indicated the db server name.
#' @param output_database string, the database name to be used,
#' it has to be created before using it
#' @param trusted_connection boolean, is the connection secure
#' @return sets the open connection in memory to be used by other functions
#'
#' @examples
#' \donttest{
#' open_mdb(driver = "SQL Server",
#' server = "MOHAMMED\\\\SQL2016", output_database = "drugbank2")
#' }
#' @export
open_mdb <-
  function(username = "root",
           password = "root",
           server,
           output_database,
           host = "localhost",
           port = 3306) {
    # db connection
    pkg.env$con <- RMariaDB::dbConnect(
      drv = RMariaDB::MariaDB(),
      dbname = output_database,
      username = username,
      password = password,
      Server = server,
      host = host,
      port = port
    )

  }

#' Close open drug bank sql database
#'
#' \code{close_db} closes connection to pre-given database.
#'
#' This function closes connection to pre-given database.
#'
#' @examples
#' \donttest{
#' close_db()
#' }
#' @export
close_db <- function() {
  dbDisconnect(conn = pkg.env$con)
}

