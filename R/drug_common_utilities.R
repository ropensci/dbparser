pkg_env <- new.env(parent = emptyenv())
pkg_env$children <- NULL
pkg_env$version <- NULL
pkg_env$exported_date <- NULL
pkg_env$root <- NULL

get_dataset_full_path <- function(data, csv_path = ".") {
  return(ifelse(csv_path == ".", file.path(getwd(), paste0(data, ".csv")),
    file.path(csv_path, paste0(data, ".csv"))
  ))
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
        xmlToDataFrame(rec[[main_node]][[seconadary_node]],
          stringsAsFactors = FALSE
        )
    }



    if (nrow(df) > 0 && !is.null(parent_key)) {
      df$parent_key <- parent_key
    }
    return(df)
  }



#' Reads \strong{DrugBank} xml database and load it into memory.
#'
#' \code{read_drugbank_xml_db} loads \strong{DrugBank} xml database full tree
#' into memory.
#'
#' This functions reads \strong{DrugBank} xml database and load it into memory
#'  for later processing. Hence; this method \strong{must} be called before any
#'  other function in the package and it needs to be called one time only.
#'
#' It takes one single mandatory argument which is the location of DrugBank db.
#'
#' @param drugbank_db_path \strong{string}, full path for the
#'  \strong{DrugBank} xml or zip file.
#' @return \strong{TRUE} when the loading process into memory to be used by
#' parser methods is completed successfully and \strong{FALSE} otherwise.
#'
#'@family DrugBank DB Loading
#'
#' @examples
#' \dontrun{
#' read_drugbank_xml_db("db_full_path")
#' read_drugbank_xml_db(drugbank_db_path = "db_full_path")
#' }
#' @export
read_drugbank_xml_db <- function(drugbank_db_path) {
  ext <- tools::file_ext(drugbank_db_path)
  dir_name <- dirname(drugbank_db_path)
  if (!ext %in% c("zip", "xml")) {
    stop("Unsupported file format, Kindly use an XML or zip file.")
  }

  if (ext == "zip") {
    tryCatch({
        unzip(drugbank_db_path, exdir = dir_name)
        db <- unzip(drugbank_db_path, list = TRUE)
        drugbank_db_path <- paste0(dir_name, "/", db[[1]])
        message(drugbank_db_path)
      },
      error = function(e) {
        stop(e)
      }
    )
  }

  if (file.exists(drugbank_db_path)) {
    # XML_Old
    drugbank_db <- xmlParse(drugbank_db_path)
    pkg_env$root <- xmlRoot(drugbank_db)
    pkg_env$children <- xmlChildren(root)
    pkg_env$version <- xmlGetAttr(root, name = "version")
    pkg_env$exported_date <- xmlGetAttr(root, name = "exported-on")
    return(TRUE)
  } else {
    stop(
      paste(
        "Could not find the file:",
        drugbank_db_path,
        ".Please ensure",
        "that the file name is entered correctly",
        "and that it exists at the specified location."
      )
    )
    return(FALSE)
  }
}

check_parameters_validation <- function(save_table, database_connection) {
  if (save_table && is.null(database_connection)) {
    stop("Please provide a valid database connection.")
  }

  if (is.null(pkg_env$children)) {
    stop("Please make sure to call read_drugbank_xml_db method first")
  }
}
