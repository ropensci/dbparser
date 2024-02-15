pkg_env <- new.env(parent = emptyenv())
pkg_env$root <- NULL


drug_sub_df <-
  function(rec,
           main_node,
           seconadary_node = NULL,
           id = "drugbank-id",
           # remove assinging to NULL after it being applied everywhere
           progress = NULL ) {
    if (is.null(rec[[main_node]])) {
      if (!is.null(progress)) {
        progress$tick()
      }
      return()
    }
    parent_key <- NULL
    if (!is.null(id)) {
      parent_key <- xmlValue(rec[id][[1]])
    }

    if (is.null(seconadary_node) &&
      !is.null(rec[[main_node]])) {
      df <- xmlToDataFrame(rec[[main_node]], stringsAsFactors = FALSE)
    } else if (!is.null(rec[[main_node]][[seconadary_node]])) {
      df <-
        xmlToDataFrame(rec[[main_node]][[seconadary_node]],
          stringsAsFactors = FALSE
        )
    } else {
      if (!is.null(progress)) {
        progress$tick()
      }
      return()
    }

    if (NROW(df) > 0 && !is.null(parent_key)) {
      df$parent_key <- parent_key
    }
    if (!is.null(progress)) {
      progress$tick()
    }

    df
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
#' @param db_path \strong{string}, full path for the
#'  \strong{DrugBank} xml or zip file.
#'
#' @keywords internal
#' @return loaded DB or NULL
read_drugbank_xml_db <- function(db_path) {
  message(paste("Loading DrugBank DB from path:", db_path))

  ext       <- tools::file_ext(db_path)
  dir_name  <- dirname(db_path)
  loaded_db <- NULL

  if (!ext %in% c("zip", "xml")) {
    message("Unsupported file format, Kindly use an XML or zip file.")
  } else {
    if (ext == "zip") {
      tryCatch({
        unzip(db_path, exdir = dir_name)
        db      <- unzip(db_path, list = TRUE)
        db_path <- paste0(dir_name, "/", db[[1]])
        message(paste("DrugBank DB zip file extraxted at path:", db_path))
      },
      error = function(e) {
        message(paste("Loading DrugBank DB failed due to error:", e$message))
      })
    }

    tryCatch({
      if (file.exists(db_path) &&
          (tolower(tools::file_ext(db_path)) == "xml") &&
          !is.na(file.info(db_path)$size) &&
          (file.info(db_path)$size > 0)){
        loaded_db <- xmlParse(db_path)
      } else {
        message(paste("Could not find the file:", db_path, ".Please ensure",
                      "that the file name is entered correctly",
                      "and that it exists at the specified location.")
        )
      }
    },
    error = function(e) {
      message(paste("Parsing db file failed due to error:", e$message))
    })
  }

  loaded_db
}
