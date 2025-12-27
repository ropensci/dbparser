#' init_dvobject
#'   initialize dvobject
#'
#' @keywords internal
#' @return dvobject
init_dvobject <- function() {
  dvobject        <- list()
  class(dvobject) <- "dvobject"

  attr(dvobject, "original_db_info") <- list()
  dvobject
}


#' add_database_info
#'   Assign passed databases db metadata to passed dvobject
#'
#' @param dvobject dvobject to assign metadata to it
#' @param db_type database type (defualt="DrugBank")
#' @param db_version database version as string
#' @param db_exported_date database official export date
#'
#' @return dvobject
#' @family utility
#' @export
#' @importFrom dplyr .data filter select rename mutate left_join
add_database_info <- function(dvobject,
                              db_type          = "DrugBank",
                              db_version       = NULL,
                              db_exported_date = NULL) {
  db_info <- attr(dvobject, db_type)

  db_info[["db_type"]]               <- db_type
  db_info[["db_version"]]            <- db_version
  db_info[["db_exported_date"]]      <- db_exported_date
  attr(dvobject, "original_db_info") <- db_info
  class(dvobject)                    <- "dvobject"
  dvobject
}


#' Display dvobject Metadata
#'
#' Displays information about passed dbobject object including basic info,
#' database metadata, and all data.frames contained within nested lists.
#'
#' @param obj A dvobject
#' @param return_df Logical. If TRUE, returns metadata data.frame without printing.
#'   Default is FALSE.
#'
#' @return Invisibly returns a data.frame containing dvobject metadata
#'
#' @examples
#' \dontrun{
#' display_merged_db_attrs(drugbank)
#' metadata <- display_merged_db_attrs(drugbank, return_df = TRUE)
#' }
#'
#' @export
#' @family utility
#' @importFrom utils object.size
show_dvobject_metadata <- function(obj, return_df = FALSE) {
  # Initialize return variable
  metadata <- NULL

  # Validate input
  attrs <- attributes(obj)

  if (is.null(attrs) || is.null(attrs$original_db_info)) {
    stop("Object does not have expected database attributes (missing 'original_db_info')")
  }

  # Find all dataframes in object
  df_info <- find_dataframes_recursive(obj)

  # Count only top-level lists (not data.frames)
  top_level_lists <- count_top_level_lists(obj)

  # Find second database if it exists
  second_db_info <- find_second_database(attrs)

  # Build metadata
  metadata <- build_metadata(
    attrs = attrs,
    has_second_db = second_db_info$has_second_db,
    second_db_name = second_db_info$second_db_name
  )

  # Display output if not returning data.frame
  if (!return_df) {
    cat("=== BASIC INFO ===\n")
    basic_info <- data.frame(
      Class = paste(attrs$class, collapse = ", "),
      Total_DataFrames = nrow(df_info),
      Top_Level_Lists = top_level_lists,
      Object_Size = format(object.size(obj), units = "auto"),
      stringsAsFactors = FALSE
    )
    print(basic_info)

    cat("\n=== DATABASE METADATA ===\n")
    print(metadata, row.names = FALSE)

    cat("\n=== DATA.FRAMES ===\n")
    print(df_info, row.names = FALSE)
  }

  # Return metadata
  invisible(metadata)
}


# Helper Functions for Display Attributes
# These are internal functions not exported to users

#' Find All Data Frames Recursively
#' @param x Object to search
#' @param prefix Current path prefix
#' @return Data.frame with paths, dimensions, and sizes
#' @keywords internal
find_dataframes_recursive <- function(x, prefix = "") {
  result <- NULL

  if (is.data.frame(x)) {
    result <- data.frame(
      Path = prefix,
      Rows = nrow(x),
      Cols = ncol(x),
      Size = format(object.size(x), units = "auto"),
      stringsAsFactors = FALSE
    )
  } else if (is.list(x) && (length(x) > 0)) {
    results <- lapply(names(x), function(name) {
      new_prefix <- if (prefix == "") name else paste0(prefix, "$", name)
      find_dataframes_recursive(x[[name]], new_prefix)
    })
    result <- do.call(rbind, results[!sapply(results, is.null)])
  }

  result
}

#' Count Top Level Lists
#' @param obj Object to analyze
#' @return Integer count of top-level lists (excluding data.frames)
#' @keywords internal
count_top_level_lists <- function(obj) {
  sum(sapply(obj, function(x) {
    (is.list(x) && !is.data.frame(x))
  }))
}

#' Find Second Database in Attributes
#' @param attrs Attributes list
#' @return List with has_second_db (logical) and second_db_name (character)
#' @keywords internal
find_second_database <- function(attrs) {
  result <- list(
    has_second_db  = FALSE,
    second_db_name = NULL
  )

  standard_attrs <- c("names", "class", "original_db_info", "row.names")
  potential_second_db <- setdiff(names(attrs), standard_attrs)

  if (length(potential_second_db) > 0) {
    db_name         <- potential_second_db[length(potential_second_db)]
    db_attr         <- attrs[[db_name]]
    required_fields <- c("db_type")

    if ((is.list(db_attr)) && (any(required_fields %in% names(db_attr)))) {
      result$has_second_db  <- TRUE
      result$second_db_name <- db_name
    }
  }

  result
}

#' Build Metadata Data Frame
#' @param attrs Attributes list
#' @param has_second_db Logical indicating if second database exists
#' @param second_db_name Name of second database attribute
#' @return Data.frame with database metadata
#' @keywords internal
#' @importFrom purrr %||%
build_metadata <- function(attrs, has_second_db, second_db_name) {
  metadata <- NULL

  if (has_second_db) {
    metadata <- rbind(
      data.frame(
        Database = "First Database",
        Type = attrs$DrugBankDB$db_type %||% "Unknown",
        Version = attrs$DrugBankDB$db_version %||% "Unknown",
        Export_Date = attrs$DrugBankDB$db_exported_date %||% "Unknown",
        stringsAsFactors = FALSE
      ),
      data.frame(
        Database = "Second Database",
        Type = attrs[[second_db_name]]$db_type %||% "Unknown",
        Version = attrs[[second_db_name]]$db_version %||% "Unknown",
        Export_Date = attrs[[second_db_name]]$db_exported_date %||% "Unknown",
        stringsAsFactors = FALSE
      )
    )
  } else {
    metadata <- data.frame(
      Database = "Original",
      Type = attrs$original_db_info$db_type %||% "Unknown",
      Version = attrs$original_db_info$db_version %||% "Unknown",
      Export_Date = attrs$original_db_info$db_exported_date %||% "Unknown",
      stringsAsFactors = FALSE
    )
  }

  metadata
}
