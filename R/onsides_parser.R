
#' Parse the OnSIDES Core Relational Database
#'
#' Parses the core relational tables from the OnSIDES database.
#'
#' @param dataDir A string specifying the path to the directory containing the
#'   OnSIDES CSV files.
#' @param include_high_confidence Logical. If TRUE (the default), the function
#'   will also parse the `high_confidence.csv` file, which is a pre-aggregated
#'   summary of ingredient-to-effect relationships. If the file is not found,
#'   a warning is issued.
#' @param db_version used onside version (default = NULL)
#' @param db_exported_date used onside release date (default = NULL)
#'
#' @return dvobject
#' @family parsers
#'
#' @export
#' @family parsers
#' @importFrom data.table fread
parseOnSIDES <- function(dataDir,
                         include_high_confidence = TRUE,
                         db_version              = NULL,
                         db_exported_date        = NULL) {
  # The 7 canonical files
  core_files <- c(
    "product_label.csv",
    "product_adverse_effect.csv",
    "product_to_rxnorm.csv",
    "vocab_meddra_adverse_effect.csv",
    "vocab_rxnorm_ingredient.csv",
    "vocab_rxnorm_product.csv",
    "vocab_rxnorm_ingredient_to_product.csv"
  )

  file_paths        <- file.path(dataDir, core_files)
  names(file_paths) <- gsub("\\.csv$", "", core_files)

  if (!all(file.exists(file_paths))) {
    missing <- core_files[!file.exists(file_paths)]
    stop("Core files not found in '", dataDir, "':\n", paste(missing, collapse = "\n"))
  }

  message("Parsing the 7 core OnSIDES database tables...")
  db_tables <- lapply(names(file_paths), function(name) {
    message("Reading ", name, ".csv ...")
    data.table::fread(file_paths[[name]])
  })
  names(db_tables) <- names(file_paths)

  # --- Optional Handling of high_confidence.csv ---
  if (include_high_confidence) {
    hc_path <- file.path(dataDir, "high_confidence.csv")
    if (file.exists(hc_path)) {
      message("Reading high_confidence.csv ...")
      hc_table <- data.table::fread(hc_path)
      db_tables$high_confidence <- hc_table
    } else {
      warning("`include_high_confidence` was TRUE, but 'high_confidence.csv' was not found.")
    }
  }

  message("Successfully parsed OnSIDES database.")
  db_tables <- add_database_info(dvobject         = db_tables,
                                 db_type          = "OnSIDES",
                                 db_version       = db_version,
                                 db_exported_date = db_exported_date)
  db_tables
}
