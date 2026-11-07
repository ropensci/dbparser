#' Parse the Health Canada Drug Product Database (DPD) Data Extract
#'
#' Reads Health Canada's Drug Product Database data extract and returns a
#' \code{dvobject} with its 13 tables. Each of the four product sets
#' (marketed, approved, cancelled, dormant) is read from its unzipped folder
#' or straight from its zip; the rows of every set go in one table with an
#' \code{extract} column.
#'
#' @details
#' \code{dpd_dir} holds the extract as Health Canada distributes it, each
#' product set either as its zip or unzipped into a folder of the same name:
#' \code{allfiles} (marketed), \code{allfiles_ap} (approved),
#' \code{allfiles_ia} (cancelled) and \code{allfiles_dr} (dormant), plus
#' \code{inactive} (the inactive products list). Zips are extracted to a
#' temporary folder that is removed after reading. When neither
#' \code{inactive} nor \code{inactive.zip} is there, \code{inactive.txt} is
#' read from the cancelled set, which holds the same file.
#'
#' The files have no header: column names are those of Health Canada's
#' read-me ("Read Me File - Drug Product Database (DPD) Data Extract"),
#' lower case, and table names are the read-me's without \code{QRYM_}. Every
#' value is quoted and the files are UTF-8; empty values become \code{NA}.
#' Code columns (\code{drug_code}, \code{active_ingredient_code},
#' \code{company_code}, \code{pharm_form_code},
#' \code{route_of_administration_code}, \code{si_code}) are integers and the
#' \code{DD-MON-YYYY} dates are dates; every other column stays text as
#' written. Dates are kept as written, including two lot expiry dates in the
#' cancelled set written with years 0030 and 0031.
#'
#' The therapeutic class files hold four fields where the read-me lists seven
#' columns: the three certain ones are kept and the parser stops if the fourth
#' ever holds a value.
#'
#' The parser groups nothing: \code{status} holds every status row as
#' written, with \code{current_status_flag} marking each product's current
#' one.
#'
#' DPD data are published under the Open Government Licence - Canada
#' (version 2.0); the attribution it asks for is recorded in the object's
#' metadata (see \code{\link{get_source_metadata}}). The DPD extract page
#' also links to Health Canada's website terms; the dataset record on
#' open.canada.ca states the Open Government Licence \enc{–}{-} Canada.
#'
#' @param dpd_dir Character. Folder holding the extract's zips or unzipped
#'   folders.
#' @param extracts Character. Product sets to read, any of
#'   \code{"marketed"}, \code{"approved"}, \code{"cancelled"} and
#'   \code{"dormant"} (all by default).
#' @param release Character or NULL. Release label, e.g. \code{"2026-10"}.
#'   DPD publishes no release number.
#' @param release_date Date, \code{"YYYY-MM-DD"} or NULL. Release date.
#' @param retrieved_at Date, \code{"YYYY-MM-DD"} or NULL. Date the files were
#'   downloaded.
#'
#' @return A \code{dvobject} with 13 tibbles, each keyed by \code{drug_code}:
#'   \code{drug_product}, \code{active_ingredients}, \code{companies},
#'   \code{form}, \code{packaging}, \code{pharmaceutical_std}, \code{route},
#'   \code{schedule}, \code{status}, \code{therapeutic_class},
#'   \code{veterinary_species}, \code{biosimilars} (each with
#'   \code{extract}) and \code{inactive_products}. \code{inactive_products}
#'   lists cancelled products, so it is built only when \code{extracts}
#'   includes \code{"cancelled"} and an inactive products file is found.
#'
#' @examples
#' dpd_dir <- system.file("extdata", "health_canada_dpd", package = "dbparser")
#' dpd <- parseHealthCanadaDPD(dpd_dir, release = "2026-10",
#'                             retrieved_at = "2026-10-21")
#' dpd$drug_product[, c("drug_code", "brand_name", "extract")]
#' get_source_metadata(dpd)
#'
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @export
#' @family parsers
parseHealthCanadaDPD <- function(dpd_dir,
                                 extracts     = c("marketed", "approved",
                                                  "cancelled", "dormant"),
                                 release      = NULL,
                                 release_date = NULL,
                                 retrieved_at = NULL) {
  # ---- Input validation ----
  if (!is.character(dpd_dir) || (length(dpd_dir) != 1L)) {
    stop("`dpd_dir` must be a single character string.", call. = FALSE)
  }
  if (!dir.exists(dpd_dir)) {
    stop("DPD folder not found at path: ", dpd_dir, call. = FALSE)
  }
  folders <- dpd_extract_folders()
  if (!is.character(extracts) || (length(extracts) == 0L) ||
      !all(extracts %in% names(folders))) {
    stop("`extracts` must be one or more of: ",
         paste(names(folders), collapse = ", "), ".", call. = FALSE)
  }
  extracts <- unique(extracts)

  # ---- Locate each set: folder, or zip extracted to a temp folder ----
  tmp_dir <- tempfile("dbparser_dpd_")
  on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
  input_files <- character()

  locate <- function(name) {
    folder <- file.path(dpd_dir, name)
    zip    <- file.path(dpd_dir, paste0(name, ".zip"))
    if (dir.exists(folder)) {
      return(list(dir = folder, zip = NULL))
    }
    if (file.exists(zip)) {
      exdir <- file.path(tmp_dir, name)
      utils::unzip(zip, exdir = exdir)
      return(list(dir = exdir, zip = zip))
    }
    NULL
  }

  set_dirs <- lapply(extracts, function(extract) {
    found <- locate(folders[[extract]]$folder)
    if (is.null(found)) {
      stop("DPD ", extract, " set not found: neither folder nor zip '",
           folders[[extract]]$folder, "' in ", dpd_dir, call. = FALSE)
    }
    found
  })
  names(set_dirs) <- extracts

  # ---- Read the 12 per-set tables ----
  specs  <- dpd_table_specs()
  tables <- lapply(setdiff(names(specs), "inactive_products"), function(table) {
    spec <- specs[[table]]
    rows <- lapply(extracts, function(extract) {
      path <- file.path(set_dirs[[extract]]$dir,
                        paste0(spec$stem, folders[[extract]]$suffix, ".txt"))
      if (is.null(set_dirs[[extract]]$zip)) {
        input_files <<- c(input_files, path)
      }
      table_rows         <- read_dpd_file(path, spec)
      table_rows$extract <- rep(extract, NROW(table_rows))
      table_rows
    })
    tibble::as_tibble(do.call(rbind, rows))
  })
  names(tables) <- setdiff(names(specs), "inactive_products")

  # ---- inactive products (the cancelled set's): own folder or zip, else
  # the copy inside the cancelled set ----
  if ("cancelled" %in% extracts) {
    inactive <- locate("inactive")
    if (is.null(inactive)) {
      inactive <- set_dirs$cancelled
    }
    inactive_path <- file.path(inactive$dir, "inactive.txt")
    if (file.exists(inactive_path)) {
      tables$inactive_products <- tibble::as_tibble(
        read_dpd_file(inactive_path, specs$inactive_products))
      input_files <- c(input_files,
                       if (is.null(inactive$zip)) inactive_path
                       else inactive$zip)
    } else {
      message("No DPD inactive products file found; ",
              "`inactive_products` is left out.")
    }
  }

  # a zip is recorded as the input, not the files extracted from it
  input_files <- c(input_files,
                   unlist(lapply(set_dirs, function(found) found$zip),
                          use.names = FALSE))

  # ---- Checks ----
  check_dpd_tables(tables)
  message(sprintf("Parsed %d DPD products from %s.",
                  NROW(tables$drug_product), paste(extracts, collapse = ", ")))

  # ---- dvobject ----
  dpd_db <- init_dvobject()
  for (table in names(tables)) {
    dpd_db[[table]] <- tables[[table]]
  }
  dpd_db <- add_database_info(dvobject         = dpd_db,
                              db_type          = "HealthCanadaDPD",
                              db_version       = release,
                              db_exported_date = release_date)
  set_source_metadata(
    dpd_db,
    source_metadata(
      source       = "health_canada_dpd",
      source_name  = "Health Canada Drug Product Database (DPD)",
      files        = sort(unique(input_files), method = "radix"),
      release      = release,
      release_date = release_date,
      retrieved_at = retrieved_at,
      source_url   = paste0("https://www.canada.ca/en/health-canada/services/",
                            "drugs-health-products/drug-products/",
                            "drug-product-database/",
                            "what-data-extract-drug-product-database.html"),
      licence      = "OGL-Canada-2.0",
      licence_url  = "https://open.canada.ca/en/open-government-licence-canada",
      attribution  = paste("Contains information licensed under the Open",
                           "Government Licence \u2013 Canada.")))
}


#' DPD product sets: folder (and zip) name and file-name suffix
#' @keywords internal
#' @return named list
dpd_extract_folders <- function() {
  list(marketed  = list(folder = "allfiles",    suffix = ""),
       approved  = list(folder = "allfiles_ap", suffix = "_ap"),
       cancelled = list(folder = "allfiles_ia", suffix = "_ia"),
       dormant   = list(folder = "allfiles_dr", suffix = "_dr"))
}


#' DPD tables as in Health Canada's read-me
#'
#' File stem, columns in file order (read-me names, lower case), integer and
#' date columns. The read-me calls the biosimilar files \code{bios*.txt}; the
#' zips hold \code{biosimilar*.txt}. The therapeutic class files hold one
#' unnamed field after the three named ones, which must be empty.
#'
#' @keywords internal
#' @return named list of table specs
dpd_table_specs <- function() {
  list(
    drug_product = list(
      stem = "drug", integer = "drug_code", date = "last_update_date",
      columns = c("drug_code", "product_categorization", "class",
                  "drug_identification_number", "brand_name", "descriptor",
                  "pediatric_flag", "accession_number", "number_of_ais",
                  "last_update_date", "ai_group_no", "class_f",
                  "brand_name_f", "descriptor_f")),
    active_ingredients = list(
      stem = "ingred", integer = c("drug_code", "active_ingredient_code"),
      date = character(0),
      columns = c("drug_code", "active_ingredient_code", "ingredient",
                  "ingredient_supplied_ind", "strength", "strength_unit",
                  "strength_type", "dosage_value", "base", "dosage_unit",
                  "notes", "ingredient_f", "strength_unit_f",
                  "strength_type_f", "dosage_unit_f")),
    companies = list(
      stem = "comp", integer = c("drug_code", "company_code"),
      date = character(0),
      columns = c("drug_code", "mfr_code", "company_code", "company_name",
                  "company_type", "address_mailing_flag",
                  "address_billing_flag", "address_notification_flag",
                  "address_other", "suite_number", "street_name",
                  "city_name", "province", "country", "postal_code",
                  "post_office_box", "province_f", "country_f")),
    form = list(
      stem = "form", integer = c("drug_code", "pharm_form_code"),
      date = character(0),
      columns = c("drug_code", "pharm_form_code", "pharmaceutical_form",
                  "pharmaceutical_form_f")),
    packaging = list(
      stem = "package", integer = "drug_code", date = character(0),
      columns = c("drug_code", "upc", "package_size_unit", "package_type",
                  "package_size", "product_information",
                  "package_size_unit_f", "package_type_f")),
    pharmaceutical_std = list(
      stem = "pharm", integer = "drug_code", date = character(0),
      columns = c("drug_code", "pharmaceutical_std")),
    route = list(
      stem = "route",
      integer = c("drug_code", "route_of_administration_code"),
      date = character(0),
      columns = c("drug_code", "route_of_administration_code",
                  "route_of_administration", "route_of_administration_f")),
    schedule = list(
      stem = "schedule", integer = "drug_code", date = character(0),
      columns = c("drug_code", "schedule", "schedule_f")),
    status = list(
      stem = "status", integer = "drug_code",
      date = c("history_date", "expiration_date"),
      columns = c("drug_code", "current_status_flag", "status",
                  "history_date", "status_f", "lot_number",
                  "expiration_date")),
    therapeutic_class = list(
      stem = "ther", integer = "drug_code", date = character(0),
      unnamed_empty_fields = 1L,
      columns = c("drug_code", "tc_atc_number", "tc_atc")),
    veterinary_species = list(
      stem = "vet", integer = "drug_code", date = character(0),
      columns = c("drug_code", "vet_species", "vet_sub_species",
                  "vet_species_f")),
    biosimilars = list(
      stem = "biosimilar", integer = c("drug_code", "si_code"),
      date = character(0),
      columns = c("drug_code", "si_desc_e", "si_desc_f", "si_code")),
    inactive_products = list(
      integer = "drug_code", date = "history_date",
      columns = c("drug_code", "drug_identification_number", "brand_name",
                  "history_date", "brand_name_f"))
  )
}


#' Parse DPD dates ("22-MAR-2025")
#'
#' English month abbreviations, whatever the session's locale.
#'
#' @param values character vector; NA stays NA
#' @param label where the values come from, for the error
#' @keywords internal
#' @return Date; stops when a set value is not a DD-MON-YYYY calendar date
parse_dpd_date <- function(values, label) {
  months <- c("JAN", "FEB", "MAR", "APR", "MAY", "JUN",
              "JUL", "AUG", "SEP", "OCT", "NOV", "DEC")
  parts  <- regmatches(values,
                       regexec("^([0-9]{2})-([A-Z]{3})-([0-9]{4})$", values))
  iso    <- vapply(parts, function(part) {
    if ((length(part) != 4L) || !(part[3] %in% months)) {
      return(NA_character_)
    }
    sprintf("%s-%02d-%s", part[4], match(part[3], months), part[2])
  }, character(1))
  dates <- as.Date(iso, format = "%Y-%m-%d")

  bad <- !is.na(values) & is.na(dates)
  if (any(bad)) {
    stop(label, ": ", sum(bad), " dates are not DD-MON-YYYY, e.g. '",
         values[bad][1], "'.", call. = FALSE)
  }
  dates
}


#' Read one DPD file
#'
#' No header, every value quoted, UTF-8. Checks the field count against the
#' read-me's columns and the parsed rows against the file's lines.
#'
#' @param path file path
#' @param spec one \code{dpd_table_specs()} entry
#' @keywords internal
#' @return data.frame with the spec's columns
read_dpd_file <- function(path, spec) {
  label <- basename(path)
  if (!file.exists(path)) {
    stop("DPD file not found: ", path, call. = FALSE)
  }

  n_extra  <- if (is.null(spec$unnamed_empty_fields)) {
    0L
  } else {
    spec$unnamed_empty_fields
  }
  n_fields <- length(spec$columns) + n_extra

  if (file.size(path) == 0) {
    rows        <- lapply(spec$columns, function(column) character(0))
    names(rows) <- spec$columns
    rows        <- as.data.frame(rows, stringsAsFactors = FALSE)
    n_lines     <- 0L
  } else {
    rows <- as.data.frame(data.table::fread(path,
                                            sep          = ",",
                                            quote        = "\"",
                                            header       = FALSE,
                                            colClasses   = "character",
                                            na.strings   = "",
                                            encoding     = "UTF-8",
                                            strip.white  = FALSE,
                                            showProgress = FALSE))
    if (ncol(rows) != n_fields) {
      stop(label, ": ", ncol(rows), " fields, the read-me gives ", n_fields,
           ".", call. = FALSE)
    }
    if (n_extra > 0L) {
      extra <- unlist(rows[, (length(spec$columns) + 1L):n_fields],
                      use.names = FALSE)
      if (any(!is.na(extra) & (extra != ""))) {
        stop(label, ": a field the read-me does not name holds values.",
             call. = FALSE)
      }
      rows <- rows[, seq_along(spec$columns), drop = FALSE]
    }
    names(rows) <- spec$columns
    # every value is quoted, so an empty value reads as "", not NA
    rows[] <- lapply(rows, function(column) {
      column[!is.na(column) & (column == "")] <- NA_character_
      column
    })
    n_lines <- length(readLines(path, warn = FALSE, encoding = "UTF-8"))
  }

  if (NROW(rows) != n_lines) {
    stop(label, ": ", NROW(rows), " rows parsed, ", n_lines, " lines.",
         call. = FALSE)
  }

  for (column in spec$integer) {
    values <- suppressWarnings(as.integer(rows[[column]]))
    if (any(is.na(values) & !is.na(rows[[column]]))) {
      stop(label, ": ", column, " is not an integer everywhere.",
           call. = FALSE)
    }
    rows[[column]] <- values
  }
  for (column in spec$date) {
    rows[[column]] <- parse_dpd_date(rows[[column]],
                                     paste(label, column))
  }
  if (anyNA(rows$drug_code)) {
    stop(label, ": drug_code missing.", call. = FALSE)
  }
  rows
}


#' Check DPD keys
#'
#' \code{drug_code} is unique in \code{drug_product}, and every other table's
#' \code{drug_code} is a product.
#'
#' @param tables named list of DPD tables
#' @keywords internal
#' @return NULL, invisibly; stops on a failed check
check_dpd_tables <- function(tables) {
  if (anyDuplicated(tables$drug_product$drug_code)) {
    stop("DPD drug_product: a drug_code is in more than one set or twice ",
         "in one.", call. = FALSE)
  }
  for (table in setdiff(names(tables), "drug_product")) {
    orphans <- setdiff(tables[[table]]$drug_code,
                       tables$drug_product$drug_code)
    if (length(orphans) > 0L) {
      stop("DPD ", table, ": ", length(orphans),
           " drug_code(s) not in drug_product, e.g. ", orphans[1], ".",
           call. = FALSE)
    }
  }
  invisible(NULL)
}
