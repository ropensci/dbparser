#' Parse the openFDA Drugs@FDA and NDC Directory Downloads
#'
#' Reads openFDA's bulk download files for Drugs@FDA (\code{drug/drugsfda})
#' and the NDC directory (\code{drug/ndc}) and returns a \code{dvobject} with
#' one table per record level, field names as in the files.
#'
#' @details
#' Each download is one JSON object, \code{\{meta, results\}}, usually zipped
#' (\code{drug-drugsfda-0001-of-0001.json.zip},
#' \code{drug-ndc-0001-of-0001.json.zip}). Zips are read directly, without
#' extracting them to disk; an unzipped \code{.json} file works too. The
#' parser checks the number of records against \code{meta.results.total}.
#'
#' Drugs@FDA lists FDA application products (NDA, ANDA, BLA); the NDC
#' directory lists products registered for sale, including products sold
#' under an OTC monograph without an application. Every application, product,
#' submission and listing is kept as the files have it, including tentative
#' approvals (\code{submission_status} "TA"); the parser does no status
#' grouping and no matching across sources.
#'
#' \code{YYYYMMDD} dates become dates. NDC packaging dates are written two ways
#' (\code{20220525} and \code{04-DEC-09}) and are kept as text, as written.
#' \code{finished} and \code{sample} are logical. Every other field stays text.
#' The \code{openfda} block of each record is kept as (key, field, value)
#' rows; it is openFDA's annotation, and it can belong to another drug (for
#' example ANDA202111).
#'
#' \code{release_date} defaults to the files' \code{meta.last_updated} (the
#' export date) when every file given has the same one.
#'
#' Parsing the full Drugs@FDA and NDC files takes about a minute and needs
#' roughly 300 MB of memory.
#'
#' openFDA data are released under CC0 1.0. openFDA asks for credit, which is
#' recorded in the object's metadata (see \code{\link{get_source_metadata}}),
#' and states: "Do not rely on openFDA to make decisions regarding medical
#' care."
#'
#' @param drugsfda_path Character or NULL. Path to the Drugs@FDA download
#'   (zip or JSON).
#' @param ndc_path Character or NULL. Path to the NDC directory download (zip
#'   or JSON).
#' @param release Character or NULL. Release label, e.g. \code{"2026-10"}.
#' @param release_date Date, \code{"YYYY-MM-DD"} or NULL. Release date; by
#'   default the files' export date.
#' @param retrieved_at Date, \code{"YYYY-MM-DD"} or NULL. Date the files were
#'   downloaded.
#'
#' @return A \code{dvobject} with tibbles. From Drugs@FDA:
#'   \code{drugsfda_application} (key \code{application_number}),
#'   \code{drugsfda_product} (key application and \code{product_number}),
#'   \code{drugsfda_active_ingredient}, \code{drugsfda_submission} (key
#'   application, \code{submission_type} and \code{submission_number}),
#'   \code{drugsfda_submission_property_type},
#'   \code{drugsfda_application_doc} and \code{drugsfda_openfda}. From the
#'   NDC directory: \code{ndc_product} (key \code{product_id}),
#'   \code{ndc_active_ingredient}, \code{ndc_route}, \code{ndc_pharm_class},
#'   \code{ndc_packaging} and \code{ndc_openfda}. Only the tables of the
#'   files given are built.
#'
#' @examples
#' openfda_dir <- system.file("extdata", "openfda", package = "dbparser")
#' openfda <- parseOpenFDA(
#'   drugsfda_path = file.path(openfda_dir,
#'                             "drug-drugsfda-0001-of-0001.json.zip"),
#'   ndc_path      = file.path(openfda_dir, "drug-ndc-0001-of-0001.json.zip"),
#'   release       = "2026-10")
#' openfda$drugsfda_product[, c("application_number", "brand_name",
#'                              "marketing_status")]
#' get_source_metadata(openfda)
#'
#' @importFrom tibble as_tibble
#' @export
#' @family parsers
parseOpenFDA <- function(drugsfda_path = NULL,
                         ndc_path      = NULL,
                         release       = NULL,
                         release_date  = NULL,
                         retrieved_at  = NULL) {
  # ---- Input validation ----
  if (is.null(drugsfda_path) && is.null(ndc_path)) {
    stop("Give `drugsfda_path`, `ndc_path` or both.", call. = FALSE)
  }
  for (arg in c("drugsfda_path", "ndc_path")) {
    path <- get(arg)
    if (!is.null(path)) {
      if (!is.character(path) || (length(path) != 1L)) {
        stop("`", arg, "` must be a single character string.", call. = FALSE)
      }
      if (!file.exists(path)) {
        stop("openFDA file not found at path: ", path, call. = FALSE)
      }
    }
  }

  # ---- Read ----
  specs        <- openfda_table_specs()
  tables       <- list()
  last_updated <- character()

  if (!is.null(drugsfda_path)) {
    message("Parsing openFDA Drugs@FDA...")
    parsed       <- read_openfda_json(drugsfda_path)
    tables       <- c(tables, flatten_drugsfda(parsed$results))
    last_updated <- c(last_updated, metadata_text(parsed$meta$last_updated))
  }
  if (!is.null(ndc_path)) {
    message("Parsing openFDA NDC directory...")
    parsed       <- read_openfda_json(ndc_path)
    tables       <- c(tables, flatten_ndc(parsed$results))
    last_updated <- c(last_updated, metadata_text(parsed$meta$last_updated))
  }

  for (table in names(tables)) {
    tables[[table]] <- shape_openfda_table(tables[[table]], specs[[table]],
                                           table)
  }
  check_openfda_tables(tables)

  # ---- Release date: the export date, when the files agree ----
  if (is.null(release_date)) {
    exports <- unique(last_updated)
    if (length(exports) == 1L) {
      release_date <- exports
    } else {
      message("The openFDA files have different export dates (",
              paste(exports, collapse = ", "), "); `release_date` is NA.")
    }
  }

  parsed_counts <- c(
    if (!is.null(tables$drugsfda_application)) {
      sprintf("%d Drugs@FDA applications", NROW(tables$drugsfda_application))
    },
    if (!is.null(tables$ndc_product)) {
      sprintf("%d NDC listings", NROW(tables$ndc_product))
    })
  message("Parsed ", paste(parsed_counts, collapse = " and "), ".")

  # ---- dvobject ----
  openfda_db <- init_dvobject()
  for (table in names(tables)) {
    openfda_db[[table]] <- tables[[table]]
  }
  openfda_db <- add_database_info(dvobject         = openfda_db,
                                  db_type          = "openFDA",
                                  db_version       = release,
                                  db_exported_date = release_date)
  set_source_metadata(
    openfda_db,
    source_metadata(
      source       = "openfda",
      source_name  = "openFDA (U.S. Food and Drug Administration)",
      files        = c(drugsfda_path, ndc_path),
      release      = release,
      release_date = release_date,
      retrieved_at = retrieved_at,
      source_url   = "https://open.fda.gov/data/downloads/",
      licence      = "CC0-1.0",
      licence_url  = "https://open.fda.gov/license/",
      attribution  = paste("Data provided by the U.S. Food and Drug",
                           "Administration (https://open.fda.gov)")))
}


#' openFDA tables: columns in order, date, logical and key columns
#' @keywords internal
#' @return named list of table specs
openfda_table_specs <- function() {
  submission_key <- c("application_number", "submission_type",
                      "submission_number")
  list(
    drugsfda_application = list(
      columns = c("application_number", "sponsor_name"),
      key     = "application_number"),
    drugsfda_product = list(
      columns = c("application_number", "product_number", "brand_name",
                  "dosage_form", "route", "marketing_status",
                  "reference_drug", "reference_standard", "te_code"),
      key     = c("application_number", "product_number"),
      parent  = list(c("application_number"), "drugsfda_application")),
    drugsfda_active_ingredient = list(
      columns = c("application_number", "product_number", "name",
                  "strength"),
      parent  = list(c("application_number", "product_number"),
                     "drugsfda_product")),
    drugsfda_submission = list(
      columns = c(submission_key, "submission_status",
                  "submission_status_date", "review_priority",
                  "submission_class_code",
                  "submission_class_code_description",
                  "submission_public_notes"),
      date    = "submission_status_date",
      key     = submission_key,
      parent  = list("application_number", "drugsfda_application")),
    drugsfda_submission_property_type = list(
      columns = c(submission_key, "code"),
      parent  = list(submission_key, "drugsfda_submission")),
    drugsfda_application_doc = list(
      columns = c(submission_key, "id", "type", "title", "date", "url"),
      date    = "date",
      parent  = list(submission_key, "drugsfda_submission")),
    drugsfda_openfda = list(
      columns = c("application_number", "field", "value"),
      parent  = list("application_number", "drugsfda_application")),
    ndc_product = list(
      columns = c("product_id", "product_ndc", "product_type", "brand_name",
                  "brand_name_base", "brand_name_suffix", "generic_name",
                  "labeler_name", "dosage_form", "marketing_category",
                  "application_number", "marketing_start_date",
                  "marketing_end_date", "listing_expiration_date",
                  "finished", "dea_schedule", "spl_id"),
      date    = c("marketing_start_date", "marketing_end_date",
                  "listing_expiration_date"),
      logical = "finished",
      key     = "product_id"),
    ndc_active_ingredient = list(
      columns = c("product_id", "name", "strength"),
      parent  = list("product_id", "ndc_product")),
    ndc_route = list(
      columns = c("product_id", "route"),
      parent  = list("product_id", "ndc_product")),
    ndc_pharm_class = list(
      columns = c("product_id", "pharm_class"),
      parent  = list("product_id", "ndc_product")),
    ndc_packaging = list(
      columns = c("product_id", "package_ndc", "description",
                  "marketing_start_date", "marketing_end_date", "sample"),
      logical = "sample",
      parent  = list("product_id", "ndc_product")),
    ndc_openfda = list(
      columns = c("product_id", "field", "value"),
      parent  = list("product_id", "ndc_product"))
  )
}


#' Read an openFDA download (zip or JSON)
#'
#' A zip is read through a connection, never extracted to disk.
#'
#' @param path file path
#' @keywords internal
#' @return list(meta, results), parsed without simplification
read_openfda_json <- function(path) {
  if (tolower(tools::file_ext(path)) == "zip") {
    inner <- utils::unzip(path, list = TRUE)$Name
    inner <- inner[grepl("\\.json$", inner, ignore.case = TRUE)]
    if (length(inner) != 1L) {
      stop("openFDA zip must hold one JSON file: ", path, call. = FALSE)
    }
    connection <- unz(path, inner, encoding = "UTF-8")
  } else {
    connection <- file(path, encoding = "UTF-8")
  }
  on.exit(close(connection), add = TRUE)

  parsed <- jsonlite::parse_json(
    paste(readLines(connection, warn = FALSE), collapse = "\n"),
    simplifyVector = FALSE)
  if (is.null(parsed$meta) || is.null(parsed$results)) {
    stop("Not an openFDA download (no meta and results): ", path,
         call. = FALSE)
  }

  total <- parsed$meta$results$total
  if (!is.null(total) && (length(parsed$results) != total)) {
    stop(basename(path), ": ", length(parsed$results),
         " records parsed, meta total ", total, ".", call. = FALSE)
  }
  parsed
}


#' A field's value as text, NA when absent or null
#' @param record list
#' @param name field name
#' @keywords internal
#' @return character(1)
openfda_field <- function(record, name) {
  value <- record[[name]]
  if (is.null(value) || (length(value) == 0L)) {
    NA_character_
  } else {
    as.character(value[[1]])
  }
}


#' One row per element of each record's list field
#'
#' @param records list of records
#' @param keys function(record) giving a named list of the parent's keys
#' @param list_field name of the list field, e.g. "products"
#' @param columns fields to take from each element
#' @keywords internal
#' @return data.frame, the parent key columns first
openfda_list_rows <- function(records, keys, list_field, columns) {
  rows <- lapply(records, function(record) {
    elements <- record[[list_field]]
    if (length(elements) == 0L) {
      return(NULL)
    }
    parent <- keys(record)
    values <- lapply(columns, function(column) {
      vapply(elements, function(element) {
        if (is.list(element)) {
          openfda_field(element, column)
        } else {
          as.character(element)
        }
      }, character(1))
    })
    names(values) <- columns
    c(lapply(parent, rep, length(elements)), values)
  })
  as.data.frame(data.table::rbindlist(rows, use.names = TRUE, fill = TRUE))
}


#' Each record's openfda block as (key, field, value) rows
#' @param records list of records
#' @param key_name the record's key field
#' @keywords internal
#' @return data.frame
openfda_annotation_rows <- function(records, key_name) {
  rows <- lapply(records, function(record) {
    block <- record$openfda
    if (length(block) == 0L) {
      return(NULL)
    }
    values <- lapply(block, function(value) {
      vapply(value, as.character, character(1))
    })
    key        <- list(rep(record[[key_name]], sum(lengths(values))))
    names(key) <- key_name
    c(key,
      list(field = rep(names(values), lengths(values)),
           value = unlist(values, use.names = FALSE)))
  })
  as.data.frame(data.table::rbindlist(rows, use.names = TRUE))
}


#' Drugs@FDA records into their tables
#' @param records parsed results
#' @keywords internal
#' @return named list of data.frames
flatten_drugsfda <- function(records) {
  specs           <- openfda_table_specs()
  application_key <- function(record) {
    list(application_number = record$application_number)
  }
  submission_key  <- function(submission) {
    list(application_number = submission$application_number,
         submission_type    = openfda_field(submission, "submission_type"),
         submission_number  = openfda_field(submission, "submission_number"))
  }
  with_application <- function(field) {
    unlist(lapply(records, function(record) {
      lapply(record[[field]], function(element) {
        c(element, list(application_number = record$application_number))
      })
    }), recursive = FALSE)
  }
  products    <- with_application("products")
  submissions <- with_application("submissions")

  list(
    drugsfda_application = data.frame(
      application_number = vapply(records, openfda_field, character(1),
                                  "application_number"),
      sponsor_name       = vapply(records, openfda_field, character(1),
                                  "sponsor_name"),
      stringsAsFactors   = FALSE),
    drugsfda_product = openfda_list_rows(
      records, application_key, "products",
      specs$drugsfda_product$columns[-1]),
    drugsfda_active_ingredient = openfda_list_rows(
      products,
      function(product) {
        list(application_number = product$application_number,
             product_number     = openfda_field(product, "product_number"))
      },
      "active_ingredients", c("name", "strength")),
    drugsfda_submission = openfda_list_rows(
      records, application_key, "submissions",
      specs$drugsfda_submission$columns[-1]),
    drugsfda_submission_property_type = openfda_list_rows(
      submissions, submission_key, "submission_property_type", "code"),
    drugsfda_application_doc = openfda_list_rows(
      submissions, submission_key, "application_docs",
      c("id", "type", "title", "date", "url")),
    drugsfda_openfda = openfda_annotation_rows(records, "application_number"))
}


#' NDC directory records into their tables
#' @param records parsed results
#' @keywords internal
#' @return named list of data.frames
flatten_ndc <- function(records) {
  product_columns <- openfda_table_specs()$ndc_product$columns
  product_key     <- function(record) list(product_id = record$product_id)

  product        <- lapply(product_columns, function(column) {
    vapply(records, openfda_field, character(1), column)
  })
  names(product) <- product_columns

  list(
    ndc_product = as.data.frame(product, stringsAsFactors = FALSE),
    ndc_active_ingredient = openfda_list_rows(
      records, product_key, "active_ingredients", c("name", "strength")),
    ndc_route = openfda_list_rows(records, product_key, "route", "route"),
    ndc_pharm_class = openfda_list_rows(records, product_key, "pharm_class",
                                        "pharm_class"),
    ndc_packaging = openfda_list_rows(
      records, product_key, "packaging",
      c("package_ndc", "description", "marketing_start_date",
        "marketing_end_date", "sample")),
    ndc_openfda = openfda_annotation_rows(records, "product_id"))
}


#' Parse openFDA dates (YYYYMMDD)
#' @param values character vector; NA and "" give NA
#' @param label where the values come from, for the error
#' @keywords internal
#' @return Date; stops when a set value is not a YYYYMMDD date
parse_openfda_date <- function(values, label) {
  values[!is.na(values) & (values == "")] <- NA_character_
  dates <- as.Date(ifelse(grepl("^[0-9]{8}$", values), values, NA_character_),
                   format = "%Y%m%d")
  bad   <- !is.na(values) & is.na(dates)
  if (any(bad)) {
    stop(label, ": ", sum(bad), " dates are not YYYYMMDD, e.g. '",
         values[bad][1], "'.", call. = FALSE)
  }
  dates
}


#' Shape an openFDA table: its columns in order, dates and logicals typed
#' @param rows data.frame (possibly with no rows or columns)
#' @param spec one \code{openfda_table_specs()} entry
#' @param table table name, for errors
#' @keywords internal
#' @return tibble
shape_openfda_table <- function(rows, spec, table) {
  if (NROW(rows) == 0L) {
    rows <- as.data.frame(matrix(character(0), nrow = 0L,
                                 ncol = length(spec$columns)),
                          stringsAsFactors = FALSE)
    names(rows) <- spec$columns
  }
  for (column in setdiff(spec$columns, names(rows))) {
    rows[[column]] <- rep(NA_character_, NROW(rows))
  }
  rows <- rows[, spec$columns, drop = FALSE]

  for (column in spec$date) {
    rows[[column]] <- parse_openfda_date(rows[[column]],
                                         paste(table, column))
  }
  for (column in spec$logical) {
    values <- rows[[column]]
    if (any(!is.na(values) & !(values %in% c("TRUE", "FALSE")))) {
      stop(table, " ", column, ": values other than true/false.",
           call. = FALSE)
    }
    rows[[column]] <- as.logical(values)
  }
  tibble::as_tibble(rows)
}


#' Check openFDA keys and references
#' @param tables named list of shaped tables
#' @keywords internal
#' @return NULL, invisibly; stops on a failed check
check_openfda_tables <- function(tables) {
  specs <- openfda_table_specs()
  for (table in names(tables)) {
    spec <- specs[[table]]
    if (!is.null(spec$key) &&
        anyDuplicated(as.data.frame(tables[[table]])[, spec$key,
                                                      drop = FALSE])) {
      stop("openFDA ", table, ": key (", paste(spec$key, collapse = ", "),
           ") is not unique.", call. = FALSE)
    }
    if (!is.null(spec$parent)) {
      columns <- spec$parent[[1]]
      parent  <- as.data.frame(tables[[spec$parent[[2]]]])[, columns,
                                                            drop = FALSE]
      child   <- as.data.frame(tables[[table]])[, columns, drop = FALSE]
      orphans <- !do.call(paste, c(child, sep = "\r")) %in%
        do.call(paste, c(parent, sep = "\r"))
      if (any(orphans)) {
        stop("openFDA ", table, ": ", sum(orphans), " rows name no ",
             spec$parent[[2]], " row.", call. = FALSE)
      }
    }
  }
  invisible(NULL)
}
