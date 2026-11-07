#' Parse the HGNC Gene Nomenclature Files
#'
#' Reads the HUGO Gene Nomenclature Committee (HGNC) complete set
#' (\code{hgnc_complete_set.txt}) and, optionally, the withdrawn entries
#' (\code{withdrawn.txt}) and returns a \code{dvobject} with gene, gene name
#' and withdrawn tables.
#'
#' @details
#' Both files are tab separated and UTF-8. In the complete set a cell holding
#' several values is pipe separated (\code{a|b}) and wrapped in double quotes;
#' the parser strips the wrapper and keeps the pipes. Column names are
#' lower-cased with \code{.} and \code{-} replaced by \code{_}
#' (\code{pseudogene.org} becomes \code{pseudogene_org}), and the
#' \code{date_*} columns become dates. Every other column stays text as
#' written.
#'
#' HGNC publishes no release number or date for the current files, so
#' \code{release} and \code{release_date} are \code{NA} unless given (e.g. the
#' month of the download, or the date of an HGNC archive file).
#'
#' HGNC data are released under CC0; HGNC recommends citing its latest paper,
#' which is recorded in the object's metadata (see
#' \code{\link{get_source_metadata}}).
#'
#' @param complete_set_path Character. Path to \code{hgnc_complete_set.txt}.
#' @param withdrawn_path Character or NULL. Path to \code{withdrawn.txt}. When
#'   NULL the \code{withdrawn} table is not built.
#' @param release Character or NULL. Release label, e.g. \code{"2026-10"}.
#' @param release_date Date, \code{"YYYY-MM-DD"} or NULL. Release date.
#' @param retrieved_at Date, \code{"YYYY-MM-DD"} or NULL. Date the files were
#'   downloaded.
#'
#' @return A \code{dvobject} with tibbles:
#'   \describe{
#'     \item{gene}{One row per approved gene, keyed by \code{hgnc_id}, with
#'       the file's columns. Multi-valued cells keep the file's \code{a|b}
#'       text.}
#'     \item{gene_name}{One row per (\code{hgnc_id}, \code{kind},
#'       \code{name}): each gene's approved symbol (\code{kind} "symbol") and
#'       name ("name"), and every alias and previous symbol and name
#'       ("alias_symbol", "prev_symbol", "alias_name", "prev_name"), split on
#'       \code{|} and trimmed. Empty values and exact duplicates are dropped;
#'       names that differ only in case are kept.}
#'     \item{withdrawn}{Only when \code{withdrawn_path} is given. One row per
#'       withdrawn ID and the ID it was merged into or split to
#'       (\code{merged_into_hgnc_id}, \code{merged_into_symbol},
#'       \code{merged_into_status}); an entry without a target has one row
#'       with these \code{NA}. \code{merged_into_report} keeps the original
#'       cell. Rows are as written in HGNC's file, including any repeated
#'       target.}
#'   }
#'
#' @examples
#' hgnc_dir <- system.file("extdata", "hgnc", package = "dbparser")
#' hgnc <- parseHGNC(file.path(hgnc_dir, "hgnc_complete_set.txt"),
#'                   withdrawn_path = file.path(hgnc_dir, "withdrawn.txt"),
#'                   release        = "2026-10",
#'                   retrieved_at   = "2026-10-16")
#' hgnc$gene_name[hgnc$gene_name$name == "PD1", ]
#' get_source_metadata(hgnc)
#'
#' @importFrom data.table fread
#' @importFrom tibble as_tibble tibble
#' @export
#' @family parsers
parseHGNC <- function(complete_set_path,
                      withdrawn_path = NULL,
                      release        = NULL,
                      release_date   = NULL,
                      retrieved_at   = NULL) {
  # ---- Input validation ----
  check_hgnc_path(complete_set_path, "complete_set_path")
  if (!is.null(withdrawn_path)) {
    check_hgnc_path(withdrawn_path, "withdrawn_path")
  }

  # ---- Read and build tables ----
  message("Parsing HGNC complete set...")
  gene      <- read_hgnc_complete_set(complete_set_path)
  gene_name <- build_hgnc_gene_names(gene)

  hgnc_db           <- init_dvobject()
  hgnc_db$gene      <- gene
  hgnc_db$gene_name <- gene_name

  if (!is.null(withdrawn_path)) {
    message("Parsing HGNC withdrawn entries...")
    hgnc_db$withdrawn <- read_hgnc_withdrawn(withdrawn_path)
  }

  message(sprintf("Parsed %d genes and %d gene names.",
                  NROW(gene), NROW(gene_name)))

  # ---- Metadata ----
  hgnc_db <- add_database_info(dvobject         = hgnc_db,
                               db_type          = "HGNC",
                               db_version       = release,
                               db_exported_date = release_date)
  set_source_metadata(
    hgnc_db,
    source_metadata(
      source       = "hgnc",
      source_name  = "HUGO Gene Nomenclature Committee (HGNC)",
      files        = c(complete_set_path, withdrawn_path),
      release      = release,
      release_date = release_date,
      retrieved_at = retrieved_at,
      source_url   = "https://www.genenames.org/download/statistics-and-files/",
      licence      = "CC0-1.0",
      licence_url  = "https://www.genenames.org/about/license/",
      attribution  = paste(
        "Seal RL, Braschi B, Gray K, McClay J, Tweedie S, Bruford EA.",
        "Genenames.org: the HGNC and PGNC resources in 2026. Nucleic Acids",
        "Research (Database issue). HGNC (RRID:SCR_002827).")))
}


#' Check an HGNC input path
#' @param path value passed by the caller
#' @param arg argument name, for the error message
#' @keywords internal
#' @return NULL, invisibly; stops on a bad path
check_hgnc_path <- function(path, arg) {
  if (!is.character(path) || (length(path) != 1L)) {
    stop("`", arg, "` must be a single character string.", call. = FALSE)
  }
  if (!file.exists(path)) {
    stop("HGNC file not found at path: ", path, call. = FALSE)
  }
  invisible(NULL)
}


#' Read hgnc_complete_set.txt
#'
#' Strips the double-quote wrapper of multi-valued cells, cleans column names
#' and types the \code{date_*} columns.
#'
#' @param path file path
#' @keywords internal
#' @return tibble, one row per HGNC ID
read_hgnc_complete_set <- function(path) {
  gene <- as.data.frame(data.table::fread(path,
                                          sep          = "\t",
                                          quote        = "",
                                          colClasses   = "character",
                                          na.strings   = "",
                                          encoding     = "UTF-8",
                                          showProgress = FALSE))
  gene[] <- lapply(gene, function(column) sub('^"(.*)"$', "\\1", column))
  names(gene) <- gsub("[.-]", "_", tolower(names(gene)))

  if (!all(c("hgnc_id", "symbol", "name") %in% names(gene))) {
    stop("Missing expected columns in the HGNC complete set: ",
         "hgnc_id, symbol, name.", call. = FALSE)
  }
  if (anyDuplicated(names(gene))) {
    stop("HGNC column names collide after cleaning.", call. = FALSE)
  }
  if (anyDuplicated(gene$hgnc_id) || anyNA(gene$hgnc_id) ||
      anyNA(gene$symbol)) {
    stop("HGNC `hgnc_id` must be unique and set, and `symbol` set.",
         call. = FALSE)
  }

  for (column in grep("^date_", names(gene), value = TRUE)) {
    gene[[column]] <- as.Date(gene[[column]], format = "%Y-%m-%d")
  }

  tibble::as_tibble(gene)
}


#' Build the HGNC gene_name table
#'
#' @param gene tibble from \code{read_hgnc_complete_set()}
#' @keywords internal
#' @return tibble with hgnc_id, kind and name
build_hgnc_gene_names <- function(gene) {
  name_columns <- intersect(c("alias_symbol", "prev_symbol",
                              "alias_name", "prev_name"),
                            names(gene))

  pieces <- c(
    list(tibble::tibble(hgnc_id = gene$hgnc_id,
                        kind    = "symbol",
                        name    = gene$symbol),
         tibble::tibble(hgnc_id = gene$hgnc_id,
                        kind    = "name",
                        name    = gene$name)),
    lapply(name_columns, function(column) {
      keep   <- !is.na(gene[[column]])
      values <- strsplit(gene[[column]][keep], "|", fixed = TRUE)
      tibble::tibble(hgnc_id = rep(gene$hgnc_id[keep], lengths(values)),
                     kind    = column,
                     name    = unlist(values, use.names = FALSE))
    }))

  gene_name      <- do.call(rbind, pieces)
  gene_name$name <- trimws(gene_name$name)
  gene_name      <- gene_name[!is.na(gene_name$name) &
                                nzchar(gene_name$name), ]
  gene_name[!duplicated(gene_name), ]
}


#' Read withdrawn.txt
#'
#' One row per (withdrawn ID, target); targets are \code{HGNC_ID|SYMBOL|STATUS}
#' separated by \code{", "}.
#'
#' @param path file path
#' @keywords internal
#' @return tibble
read_hgnc_withdrawn <- function(path) {
  withdrawn <- as.data.frame(data.table::fread(path,
                                               sep          = "\t",
                                               quote        = "",
                                               colClasses   = "character",
                                               na.strings   = "",
                                               encoding     = "UTF-8",
                                               showProgress = FALSE))
  if ((ncol(withdrawn) != 4L) ||
      !identical(names(withdrawn)[1:3],
                 c("HGNC_ID", "STATUS", "WITHDRAWN_SYMBOL")) ||
      !startsWith(names(withdrawn)[4], "MERGED_INTO_REPORT")) {
    stop("Unexpected columns in the HGNC withdrawn file: ",
         paste(names(withdrawn), collapse = ", "), call. = FALSE)
  }
  names(withdrawn) <- c("hgnc_id", "status", "withdrawn_symbol",
                        "merged_into_report")
  if (anyDuplicated(withdrawn$hgnc_id)) {
    stop("HGNC withdrawn `HGNC_ID` must be unique.", call. = FALSE)
  }

  targets <- strsplit(withdrawn$merged_into_report, ",\\s*")
  targets[is.na(withdrawn$merged_into_report)] <- list(NA_character_)
  rows    <- withdrawn[rep(seq_len(NROW(withdrawn)), lengths(targets)), ]
  targets <- unlist(targets, use.names = FALSE)

  parts <- strsplit(targets, "|", fixed = TRUE)
  bad   <- !is.na(targets) & (lengths(parts) != 3L)
  if (any(bad)) {
    stop(sum(bad), " HGNC withdrawn targets are not HGNC_ID|SYMBOL|STATUS.",
         call. = FALSE)
  }
  part <- function(index) {
    vapply(parts, function(x) if (length(x) == 3L) x[index] else NA_character_,
           character(1))
  }

  tibble::tibble(hgnc_id             = rows$hgnc_id,
                 status              = rows$status,
                 withdrawn_symbol    = rows$withdrawn_symbol,
                 merged_into_hgnc_id = part(1),
                 merged_into_symbol  = part(2),
                 merged_into_status  = part(3),
                 merged_into_report  = rows$merged_into_report)
}
