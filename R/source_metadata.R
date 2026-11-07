#' source_metadata
#'   Build the source metadata block that every parser attaches to its
#'   dvobject.
#'
#' Unknown fields stay \code{NA}; nothing is guessed. \code{retrieved_at} is
#' never taken from file modification dates. File hashes are SHA-256 and need
#' R >= 4.5.0 (\code{tools::sha256sum()}); on older R they are \code{NA}.
#'
#' @param source short source id, e.g. "hgnc"
#' @param source_name full source name
#' @param files paths of the input files that were parsed
#' @param release source release label as published, e.g. "5.1"
#' @param release_date source release date (Date or "YYYY-MM-DD")
#' @param retrieved_at date the files were downloaded (Date or "YYYY-MM-DD")
#' @param source_url source download or home page
#' @param licence SPDX licence id, e.g. "CC0-1.0"
#' @param licence_url licence or terms page
#' @param attribution credit or citation sentence the source asks for
#'
#' @keywords internal
#' @return list, the metadata block
source_metadata <- function(source,
                            source_name  = source,
                            files        = character(),
                            release      = NA,
                            release_date = NA,
                            retrieved_at = NA,
                            source_url   = NA,
                            licence      = NA,
                            licence_url  = NA,
                            attribution  = NA) {
  if (!is.character(source) || (length(source) != 1L) || is.na(source)) {
    stop("`source` must be a single character string.", call. = FALSE)
  }

  list(
    metadata_version = "1",
    source           = source,
    source_name      = metadata_text(source_name),
    release          = metadata_text(release),
    release_date     = metadata_date(release_date),
    retrieved_at     = metadata_date(retrieved_at),
    source_url       = metadata_text(source_url),
    licence          = metadata_text(licence),
    licence_url      = metadata_text(licence_url),
    attribution      = metadata_text(attribution),
    files            = metadata_files(files),
    parser           = paste("dbparser", utils::packageVersion("dbparser"))
  )
}


#' set_source_metadata
#'   Attach a metadata block to a dvobject
#'
#' Blocks are kept in one attribute, \code{source_metadata}, as a list named by
#' each block's \code{source}. A block for a source already present replaces it.
#'
#' @param dvobject dvobject to assign metadata to it
#' @param metadata block from \code{source_metadata()}, or a named list of
#'   blocks (as stored on another dvobject)
#'
#' @keywords internal
#' @return dvobject
set_source_metadata <- function(dvobject, metadata) {
  if (is.null(metadata)) {
    return(dvobject)
  }

  if (!is.null(metadata$metadata_version)) {
    metadata        <- list(metadata)
    names(metadata) <- metadata[[1]]$source
  }

  blocks <- attr(dvobject, "source_metadata")
  if (is.null(blocks)) {
    blocks <- list()
  }
  blocks[names(metadata)]            <- metadata
  attr(dvobject, "source_metadata") <- blocks
  dvobject
}


#' Get the source metadata of a dvobject
#'
#' Returns where each source in a dvobject came from: release, dates, URLs,
#' licence, attribution, the parsed files with their SHA-256 hashes, and the
#' dbparser version that parsed them. A merged dvobject has one row per
#' source. Unknown values are \code{NA}.
#'
#' @param dvobject a dvobject returned by a parser or a merge function
#'
#' @return a tibble with one row per source and columns
#'   \code{metadata_version}, \code{source}, \code{source_name},
#'   \code{release}, \code{release_date}, \code{retrieved_at},
#'   \code{source_url}, \code{licence}, \code{licence_url},
#'   \code{attribution}, \code{files} (a list column of data.frames with
#'   \code{name} and \code{sha256}) and \code{parser}. It has no rows when
#'   the dvobject carries no source metadata (e.g. one saved by an older
#'   dbparser version).
#'
#' @examples
#' drugbank <- parseDrugBank(
#'   system.file("extdata", "drugbank_record_biotech.xml",
#'               package = "dbparser"))
#' get_source_metadata(drugbank)
#'
#' @family utility
#' @export
get_source_metadata <- function(dvobject) {
  blocks <- attr(dvobject, "source_metadata", exact = TRUE)
  if (is.null(blocks)) {
    blocks <- list()
  }

  text_column <- function(field) {
    vapply(blocks, function(block) metadata_text(block[[field]]),
           character(1), USE.NAMES = FALSE)
  }
  date_column <- function(field) {
    as.Date(vapply(blocks, function(block) as.character(block[[field]]),
                   character(1), USE.NAMES = FALSE))
  }

  tibble::tibble(
    metadata_version = text_column("metadata_version"),
    source           = text_column("source"),
    source_name      = text_column("source_name"),
    release          = text_column("release"),
    release_date     = date_column("release_date"),
    retrieved_at     = date_column("retrieved_at"),
    source_url       = text_column("source_url"),
    licence          = text_column("licence"),
    licence_url      = text_column("licence_url"),
    attribution      = text_column("attribution"),
    files            = unname(lapply(blocks, function(block) block$files)),
    parser           = text_column("parser")
  )
}


#' metadata_text
#'   Single string or NA
#' @param x value
#' @keywords internal
#' @return character(1)
metadata_text <- function(x) {
  if (is.null(x) || (length(x) == 0L) || all(is.na(x))) {
    return(NA_character_)
  }
  as.character(x[[1]])
}


#' metadata_date
#'   Date from a Date or a "YYYY-MM-DD" string, else NA with a message
#' @param x value
#' @keywords internal
#' @return Date
metadata_date <- function(x) {
  if (is.null(x) || (length(x) == 0L) || all(is.na(x))) {
    return(as.Date(NA))
  }
  if (inherits(x, "Date")) {
    return(x[[1]])
  }

  text   <- as.character(x[[1]])
  parsed <- as.Date(NA)
  if (grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", text)) {
    parsed <- as.Date(text, format = "%Y-%m-%d")
  }
  if (is.na(parsed)) {
    message("Metadata date '", x[[1]], "' is not YYYY-MM-DD; recorded as NA.")
  }
  parsed
}


#' metadata_files
#'   File names and SHA-256 hashes
#' @param files file paths
#' @keywords internal
#' @return data.frame with name and sha256
metadata_files <- function(files) {
  files  <- as.character(files)
  sha256 <- rep(NA_character_, length(files))

  if (length(files) > 0L) {
    tools_ns <- asNamespace("tools")
    if (exists("sha256sum", envir = tools_ns, inherits = FALSE)) {
      sha256 <- unname(get("sha256sum", envir = tools_ns)(files))
    } else {
      message("SHA-256 file hashes need R >= 4.5.0; recorded as NA.")
    }
  }

  data.frame(name             = basename(files),
             sha256           = sha256,
             stringsAsFactors = FALSE)
}
