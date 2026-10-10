#' Parse HPO Gene-Phenotype Associations
#'
#' Reads the Human Phenotype Ontology (HPO) gene-phenotype associations as
#' distributed by Harmonizome (\code{gene_attribute_edges.txt}) and returns a
#' \code{dvobject} with gene, phenotype and association tables.
#'
#' @details
#' The Harmonizome file is tab separated and has two header lines: the column
#' names (\code{source}, \code{source_desc}, \code{source_id}, \code{target},
#' \code{target_desc}, \code{target_id}, \code{weight}) followed by a
#' descriptor row (\code{GeneSym}, \code{NA}, \code{GeneID}, \code{Phenotype},
#' \code{HPOID}, \code{NA}, \code{weight}). The parser drops the descriptor row
#' and renames the columns to domain-specific names. Columns that carry no
#' information in this dataset (\code{source_desc} and the \code{-666}
#' placeholder in \code{target_id}) are not kept.
#'
#' Gzip-compressed files (\code{.gz}) are read directly.
#'
#' The resulting object can be linked to DrugBank target genes with
#' \code{\link{merge_drugbank_hpo}}.
#'
#' @param filepath Character. Path to the Harmonizome HPO
#'   \code{gene_attribute_edges.txt} file, plain or gzip-compressed.
#' @param minimum_weight Numeric. Minimum association weight to keep.
#'   The default \code{0} keeps all associations.
#' @param db_version Character or NULL. Dataset version, stored in the
#'   object metadata.
#' @param db_exported_date Character or NULL. Dataset release date, stored in
#'   the object metadata.
#'
#' @return A \code{dvobject} with three tibbles:
#'   \describe{
#'     \item{associations}{One row per gene-phenotype pair: \code{gene_symbol},
#'       \code{entrez_id}, \code{hpo_id}, \code{phenotype_name} and
#'       \code{association_weight}.}
#'     \item{genes}{Unique genes: \code{gene_symbol}, \code{entrez_id} and
#'       \code{n_phenotypes}, the number of associated phenotypes.}
#'     \item{phenotypes}{Unique phenotypes: \code{hpo_id},
#'       \code{phenotype_name} and \code{n_genes}, the number of associated
#'       genes.}
#'   }
#'
#' @examples
#' hpo_path <- system.file("extdata", "hpo_gene_attribute_edges.txt",
#'                         package = "dbparser")
#' hpo_db <- parseHPO(hpo_path)
#' hpo_db$phenotypes
#' show_dvobject_metadata(hpo_db)
#'
#' @importFrom data.table fread
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select distinct arrange count desc all_of .data
#' @export
#' @family parsers
parseHPO <- function(filepath,
                     minimum_weight   = 0,
                     db_version       = NULL,
                     db_exported_date = NULL) {
  # ---- Input validation ----
  if (!is.character(filepath) || (length(filepath) != 1L)) {
    stop("`filepath` must be a single character string.", call. = FALSE)
  }
  if (!file.exists(filepath)) {
    stop("HPO file not found at path: ", filepath, call. = FALSE)
  }
  if (!is.numeric(minimum_weight) || (length(minimum_weight) != 1L) ||
      is.na(minimum_weight) || (minimum_weight < 0)) {
    stop("`minimum_weight` must be a single non-negative number.",
         call. = FALSE)
  }

  # ---- Read raw data ----
  message("Parsing HPO gene-phenotype associations...")
  raw <- read_hpo_edges(filepath)

  # ---- Clean and filter ----
  associations <- tibble::as_tibble(raw) %>%
    dplyr::select(gene_symbol        = "source",
                  entrez_id          = "source_id",
                  hpo_id             = "target_desc",
                  phenotype_name     = "target",
                  association_weight = "weight") %>%
    dplyr::mutate(gene_symbol    = trimws(.data$gene_symbol),
                  phenotype_name = trimws(.data$phenotype_name)) %>%
    dplyr::filter(!is.na(.data$gene_symbol), .data$gene_symbol != "",
                  !is.na(.data$hpo_id), .data$hpo_id != "")

  weights <- suppressWarnings(as.numeric(associations$association_weight))
  if (any(is.na(weights) & !is.na(associations$association_weight))) {
    stop("Column `weight` contains non-numeric values.", call. = FALSE)
  }
  associations$association_weight <- weights

  if (minimum_weight > 0) {
    n_before     <- NROW(associations)
    associations <- associations %>%
      dplyr::filter(.data$association_weight >= minimum_weight)
    message(sprintf("Weight filter (>= %s): kept %d of %d associations",
                    format(minimum_weight), NROW(associations), n_before))
  }

  associations <- associations %>%
    dplyr::distinct() %>%
    dplyr::arrange(.data$gene_symbol, .data$hpo_id)

  # ---- Build lookup tables ----
  genes <- associations %>%
    dplyr::count(.data$gene_symbol, .data$entrez_id, name = "n_phenotypes") %>%
    dplyr::arrange(.data$gene_symbol)

  phenotypes <- associations %>%
    dplyr::count(.data$hpo_id, .data$phenotype_name, name = "n_genes") %>%
    dplyr::arrange(dplyr::desc(.data$n_genes), .data$hpo_id)

  message(sprintf(paste0("Parsed %d genes, %d phenotypes and ",
                         "%d gene-phenotype associations."),
                  NROW(genes), NROW(phenotypes), NROW(associations)))

  # ---- Return dvobject ----
  hpo_db                 <- init_dvobject()
  hpo_db$associations    <- associations
  hpo_db$genes           <- genes
  hpo_db$phenotypes      <- phenotypes

  add_database_info(dvobject         = hpo_db,
                    db_type          = "HPO",
                    db_version       = db_version,
                    db_exported_date = db_exported_date)
}


#' Read a Harmonizome HPO edges file
#'
#' Reads every column as character and drops the Harmonizome descriptor row.
#'
#' @param filepath path to a plain or gzip-compressed edges file
#'
#' @return data.frame with the raw Harmonizome columns
#' @noRd
#' @keywords internal
read_hpo_edges <- function(filepath) {
  fread_args <- list(sep          = "\t",
                     header       = TRUE,
                     colClasses   = "character",
                     na.strings   = c("", "NA", "na"),
                     showProgress = FALSE,
                     data.table   = FALSE)

  if (grepl("\\.gz$", filepath, ignore.case = TRUE)) {
    # fread needs the optional R.utils package for .gz files
    con <- gzfile(filepath, open = "rt")
    on.exit(close(con))
    fread_args$text <- readLines(con, warn = FALSE)
  } else {
    fread_args$file <- filepath
  }
  raw <- do.call(data.table::fread, fread_args)

  expected_cols <- c("source", "source_desc", "source_id",
                     "target", "target_desc", "target_id", "weight")
  missing_cols  <- setdiff(expected_cols, names(raw))
  if (length(missing_cols) > 0L) {
    stop("Missing expected columns: ", paste(missing_cols, collapse = ", "),
         "\nExpected a Harmonizome HPO gene_attribute_edges.txt file.",
         call. = FALSE)
  }

  descriptor_row <- (raw$source %in% "GeneSym") & (raw$weight %in% "weight")
  raw[!descriptor_row, , drop = FALSE]
}


#' Check that an object is a parsed HPO dvobject
#'
#' @param hpo_db object to check
#'
#' @return invisible TRUE, or an error
#' @noRd
#' @keywords internal
check_hpo_dvobject <- function(hpo_db) {
  if (!inherits(hpo_db, "dvobject") ||
      !is.data.frame(hpo_db$associations) ||
      !all(c("gene_symbol", "hpo_id", "phenotype_name") %in%
           names(hpo_db$associations))) {
    stop("`hpo_db` must be a dvobject returned by parseHPO().",
         call. = FALSE)
  }
  invisible(TRUE)
}


#' Search HPO Phenotypes by Keyword
#'
#' Finds phenotypes whose name contains a keyword and lists their associated
#' genes. Matching is case-insensitive and treats \code{keyword} as a plain
#' string, not a regular expression.
#'
#' @param hpo_db An HPO \code{dvobject} returned by \code{\link{parseHPO}}.
#' @param keyword Character. Search term, e.g. \code{"seizure"}.
#' @param max_results Integer. Maximum number of phenotypes to return.
#'
#' @return A tibble with columns \code{hpo_id}, \code{phenotype_name},
#'   \code{n_genes} and \code{genes} (comma-separated gene symbols), ordered
#'   by decreasing \code{n_genes}.
#'
#' @examples
#' hpo_db <- parseHPO(system.file("extdata", "hpo_gene_attribute_edges.txt",
#'                                package = "dbparser"))
#' search_phenotypes(hpo_db, "seizure")
#'
#' @importFrom dplyr filter group_by summarise arrange desc n .data
#' @importFrom utils head
#' @export
#' @family utility
search_phenotypes <- function(hpo_db, keyword, max_results = 50L) {
  check_hpo_dvobject(hpo_db)
  if (!is.character(keyword) || (length(keyword) != 1L) ||
      is.na(keyword) || (trimws(keyword) == "")) {
    stop("`keyword` must be a single non-empty character string.",
         call. = FALSE)
  }

  keyword_lower <- tolower(trimws(keyword))

  hpo_db$associations %>%
    dplyr::filter(grepl(keyword_lower, tolower(.data$phenotype_name),
                        fixed = TRUE)) %>%
    dplyr::group_by(.data$hpo_id, .data$phenotype_name) %>%
    dplyr::summarise(
      n_genes = dplyr::n_distinct(.data$gene_symbol),
      genes   = paste(sort(unique(.data$gene_symbol)), collapse = ", "),
      .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$n_genes), .data$hpo_id) %>%
    utils::head(max_results)
}


#' Get Genes Associated with a Phenotype
#'
#' Returns the genes associated with a phenotype, given either its HPO
#' identifier (e.g. \code{"HP:0001250"}) or its name.
#'
#' @param hpo_db An HPO \code{dvobject} returned by \code{\link{parseHPO}}.
#' @param phenotype Character. An HPO identifier or a phenotype name.
#' @param exact Logical. Used when \code{phenotype} is a name. If \code{TRUE},
#'   the name must match exactly (ignoring case). If \code{FALSE} (default),
#'   any phenotype whose name contains \code{phenotype} matches.
#'
#' @return A tibble with columns \code{gene_symbol}, \code{entrez_id},
#'   \code{hpo_id}, \code{phenotype_name} and \code{association_weight},
#'   ordered by gene symbol.
#'
#' @examples
#' hpo_db <- parseHPO(system.file("extdata", "hpo_gene_attribute_edges.txt",
#'                                package = "dbparser"))
#' get_genes_for_phenotype(hpo_db, "HP:0001250")
#' get_genes_for_phenotype(hpo_db, "thrombosis")
#'
#' @importFrom dplyr filter select arrange all_of .data
#' @export
#' @family utility
get_genes_for_phenotype <- function(hpo_db, phenotype, exact = FALSE) {
  check_hpo_dvobject(hpo_db)
  if (!is.character(phenotype) || (length(phenotype) != 1L) ||
      is.na(phenotype) || (trimws(phenotype) == "")) {
    stop("`phenotype` must be a single non-empty character string.",
         call. = FALSE)
  }

  query <- tolower(trimws(phenotype))

  if (grepl("^hp:[0-9]{7}$", query)) {
    matched <- hpo_db$associations %>%
      dplyr::filter(tolower(.data$hpo_id) == query)
  } else if (exact) {
    matched <- hpo_db$associations %>%
      dplyr::filter(tolower(.data$phenotype_name) == query)
  } else {
    matched <- hpo_db$associations %>%
      dplyr::filter(grepl(query, tolower(.data$phenotype_name), fixed = TRUE))
  }

  matched %>%
    dplyr::select(dplyr::all_of(c("gene_symbol", "entrez_id", "hpo_id",
                                  "phenotype_name", "association_weight"))) %>%
    dplyr::arrange(.data$gene_symbol, .data$hpo_id)
}
