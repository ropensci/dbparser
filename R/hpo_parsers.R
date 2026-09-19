#' #' Parse HPO Gene-Phenotype Association Data
#'
#' Reads an HPO gene-phenotype edge file (TSV format from Harmonizome) and
#' returns a structured dvobject containing normalized gene, phenotype, and
#' association tables. The output structure is consistent with
#' \code{\link[dbparser]{parseDrugBank}} and \code{\link[dbparser]{parseOnSIDES}},
#' enabling seamless cross-database integration.
#'
#' @param filepath Character. Path to the gene_attribute_edges.tsv file.
#' @param minimum_weight Numeric. Minimum association weight to retain.
#'   Default \code{0} retains all associations.
#' @param source_name Character. Data source label for metadata.
#'   Default \code{"Harmonizome HPO Gene-Phenotype"}.
#' @param version Character or NULL. Dataset version string. If NULL,
#'   extracted from file metadata or set to "unknown".
#'
#' @return A list of class \code{c("dvobject", "hpo_db", "list")} with
#'   components:
#'   \describe{
#'     \item{genes}{Tibble of unique genes with symbol, Entrez ID, and
#'       description.}
#'     \item{phenotypes}{Tibble of unique phenotypes with name, HPO ID,
#'       and association count.}
#'     \item{associations}{Tibble of all gene-phenotype
#'       associations with weights.}
#'     \item{metadata}{List of parse provenance: source, date, file path,
#'       record counts.}
#'   }
#'
#' @examples
#' \dontrun{
#'   hpo_db <- parseHPO("data/gene_attribute_edges.tsv")
#'   head(hpo_db$associations)
#'   hpo_db$metadata$record_counts
#' }
#'
#' @importFrom data.table fread setnames
#' @importFrom tibble as_tibble
#' @importFrom dplyr distinct arrange desc n
#' @export
#' @family parsers
parseHPO <- function(filepath,
                     minimum_weight = 0,
                     source_name    = "Harmonizome HPO Gene-Phenotype",
                     version        = NULL) {

    # ---- Input validation ----
    if (!is.character(filepath) || (length(filepath) != 1L)) {
        stop("`filepath` must be a single character string.", call. = FALSE)
    }
    if (!file.exists(filepath)) {
        stop("File not found: ", filepath,
             "\nPlease provide the path to gene_attribute_edges.tsv.",
             call. = FALSE)
    }
    if (!is.numeric(minimum_weight) || (minimum_weight < 0)) {
        stop("`minimum_weight` must be a non-negative number.", call. = FALSE)
    }

    # ---- Read raw data ----
    message("Parsing HPO gene-phenotype associations from:\n  ", filepath)
    parse_start <- Sys.time()

    raw <- data.table::fread(
        filepath,
        sep          = "\t",
        header       = TRUE,
        na.strings   = c("", "NA", "na"),
        showProgress = FALSE
    )

    # Validate expected columns
    expected_cols <- c("source", "source_desc", "source_id",
                       "target", "target_desc", "target_id", "weight")
    missing_cols  <- setdiff(expected_cols, names(raw))
    if (length(missing_cols) > 0L) {
        stop("Missing expected columns: ",
             paste(missing_cols, collapse = ", "),
             "\nExpected a Harmonizome gene_attribute_edges.tsv file.",
             call. = FALSE)
    }

    # ---- Standardize column names ----
    # Transform generic Harmonizome names → domain-specific names
    data.table::setnames(
        raw,
        old = c("source",      "source_desc", "source_id",
                "target",       "target_desc", "target_id",    "weight"),
        new = c("gene_symbol", "gene_desc",   "entrez_id",
                "phenotype_name", "hpo_id",   "hpo_internal_id", "association_weight")
    )

    # ---- Clean and filter ----
    # Normalize text: lowercase phenotype names for consistent matching
    raw[, phenotype_name_clean := tolower(trimws(phenotype_name))]
    raw[, gene_symbol := trimws(gene_symbol)]

    # Apply weight filter
    if (minimum_weight > 0) {
        n_before <- nrow(raw)
        raw <- raw[association_weight >= minimum_weight]
        message(sprintf("  Weight filter (>= %.3f): %s → %s associations",
                        minimum_weight, format(n_before, big.mark = ","),
                        format(nrow(raw), big.mark = ",")))
    }

    # ---- Build normalized tables ----

    # Gene table: unique genes with metadata
    genes <- raw[, .(
        gene_symbol,
        entrez_id,
        gene_desc
    )] |>
        unique() |>
        tibble::as_tibble() |>
        dplyr::arrange(gene_symbol)

    # Phenotype table: unique phenotypes with association counts
    phenotype_counts <- raw[, .(n_genes = .N), by = .(phenotype_name, hpo_id)]

    phenotypes <- tibble::as_tibble(phenotype_counts) |>
        dplyr::arrange(dplyr::desc(n_genes))

    # Association table: the core relational data
    associations <- raw[, .(
        gene_symbol,
        entrez_id,
        phenotype_name,
        phenotype_name_clean,
        hpo_id,
        association_weight
    )] |>
        tibble::as_tibble()

    parse_end <- Sys.time()
    parse_duration <- difftime(parse_end, parse_start, units = "secs")

    # ---- Assemble metadata ----
    metadata <- list(
        source       = source_name,
        version      = version %||% "unknown",
        parser       = "dbparser::parseHPO",
        parse_date   = parse_start,
        parse_time_seconds = as.numeric(parse_duration),
        file         = normalizePath(filepath, mustWork = FALSE),
        file_size_mb = round(file.info(filepath)$size / 1e6, 2),
        minimum_weight_applied = minimum_weight,
        record_counts = list(
            genes        = nrow(genes),
            phenotypes   = nrow(phenotypes),
            associations = nrow(associations)
        )
    )

    # ---- Summary message ----
    message(sprintf(
        paste0("  Parsed in %.1f seconds:\n",
               "    %s unique genes\n",
               "    %s unique phenotypes\n",
               "    %s gene-phenotype associations"),
        as.numeric(parse_duration),
        format(NROW(genes), big.mark = ","),
        format(NROW(phenotypes), big.mark = ","),
        format(NROW(associations), big.mark = ",")
    ))

    # ---- Return dvobject ----
    result                   <- init_dvobject()
    result[["genes"]]        <- genes
    result[["phenotypes"]]   <- phenotypes
    result[["associations"]] <- associations

    result <- add_database_info(
      dvobject = result,
      db_type  = "HPODB")
    result
}


#' Search phenotypes by keyword
#'
#' Convenience function for interactive exploration. Searches phenotype
#' names using case-insensitive partial matching.
#'
#' @param hpo_db An HPO dvobject returned by \code{parseHPO}.
#' @param keyword Character. Search term (e.g., "seizure", "cognitive").
#' @param max_results Integer. Maximum phenotypes to return.
#'
#' @return Tibble of matching phenotypes with gene counts.
#' @export
search_phenotypes <- function(hpo_db, keyword, max_results = 50L) {
    stopifnot(inherits(hpo_db, "hpo_db"))
    keyword_lower <- tolower(trimws(keyword))

    matches <- hpo_db$associations |>
        dplyr::filter(grepl(keyword_lower, phenotype_name_clean, fixed = FALSE)) |>
        dplyr::group_by(phenotype_name, hpo_id) |>
        dplyr::summarise(
            n_genes = dplyr::n(),
            genes   = paste(sort(unique(gene_symbol)), collapse = ", "),
            .groups = "drop"
        ) |>
        dplyr::arrange(dplyr::desc(n_genes)) |>
        head(max_results)

    matches
}


#' Get genes for a specific phenotype
#'
#' @param hpo_db An HPO dvobject.
#' @param phenotype_name Character. Exact or partial phenotype name.
#' @param exact Logical. If TRUE, match exactly. If FALSE, partial match.
#'
#' @return Tibble of genes associated with the phenotype.
#' @export
get_genes_for_phenotype <- function(hpo_db, phenotype_name, exact = FALSE) {
    stopifnot(inherits(hpo_db, "hpo_db"))

    if (exact) {
        result <- hpo_db$associations |>
            dplyr::filter(phenotype_name_clean == tolower(phenotype_name))
    } else {
        result <- hpo_db$associations |>
            dplyr::filter(grepl(tolower(phenotype_name),
                                phenotype_name_clean, fixed = FALSE))
    }

    result |>
        dplyr::select(gene_symbol, entrez_id, phenotype_name,
                       hpo_id, association_weight) |>
        dplyr::arrange(gene_symbol)
}
