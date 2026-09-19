#' Merge DrugBank and OnSIDES Database Objects
#'
#' Creates an integrated dvobject object by linking DrugBank dvobject with
#' OnSIDES dvobject using RxNorm CUIs as the bridge.
#'
#' @details
#' This function performs the following key steps:
#' 1. Creates a mapping table between DrugBank IDs and RxNorm CUIs from the DrugBank object.
#' 2. Enriches the relevant OnSIDES tables (`vocab_rxnorm_ingredient` and optionally
#'    `high_confidence`) by adding a `drugbank_id` column.
#' 3. Assembles a new list object containing all original tables plus the enriched ones
#'    and the ID mapping table itself.
#'
#' The resulting object allows for powerful queries that span both mechanistic data from
#' DrugBank and clinical side-effect data from OnSIDES.
#' Supports piping and chaining with other merge functions.
#'
#' @param db_object A dvobject from `parseDrugBank()` OR an existing merged
#'   dvobject (containing `$drugbank`).
#' @param onsides_db A dvobject produced by `dbparser::parseOnSIDES()`.
#'
#' @return A new dvobject containing the integrated data.
#'
#' @export
#' @family mergers
#' @importFrom dplyr filter select rename mutate left_join .data
#'
#' @examples
#' \dontrun{
#' # First, parse the individual databases
#' drugbank <- parseDrugBank("path/to/drugbank.xml")
#' onsides <- parseOnSIDES("path/to/onsides_csvs/")
#'
#' # Now, merge them into a single, powerful object
#' merged_db <- merge_drugbank_onsides(drugbank, onsides)
#'
#' # --- Example Analysis: Find the protein targets of all drugs known to ---
#' # --- cause the side effect "Hepatitis" with high confidence.        ---
#'
#' # 1. Find the MedDRA ID for "Hepatitis"
#' hepatitis_id <- merged_db$onsides$vocab_meddra_adverse_effect %>%
#'   filter(meddra_name == "Hepatitis") %>%
#'   pull(meddra_id)
#'
#' # 2. Find all drug ingredients linked to this effect in the high_confidence table
#' drug_ids_causing_hepatitis <- merged_db$onsides$high_confidence_enriched %>%
#'   filter(effect_meddra_id == hepatitis_id) %>%
#'   pull(drugbank_id) %>%
#'   na.omit() %>%
#'   unique()
#'
#' # 3. Look up the targets for these DrugBank IDs
#' targets_of_interest <- merged_db$targets %>%
#'   filter(parent_key %in% drug_ids_causing_hepatitis) %>%
#'   select(drug_id = parent_key, target_name = name, gene_name)
#'
#' head(targets_of_interest)
#' }
merge_drugbank_onsides <- function(db_object, onsides_db) {

  # --- Step 0: Input Validation and Hub Detection ---
  # This logic enables the Pipe (%>%) and Chaining.

  if ("drugbank" %in% names(db_object)) {
    # CASE A: Input is an already-merged object (e.g., passed via pipe from another merge)
    drugbank_db   <- db_object$drugbank
    merged_object <- db_object # Start with existing data to preserve previous merges
  } else {
    # CASE B: Input is a raw DrugBank object
    drugbank_db            <- db_object
    merged_object          <- init_dvobject()
    merged_object$drugbank <- db_object
    attr(merged_object, "DrugBankDB") <- attr(drugbank_db, "original_db_info")
  }

  # Validate the Hub
  if (!inherits(drugbank_db, "dvobject") ||
      (!"drugs" %in% names(drugbank_db))) {
    stop("`db_object` must contain a valid DrugBank dvobject.")
  }

  if (!inherits(drugbank_db, "dvobject") ||
      (!"external_identifiers" %in% names(drugbank_db$drugs))) {
    stop("`drugbank_db` must contain external_identifiers data.")
  }

  # Validate the Spoke
  if (!inherits(onsides_db, "dvobject") ||
      (!"vocab_rxnorm_ingredient" %in% names(onsides_db))) {
    stop("`onsides_db` must be a valid dvobject from parseOnSIDES().")
  }

  # --- Step 1: Create the Bridge (RxCUI Mapping Table) ---
  message("Creating DrugBank ID <-> RxCUI mapping table...")
  rxcui_mapping_df <- drugbank_db$drugs$external_identifiers %>%
    dplyr::filter(.data$resource == "RxCUI") %>%
    dplyr::select(all_of("drugbank_id"), rxcui = .data$identifier) %>%
    dplyr::distinct()

  # --- Step 2: Enrich OnSIDES Tables ---
  message("Enriching OnSIDES tables with DrugBank IDs...")

  # Enrich the core ingredient vocabulary
  onsides_ingredient_enriched <- onsides_db$vocab_rxnorm_ingredient %>%
    dplyr::left_join(rxcui_mapping_df, by = c("rxnorm_id" = "rxcui"))

  # Optionally enrich the high_confidence table if it exists
  if ("high_confidence" %in% names(onsides_db)) {
    onsides_hc_enriched <- onsides_db$high_confidence %>%
      dplyr::mutate(ingredient_id = as.character(.data$ingredient_id)) %>%
      dplyr::left_join(rxcui_mapping_df, by = c("ingredient_id" = "rxcui"))
  }

  # --- Step 3: Assemble the Final Merged Object ---
  message("Assembling final merged object...")

  # Add OnSIDES structure (Initialize if needed, but append to existing)
  if (is.null(merged_object$onsides)) {
    merged_object$onsides <- list()
  }

  # Copy all OnSIDES tables
  for (name in names(onsides_db)) {
    merged_object$onsides[[name]] <- onsides_db[[name]]
  }

  # Ensure integrated_data list exists
  if (is.null(merged_object$integrated_data)) {
    merged_object$integrated_data <- list()
  }

  merged_object$integrated_data[["vocab_rxnorm_ingredient_enriched"]] <- onsides_ingredient_enriched

  if (exists("onsides_hc_enriched")) {
    merged_object$integrated_data[["high_confidence_enriched"]] <- onsides_hc_enriched
  }

  # Add the mapping table itself for user reference
  merged_object$integrated_data[["DrugBank_RxCUI_Mapping"]] <- rxcui_mapping_df

  # Update metadata
  attr(merged_object, "onSideDB") <- attr(onsides_db, "original_db_info")

  # Assign a new class (Prepend to keep existing classes like DrugBankTWOSIDESDb)
  class(merged_object) <- unique(c("DrugBankOnSIDESDb", class(merged_object)))

  message("Merge complete.")
  merged_object
}


#' Merge a DrugBank dvobject with a TWOSIDES dvobject
#'
#' Integrates drug-drug interaction data from TWOSIDES with the rich mechanistic
#' information from DrugBank. This function is chainable and can accept a raw
#' DrugBank object or an already-merged dvobject.
#'
#' @param db_object A dvobject from `parseDrugBank()` or an existing merged dvobject.
#' @param twosides_db A dvobject from `parseTWOSIDES()`.
#'
#' @return A new, nested dvobject with the TWOSIDES data added.
#'
#' @importFrom dplyr filter select rename mutate left_join .data distinct
#' @family mergers
#' @export
merge_drugbank_twosides <- function(db_object, twosides_db) {

  # --- Step 0: Input Validation and Hub Detection (Pipe Friendly) ---
  if ("drugbank" %in% names(db_object)) {
    # Case A: Input is an already-merged object
    drugbank_db   <- db_object$drugbank
    merged_object <- db_object
  } else {
    # Case B: Input is a raw DrugBank object
    drugbank_db            <- db_object
    merged_object          <- init_dvobject()
    merged_object$drugbank <- db_object
    attr(merged_object, "DrugBankDB") <- attr(drugbank_db, "original_db_info")
  }

  # Validate inputs
  if (!inherits(drugbank_db, "dvobject") || (!"drugs" %in% names(drugbank_db))) {
    stop("`db_object` must contain a valid DrugBank dvobject.")
  }
  if (!inherits(drugbank_db, "dvobject") || (!"external_identifiers" %in% names(drugbank_db$drugs))) {
    stop("`drugbank_db` must contain external_identifiers data.")
  }
  if (!is.list(twosides_db) || !("drug_drug_interactions" %in% names(twosides_db))) {
    stop("`twosides_db` must be a valid dvobject from parseTWOSIDES().")
  }

  # --- Step 1: Create Bridge ---
  message("Creating DrugBank ID <-> RxCUI mapping table...")
  rxcui_mapping_df <- drugbank_db$drugs$external_identifiers %>%
    dplyr::filter(.data$resource == "RxCUI") %>%
    dplyr::select(all_of("drugbank_id"), rxcui = .data$identifier) %>%
    dplyr::mutate(rxcui = .data$rxcui) %>%
    dplyr::distinct()

  # Drug name lookup
  drug_name_lookup <- drugbank_db$drugs$general_information %>%
    dplyr::select(all_of("drugbank_id"), drug_name = .data$name)

  # --- Step 2: Enrich TWOSIDES Data ---
  message("Enriching TWOSIDES data with DrugBank information...")

  # Prepare lookup tables for double joining
  rxcui_map_1 <- rxcui_mapping_df %>% dplyr::rename(drugbank_id_1 = .data$drugbank_id)
  rxcui_map_2 <- rxcui_mapping_df %>% dplyr::rename(drugbank_id_2 = .data$drugbank_id)

  drug_name_lookup_1 <- drug_name_lookup %>%
    dplyr::rename(drug_name_1 = .data$drug_name, drugbank_id_1 = .data$drugbank_id)
  drug_name_lookup_2 <- drug_name_lookup %>%
    dplyr::rename(drug_name_2 = .data$drug_name, drugbank_id_2 = .data$drugbank_id)

  enriched_ddis <- twosides_db$drug_drug_interactions %>%
    dplyr::mutate(drug_1_rxnorn_id = as.character(.data$drug_1_rxnorn_id),
                  drug_2_rxnorm_id = as.character(.data$drug_2_rxnorm_id)) %>%
    # LOGIC CHANGE 1: Union (OR) Filter
    # Note the spelling: rxnorn
    dplyr::filter((.data$drug_1_rxnorn_id %in% rxcui_mapping_df$rxcui) |
                    (.data$drug_2_rxnorm_id %in% rxcui_mapping_df$rxcui)) %>%

    # Join for Drug 1 (using 'rxnorn' spelling)
    dplyr::left_join(rxcui_map_1, by = c("drug_1_rxnorn_id" = "rxcui")) %>%
    dplyr::left_join(drug_name_lookup_1, by = "drugbank_id_1") %>%

    # Join for Drug 2 (Twosides seems to use 'rxnorm')
    dplyr::left_join(rxcui_map_2, by = c("drug_2_rxnorm_id" = "rxcui")) %>%
    dplyr::left_join(drug_name_lookup_2, by = "drugbank_id_2") %>%

    # LOGIC CHANGE 2: Keep if at least one side matched
    dplyr::filter(!is.na(.data$drugbank_id_1) | !is.na(.data$drugbank_id_2)) %>%

    # LOGIC CHANGE 3: Fallback names to prevent NAs
    dplyr::mutate(
      drug_name_1 = dplyr::coalesce(.data$drug_name_1, .data$drug_1_concept_name),
      drug_name_2 = dplyr::coalesce(.data$drug_name_2, .data$drug_2_concept_name)
    )

  # --- Step 3: Assemble Final Object ---
  # Initialize integrated_data if it doesn't exist
  if (is.null(merged_object$integrated_data)) {
    merged_object$integrated_data <- list()
  }

  # Add the new enriched table
  merged_object$integrated_data$drug_drug_interactions <- enriched_ddis

  # Add raw Twosides data
  merged_object$twosides <- twosides_db

  # --- Step 4: Metadata ---
  attr(merged_object, "TwoSidesDB") <- attr(twosides_db, "original_db_info")

  # Prepend new class
  class(merged_object) <- unique(c("DrugBankTWOSIDESDb", class(merged_object)))

  message("Merge complete.")
  merged_object
}


# =============================================================================
# File: R/merge_drugbank_hpo.R
# Purpose: Integrate DrugBank with HPO gene-phenotype associations using
#          gene symbols as the bridge identifier.
#
# DrugBank structure navigated:
#   $cett$targets$general_information       → target_id, drugbank_id
#   $cett$targets$polypeptides$
#     general_information                   → target_id, gene_name
#
# Join path:
#   polypeptides.gene_name + polypeptides.target_id
#     → targets.target_id + targets.drugbank_id
#       → HPO.gene_symbol
#
# Author: Mohammed Ali — Interstellar Consultation Services
# License: MIT
# =============================================================================

#' Merge DrugBank and HPO Gene-Phenotype Database Objects
#'
#' Creates an integrated dvobject by linking DrugBank target gene data with
#' HPO gene-phenotype associations using gene symbols as the bridge.
#'
#' @details
#' This function performs the following key steps:
#' 1. Extracts gene names from DrugBank polypeptide data
#'    (\code{$cett$targets$polypeptides$general_information}).
#' 2. Links gene names to DrugBank IDs via the target_id bridge
#'    (\code{$cett$targets$general_information}).
#' 3. Optionally resolves DrugBank IDs to drug names.
#' 4. Enriches the HPO gene-phenotype association table by adding
#'    \code{drugbank_id} and \code{drug_name} columns.
#' 5. Assembles a merged object containing all original data plus the
#'    enriched table and the gene mapping bridge.
#'
#' Supports piping and chaining with other merge functions.
#'
#' @param db_object A dvobject from \code{parseDrugBank()} OR an existing
#'   merged dvobject (containing \code{$drugbank}).
#' @param hpo_db A dvobject produced by \code{dbparser::parseHPO()}.
#'
#' @return A new dvobject containing the integrated data.
#'
#' @export
#' @family mergers
#' @importFrom dplyr filter select mutate left_join inner_join distinct
#'   n_distinct all_of .data %>%
#' @importFrom tibble as_tibble
#'
#' @examples
#' \dontrun{
#' drugbank <- parseDrugBank("path/to/drugbank.xml")
#' hpo      <- parseHPO("path/to/gene_attribute_edges.tsv")
#'
#' # Standalone merge
#' merged_db <- merge_drugbank_hpo(drugbank, hpo)
#'
#' # Chained with other merges (any order)
#' full_db <- drugbank %>%
#'   merge_drugbank_hpo(hpo) %>%
#'   merge_drugbank_onsides(onsides) %>%
#'   merge_drugbank_twosides(twosides)
#'
#' # --- Example: Find drugs targeting genes associated with seizures ---
#' seizure_drugs <- full_db$integrated_data$gene_phenotype_enriched %>%
#'   filter(grepl("seizure", phenotype_name, ignore.case = TRUE)) %>%
#'   filter(!is.na(drugbank_id)) %>%
#'   select(drug_name, drugbank_id, gene_symbol, phenotype_name) %>%
#'   distinct()
#' }
merge_drugbank_hpo <- function(db_object, hpo_db) {

  # --- Step 0: Input Validation and Hub Detection ---
  # Enables Pipe (%>%) and Chaining with other merge functions.

  if ("drugbank" %in% names(db_object)) {
    # CASE A: Input is an already-merged object (from another merge)
    drugbank_db   <- db_object$drugbank
    merged_object <- db_object
  } else {
    # CASE B: Input is a raw DrugBank object
    drugbank_db            <- db_object
    merged_object          <- init_dvobject()
    merged_object$drugbank <- db_object
    attr(merged_object, "DrugBankDB") <- attr(drugbank_db, "original_db_info")
  }

  # Validate the Hub — need cett$targets structure
  if (!inherits(drugbank_db, "dvobject") ||
      (!"cett" %in% names(drugbank_db))) {
    stop("`db_object` must contain a valid DrugBank dvobject with `cett` data.",
         call. = FALSE)
  }

  if (!"targets" %in% names(drugbank_db$cett)) {
    stop("`db_object$cett` must contain `targets` data.",
         call. = FALSE)
  }

  # Validate the Spoke
  if (!inherits(hpo_db, "dvobject") ||
      (!"associations" %in% names(hpo_db))) {
    stop("`hpo_db` must be a valid dvobject from parseHPO() ",
         "containing `associations`.", call. = FALSE)
  }

  # --- Step 1: Build the Gene-to-DrugBankID Bridge ---
  #
  # Path: polypeptides$general_information (has gene_name + target_id)
  #     → targets$general_information      (has target_id + drugbank_id)
  #
  message("Building gene_name <-> DrugBank ID bridge via target_id...")

  # Extract polypeptides (where gene_name lives)
  polypeptides_gi <- .resolve_polypeptides_gi(drugbank_db)

  if (is.null(polypeptides_gi) || (NROW(polypeptides_gi) == 0L)) {
    stop("Could not locate polypeptides general_information table. ",
         "Expected at: $cett$targets$polypeptides$general_information",
         call. = FALSE)
  }

  # Extract targets general_information (where drugbank_id lives)
  targets_gi <- .resolve_targets_gi(drugbank_db)

  if (is.null(targets_gi) || (NROW(targets_gi) == 0L)) {
    stop("Could not locate targets general_information table. ",
         "Expected at: $cett$targets$general_information",
         call. = FALSE)
  }

  # Build the bridge: gene_name → target_id → drugbank_id
  gene_bridge <- polypeptides_gi %>%
    dplyr::filter(!is.na(.data$gene_name) &
                    (trimws(.data$gene_name) != "")) %>%
    dplyr::select(.data$gene_name, .data$target_id,
                  polypeptide_name = .data$name,
                  .data$polypeptide_id) %>%
    dplyr::distinct() %>%
    dplyr::inner_join(
      targets_gi %>%
        dplyr::select(.data$target_id, .data$drugbank_id) %>%
        dplyr::distinct(),
      by = "target_id"
    ) %>%
    dplyr::mutate(gene_name_upper = toupper(trimws(.data$gene_name)))

  message(sprintf(
    "  Bridge: %s gene-target-drug links\n    %d unique genes | %d unique targets | %d unique drugs",
    format(NROW(gene_bridge), big.mark = ","),
    dplyr::n_distinct(gene_bridge$gene_name),
    dplyr::n_distinct(gene_bridge$target_id),
    dplyr::n_distinct(gene_bridge$drugbank_id)
  ))

  # --- Step 2: Add Drug Names to Bridge ---
  drug_name_lookup <- .resolve_drug_names(drugbank_db)

  if (!is.null(drug_name_lookup) && (NROW(drug_name_lookup) > 0L)) {
    gene_bridge <- gene_bridge %>%
      dplyr::left_join(drug_name_lookup, by = "drugbank_id")
    n_named <- sum(!is.na(gene_bridge$drug_name))
    message(sprintf("  Drug names resolved for %d / %d bridge entries.",
                    n_named, NROW(gene_bridge)))
  }

  # --- Step 3: Enrich HPO Gene-Phenotype Associations ---
  message("Enriching HPO gene-phenotype associations with DrugBank IDs...")

  associations <- hpo_db$associations

  hpo_enriched <- associations %>%
    dplyr::mutate(gene_symbol_upper = toupper(trimws(.data$gene_symbol))) %>%
    dplyr::left_join(
      gene_bridge %>%
        dplyr::select(.data$gene_name_upper, .data$gene_name,
                      .data$target_id, .data$drugbank_id,
                      .data$drug_name, .data$polypeptide_name) %>%
        dplyr::distinct(),
      by = c("gene_symbol_upper" = "gene_name_upper"),
      relationship = "many-to-many"
    ) %>%
    dplyr::select(-.data$gene_symbol_upper)

  # Report match statistics
  n_hpo_genes     <- dplyr::n_distinct(associations$gene_symbol)
  n_matched_genes <- hpo_enriched %>%
    dplyr::filter(!is.na(.data$drugbank_id)) %>%
    dplyr::pull(.data$gene_symbol) %>%
    unique() %>%
    length()
  n_enriched_rows <- hpo_enriched %>%
    dplyr::filter(!is.na(.data$drugbank_id)) %>%
    NROW()

  message(sprintf(
    paste0("  %d / %d HPO genes matched to DrugBank targets (%.1f%%)\n",
           "  Total enriched rows: %s (%s with drug links)"),
    n_matched_genes, n_hpo_genes,
    100 * n_matched_genes / max(n_hpo_genes, 1L),
    format(NROW(hpo_enriched), big.mark = ","),
    format(n_enriched_rows, big.mark = ",")
  ))

  # --- Step 4: Assemble Final Merged Object ---
  message("Assembling final merged object...")

  # Store raw HPO spoke
  merged_object$hpo <- hpo_db

  # Initialize integrated_data if needed
  if (is.null(merged_object$integrated_data)) {
    merged_object$integrated_data <- list()
  }

  merged_object$integrated_data$gene_phenotype_enriched     <- hpo_enriched
  merged_object$integrated_data$DrugBank_GeneSymbol_Mapping <- gene_bridge

  # --- Step 5: Metadata ---
  attr(merged_object, "HPODB") <- attr(hpo_db, "original_db_info")

  class(merged_object) <- unique(c("DrugBankHPODb", class(merged_object)))

  message("Merge complete.")
  merged_object
}


# =============================================================================
# Internal helpers for navigating the DrugBank dvobject structure
# =============================================================================

#' Resolve polypeptides general_information table
#'
#' Navigates: $cett$targets$polypeptides$general_information
#' Contains: gene_name, target_id, polypeptide_id, name, ...
#'
#' @param drugbank_db A DrugBank dvobject.
#' @return A tibble or NULL.
#' @keywords internal
.resolve_polypeptides_gi <- function(drugbank_db) {

  # Primary path: $cett$targets$polypeptides$general_information
  pp <- tryCatch(
    drugbank_db$cett$targets$polypeptides$general_information,
    error = function(e) NULL
  )

  if (is.data.frame(pp) && (NROW(pp) > 0L) &&
      ("gene_name" %in% names(pp)) &&
      ("target_id" %in% names(pp))) {
    return(tibble::as_tibble(pp))
  }

  # Fallback: search for any table with both gene_name and target_id
  .find_table_with_columns(drugbank_db,
                           required_cols = c("gene_name", "target_id"))
}


#' Resolve targets general_information table
#'
#' Navigates: $cett$targets$general_information
#' Contains: target_id, drugbank_id, name, organism, ...
#'
#' @param drugbank_db A DrugBank dvobject.
#' @return A tibble or NULL.
#' @keywords internal
.resolve_targets_gi <- function(drugbank_db) {

  # Primary path: $cett$targets$general_information
  tgi <- tryCatch(
    drugbank_db$cett$targets$general_information,
    error = function(e) NULL
  )

  if (is.data.frame(tgi) && (NROW(tgi) > 0L) &&
      ("target_id" %in% names(tgi)) &&
      ("drugbank_id" %in% names(tgi))) {
    return(tibble::as_tibble(tgi))
  }

  # Fallback
  .find_table_with_columns(drugbank_db,
                           required_cols = c("target_id", "drugbank_id"))
}


#' Resolve drug name lookup from a DrugBank dvobject
#'
#' Returns a two-column tibble: drugbank_id, drug_name.
#' Navigates: $drugs$general_information
#'
#' @param drugbank_db A DrugBank dvobject.
#' @return A tibble or NULL.
#' @keywords internal
.resolve_drug_names <- function(drugbank_db) {

  # Primary path: $drugs$general_information
  gi <- tryCatch(
    drugbank_db$drugs$general_information,
    error = function(e) NULL
  )

  if (is.data.frame(gi) && (NROW(gi) > 0L) &&
      ("drugbank_id" %in% names(gi)) &&
      ("name" %in% names(gi))) {
    return(
      gi %>%
        dplyr::select(.data$drugbank_id, drug_name = .data$name) %>%
        dplyr::distinct()
    )
  }

  # Fallback: flat $drugs as data frame
  if (is.data.frame(drugbank_db$drugs) &&
      ("drugbank_id" %in% names(drugbank_db$drugs)) &&
      ("name" %in% names(drugbank_db$drugs))) {
    return(
      drugbank_db$drugs %>%
        dplyr::select(.data$drugbank_id, drug_name = .data$name) %>%
        dplyr::distinct()
    )
  }

  NULL
}


#' Resolve drug groups table
#'
#' @param drugbank_db A DrugBank dvobject.
#' @return A tibble with drugbank_id and group columns, or NULL.
#' @keywords internal
.resolve_drug_groups <- function(drugbank_db) {

  candidates <- list(
    tryCatch(drugbank_db$drugs$drug_groups, error = function(e) NULL),
    tryCatch(drugbank_db$drug_groups, error = function(e) NULL),
    tryCatch(drugbank_db$drugs$groups, error = function(e) NULL)
  )

  for (tbl in candidates) {
    if (is.data.frame(tbl) && (NROW(tbl) > 0L) &&
        ("drugbank_id" %in% names(tbl)) &&
        ("group" %in% names(tbl))) {
      return(tibble::as_tibble(tbl))
    }
  }
  NULL
}


#' Generic fallback: search an object tree for a data frame with required columns
#'
#' @param obj A list (possibly nested).
#' @param required_cols Character vector of column names that must all be present.
#' @param max_depth Integer. Maximum recursion depth.
#' @return A tibble or NULL.
#' @keywords internal
.find_table_with_columns <- function(obj, required_cols, max_depth = 5L) {

  if (max_depth <= 0L) return(NULL)

  if (is.data.frame(obj) && (NROW(obj) > 0L)) {
    if (all(required_cols %in% names(obj))) {
      return(tibble::as_tibble(obj))
    }
  }

  if (is.list(obj) && (!is.data.frame(obj))) {
    for (nm in names(obj)) {
      result <- .find_table_with_columns(obj[[nm]], required_cols,
                                         max_depth = max_depth - 1L)
      if (!is.null(result)) return(result)
    }
  }

  NULL
}


# =============================================================================
# Internal helpers (not exported)
# =============================================================================

#' Resolve the targets table from a DrugBank dvobject
#'
#' Handles multiple possible structures:
#'
#' \itemize{
#'   \item Full parse: \code{$targets} as data.frame with \code{gene_name}
#'   \item Nested parse: \code{$targets$<subtable>} containing \code{gene_name}
#'   \item Sample data: \code{$targets_actions} with \code{gene_name}
#' }
#'
#' @param drugbank_db A DrugBank dvobject.
#' @return A data.frame with at minimum \code{gene_name} and an ID column,
#'   or \code{NULL} if none found.
#' @keywords internal
.resolve_targets_table <- function(drugbank_db) {

  # Priority 1: Direct $targets as a data frame
  if ("targets" %in% names(drugbank_db)) {
    tgt <- drugbank_db$targets
    if (is.data.frame(tgt) && ("gene_name" %in% names(tgt))) {
      return(tgt)
    }
    # Nested structure: check sub-tables
    if (is.list(tgt) && (!is.data.frame(tgt))) {
      for (sub_name in names(tgt)) {
        sub_tbl <- tgt[[sub_name]]
        if (is.data.frame(sub_tbl) && ("gene_name" %in% names(sub_tbl))) {
          return(sub_tbl)
        }
      }
    }
  }

  # Priority 2: $targets_actions (sample RDS format)
  if ("targets_actions" %in% names(drugbank_db)) {
    tgt_a <- drugbank_db$targets_actions
    if (is.data.frame(tgt_a) && ("gene_name" %in% names(tgt_a))) {
      return(tgt_a)
    }
  }

  NULL
}
