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
    dplyr::mutate(rxcui = as.integer(.data$rxcui)) %>%
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
    # LOGIC CHANGE 1: Union (OR) Filter
    # Note the spelling: rxnorn
    dplyr::filter((.data$drug_1_rxnorn_id %in% rxcui_mapping_df$rxcui) |
                    (.data$drug_2_rxnorm_id %in% rxcui_mapping_df$rxcui)) %>%

    # Join for Drug 1 (using 'rxnorn' spelling)
    dplyr::left_join(rxcui_map_1, by = c("drug_1_rxnorn_id" = "rxcui")) %>%
    dplyr::left_join(drug_name_lookup_1, by = "drugbank_id_1") %>%

    # Join for Drug 2 (Twosides seems to use 'rxnorm' for the second one? Or check if consistent)
    # Based on your screenshot, assuming consistent spelling might be safer,
    # but check your colnames. Usually data is consistent.
    # If the second col is "drug_2_rxnorm_id" (with m), keep as is.
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
