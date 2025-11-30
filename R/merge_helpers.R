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
#'
#' @param drugbank_db A dvobject produced by `dbparser::parseDrugBank()`.
#' @param onsides_db A dvobject produced by `dbparser::parseOnSIDES()`.
#'
#' @return A new dvobject containing the integrated data.
#'
#' @export
#' @importFrom dplyr .data filter select rename mutate left_join
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
merge_drugbank_onsides <- function(drugbank_db, onsides_db) {
  # --- Step 0: Input Validation ---
  if (!inherits(drugbank_db, "dvobject") ||
      (!"drugs" %in% names(drugbank_db))) {
    stop("`drugbank_db` must be a valid dvobject from parseDrugBank().")
  }

  if (!inherits(drugbank_db, "dvobject") ||
      (!"external_identifiers" %in% names(drugbank_db$drugs))) {
    stop("`drugbank_db` dvobject must contain external_identifiers data.")
  }

  if (!inherits(onsides_db, "dvobject") ||
      (!"vocab_rxnorm_ingredient" %in% names(onsides_db))) {
    stop("`onsides_db` must be a valid dvobject from parseOnSIDES().")
  }

  # --- Step 1: Create the Bridge (RxCUI Mapping Table) ---
  message("Creating DrugBank ID <-> RxCUI mapping table...")
  rxcui_mapping_df <- drugbank_db$drugs$external_identifiers %>%
    dplyr::filter(.data$resource == "RxCUI") %>%
    dplyr::select(all_of(drugbank_id), rxcui = .data$identifier)

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

  # Start with all of DrugBank's data
  merged_object                 <- list()
  merged_object$drugbank        <- drugbank_db
  merged_object$onsides         <- list()
  merged_object$integrated_data <- list()

  # Add all of OnSIDES's data
  for (name in names(onsides_db)) {
    merged_object$onsides[[name]] <- onsides_db[[name]]
  }

  # Overwrite the original OnSIDES tables with our new enriched versions
  merged_object$vocab_rxnorm_ingredient_enriched <- onsides_ingredient_enriched
  if (exists("onsides_hc_enriched")) {
    merged_object$integrated_data[["high_confidence_enriched"]] <- onsides_hc_enriched
  }

  # Add the mapping table itself for user reference
  merged_object$integrated_data[["DrugBank_RxCUI_Mapping"]] <- rxcui_mapping_df

  # Update metadata
  attr(merged_object, "onSideDB") <- attr(onsides_db, "original_db_info")

  # Assign a new class
  class(merged_object) <- c("DrugBankOnSIDESDb", class(merged_object))

  message("Merge complete.")
  merged_object
}
