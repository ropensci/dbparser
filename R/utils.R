#' Create a reusable helper function to process one component (carrier, enzyme, etc.)
#' @param component the component to subset from CETT
#' @param component_name componemt name
#' @param drug_ids passed drugs ids to subset for
#'
#' @return A new, smaller dvobject with the same structure.
#' @noRd
#' @keywords internal
subset_cett_component <- function(component, component_name, drug_ids) {
  new_component <- list()

  if (!is.null(component) && (NROW(component$general_information) > 0)) {
    # The name of the intermediate ID, e.g., "carrier_id", "target_id"
    intermediate_id_col <- paste0(sub("s$", "", component_name), "_id")

    # Step A: Filter the top-level `general_information` table by drug_id.
    # This is our anchor.
    general_info_filtered <- component$general_information %>%
      dplyr::filter(drugbank_id %in% drug_ids)

    # Step B: From this anchor, get the set of relevant intermediate IDs.
    relevant_intermediate_ids <- general_info_filtered[[intermediate_id_col]] %>%
      unique()
    # No matching items for this component
    if (length(relevant_intermediate_ids) > 0) {
      # Step C: Use these intermediate IDs to filter all other tables in the component.
      new_component                     <- list()
      new_component$general_information <- general_info_filtered
      new_component$actions             <- component$actions %>%
        dplyr::filter(.data[[intermediate_id_col]] %in% relevant_intermediate_ids)

      # Step D: Recurse into the `polypeptides` list, using the same intermediate IDs
      if (!is.null(component$polypeptides)) {
        new_component$polypeptides <- list()
        for (poly_table_name in names(component$polypeptides)) {
          poly_table <- component$polypeptides[[poly_table_name]]
          if (is.data.frame(poly_table) && intermediate_id_col %in% names(poly_table)) {
            new_component$polypeptides[[poly_table_name]] <- poly_table %>%
              dplyr::filter(.data[[intermediate_id_col]] %in% relevant_intermediate_ids)
          }
        }
      }
    }
  }

  new_component
}


#' Subset a DrugBank dvobject by a vector of DrugBank IDs
#'
#' @details
#' Intelligently filters a DrugBank dvobject to retain only the data associated
#' with a specified list of drugbank_ids. It correctly handles the deep,
#' multi-level nested structure of the entire object, including the complex
#' relationships within the `cett` list.
#'
#' @param dvobject The dvobject from `parseDrugBank()`.
#' @param drug_ids A character vector of `drugbank_id` values to keep.
#'
#' @return A new, smaller dvobject with the same structure and attributes.
#'
#' @export
#' @importFrom dplyr .data filter
#'
#' @examples
#' \dontrun{
#' library(dbparser)
#' one_drug <- subset_drugbank_dvobject(dvobject = dbdataset::drugbank,
#'                                      drug_ids = "DB00001")
#' }
#' @family utils
subset_drugbank_dvobject <- function(dvobject, drug_ids) {

  new_dvobject <- NULL

  if ((length(drug_ids) == 0) || (sum(nchar(drug_ids)) == 0)) {
    warning("`drug_ids` is empty. Returning NULL")
  } else {
    new_dvobject <- init_dvobject()

    # --- 1. Filter the `drugs` list (many sub-tables) ---
    if (!is.null(dvobject$drugs)) {
      message("Subsetting `drugs` list...")
      new_dvobject$drugs <- list()
      for (name in names(dvobject$drugs)) {
        sub_table <- dvobject$drugs[[name]]
        # Most tables here link directly via drugbank_id
        if (is.data.frame(sub_table) && ("drugbank_id" %in% names(sub_table))) {
          filtered_subtable<- sub_table %>%
            dplyr::filter(drugbank_id %in% drug_ids)
          if (NROW(filtered_subtable) > 0) {
            new_dvobject$drugs[[name]] <- filtered_subtable
          }
        }
      }
    }

    # --- 2. Filter the `salts`, `products` data.frames ---
    for (name in c("salts", "products")) {
      if (NROW(dvobject[[name]]) > 0) {
        message(paste("Subsetting", name, "..."))
        filtered_subtable <- dvobject[[name]] %>%
          dplyr::filter(drugbank_id %in% drug_ids)
        if (NROW(filtered_subtable) > 0) {
          new_dvobject[[name]] <- filtered_subtable
        }
      }
    }

    # --- 3. Filter the drugs`references` list ---
    if (!is.null(dvobject$references) && !is.null(dvobject$references$drugs)) {
      message("Subsetting drugs `references` list...")
      new_dvobject$references$drugs <- list()
      for (name in names(dvobject$references$drugs)) {
        sub_table <- dvobject$references$drugs[[name]]
        if (is.data.frame(sub_table) && "drugbank_id" %in% names(sub_table)) {
          filtered_subtable <- sub_table %>%
            dplyr::filter(drugbank_id %in% drug_ids)
          if (NROW(filtered_subtable) > 0) {
            new_dvobject$references$drugs[[name]] <- filtered_subtable
          }
        }
      }
    }

    # --- 4. Filter the complex, multi-level `cett` List ---
    if (!is.null(dvobject$cett)) {
      message("Subsetting complex `cett` list...")
      new_dvobject$cett <- list()

      # Apply the helper to each component within cett
      for (cett_name in c("carriers", "enzymes", "targets", "transporters")) {
        component <- subset_cett_component(
          component      = dvobject$cett[[cett_name]],
          component_name = cett_name,
          drug_ids       = drug_ids)

        if (length(component) > 0) {
          new_dvobject$cett[[cett_name]] <- component
        }
      }
    }

    # --- 5. Filter the CETT`references` list ---
    if (!is.null(dvobject$references)) {
      for (cett_name in c("carriers", "enzymes", "targets", "transporters")) {
        if ((length(dvobject$references[[cett_name]]) > 0) &&
            (length(new_dvobject$cett[[cett_name]]) > 0)) {
          message("Subsetting ", cett_name ," references list...")
          cett_references <- list()
          # The name of the intermediate ID, e.g., "carrier_id", "target_id"
          intermediate_id_col <- paste0(sub("s$", "", cett_name), "_id")

          for (name in names(dvobject$references[[cett_name]])) {
            sub_table <- dvobject$references[[cett_name]][[name]]
            if (is.data.frame(sub_table) && (intermediate_id_col %in% names(sub_table))) {
              filtered_subtable <- sub_table %>%
                dplyr::filter(.data[[intermediate_id_col]] %in% new_dvobject$cett[[cett_name]][["general_information"]][[intermediate_id_col]])
              if (NROW(filtered_subtable) > 0) {
                new_dvobject$references[[cett_name]][[name]] <- filtered_subtable
              }
            }
          }
        }
      }
    }

    attr(new_dvobject, "original_db_info") <- attr(dvobject, "original_db_info")
    class(dvobject) <- "dvobject"

    # --- Final Step: Preserve original object's attributes ---
    #attributes(new_dvobject) <- attributes(dvobject)
    message("Subsetting complete.")
  }

  new_dvobject
}


#' Subset an OnSIDES dvobject by a vector of RxNorm Ingredient IDs (Schema-Aware)
#'
#' Intelligently filters an OnSIDES dvobject by cascading filters through the
#' relational tables, ensuring the final subset is self-consistent.
#'
#' @param dvobject A dvobject from `parseOnSIDES()`.
#' @param ingredient_ids A character vector of RxNorm CUIs (ingredients) to keep.
#'
#' @return A new, smaller dvobject with the same structure.
#' @noRd
subset_onsides_dvobject <- function(dvobject, ingredient_ids) {
  if (length(ingredient_ids) == 0) return(dvobject[0])

  new_db <- list()

  # --- 1. Get the cascading set of keys ---
  message("Subsetting OnSIDES: Identifying all related keys...")
  # Find all products containing our target ingredients
  relevant_product_ids <- dvobject$vocab_rxnorm_ingredient_to_product %>%
    dplyr::filter(ingredient_id %in% ingredient_ids) %>%
    dplyr::pull(product_id) %>%
    unique()

  # Find all labels associated with those products
  relevant_label_ids <- dvobject$product_to_rxnorm %>%
    dplyr::filter(rxnorm_product_id %in% relevant_product_ids) %>%
    dplyr::pull(label_id) %>%
    unique()

  # --- 2. Filter the main data tables ---
  message("Filtering main OnSIDES data tables...")
  new_db$product_adverse_effect <- dvobject$product_adverse_effect %>%
    dplyr::filter(product_label_id %in% relevant_label_ids)

  if ("high_confidence" %in% names(dvobject)) {
    new_db$high_confidence <- dvobject$high_confidence %>%
      dplyr::filter(ingredient_id %in% ingredient_ids)
  }

  # --- 3. Filter the "bridge" and vocabulary tables to keep the subset lean ---
  message("Filtering vocabulary and mapping tables...")
  new_db$product_label <- dvobject$product_label %>%
    dplyr::filter(label_id %in% relevant_label_ids)
  new_db$product_to_rxnorm <- dvobject$product_to_rxnorm %>%
    dplyr::filter(label_id %in% relevant_label_ids)
  new_db$vocab_rxnorm_ingredient_to_product <- dvobject$vocab_rxnorm_ingredient_to_product %>%
    dplyr::filter(ingredient_id %in% ingredient_ids)

  # Find all MedDRA effects that are actually present in our subset
  relevant_meddra_ids <- new_db$product_adverse_effect$effect_meddra_id %>% unique()

  new_db$vocab_meddra_adverse_effect <- dvobject$vocab_meddra_adverse_effect %>%
    dplyr::filter(meddra_id %in% relevant_meddra_ids)
  new_db$vocab_rxnorm_ingredient <- dvobject$vocab_rxnorm_ingredient %>%
    dplyr::filter(rxnorm_id %in% ingredient_ids)
  new_db$vocab_rxnorm_product <- dvobject$vocab_rxnorm_product %>%
    dplyr::filter(rxnorm_id %in% relevant_product_ids)

  # Preserve attributes and return
  attributes(new_db) <- attributes(dvobject)
  return(new_db)
}
