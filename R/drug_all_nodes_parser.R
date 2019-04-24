#' Extracts the all drug elements and return data as list of dataframes.
#'
#' \code{parse_drug_all} returns list of dataframes of drugs elements.
#'
#' This functions extracts all element of drug nodes in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return all drug elements dataframes
#'
#' @examples
#' \donttest{
#' parse_drug_all()
#' parse_drug_all(TRUE)
#' parse_drug_all(save_table = FALSE)
#' }
#' @export
parse_drug_all <- function(save_table = FALSE) {
  Drugs <- parse_drug(save_table)
  print("Parsed Drugs main attributes, 1/74")
  Groups_Drug <- parse_drug_groups(save_table)
  print("Parsed Groups_Drug, 2/74")
  Articles_Drug <- parse_drug_articles(save_table)
  print("Parsed Articles_Drug, 3/74")
  Books_Drug <- parse_drug_books(save_table)
  print("Parsed Books_Drug, 4/74")
  Links_Drug <- parse_drug_links(save_table)
  print("Parsed Links_Drug, 5/74")
  Synonyms_Drug <- parse_drug_synonyms(save_table)
  print("Parsed Synonyms_Drug, 6/74")
  Products_Drug <- parse_drug_products(save_table)
  print("Parsed Products_Drug, 7/74")
  Mixtures_Drug <- parse_drug_mixtures(save_table)
  print("Parsed Mixtures_Drug, 8/74")
  Packagers_Drug <- parse_drug_packagers(save_table)
  print("Parsed Packagers_Drug, 9/74")
  Categories_Drug <- parse_drug_categories(save_table)
  print("Parsed Categories_Drug, 10/74")
  Affected_Organisms_Drug <-
    parse_drug_affected_organisms(save_table)
  print("Parsed Affected_Organisms_Drug, 11/74")
  Dosages_Drug <- parse_drug_dosages(save_table)
  print("Parsed Dosages_Drug, 12/74")
  AHFS_Codes_Drug <- parse_drug_ahfs_codes(save_table)
  print("Parsed AHFS_Codes_Drug, 13/74")
  PDB_Entries_Drug <- parse_drug_pdb_entries(save_table)
  print("Parsed PDB_Entries_Drug, 14/74")
  Patents_Drug <- parse_drug_patents(save_table)
  print("Parsed Patents_Drug, 15/74")
  Food_Interactions_Drug <- parse_drug_food_interactions(save_table)
  print("Parsed Food_Interactions_Drug, 16/74")
  Interactions_Drug <- parse_drug_interactions(save_table)
  print("Parsed Interactions_Drug, 17/74")
  Experimental_Properties_Drug <-
    parse_drug_experimental_properties(save_table)
  print("Parsed Experimental_Properties_Drug, 18/74")
  External_Identifiers_Drug <-
    parse_drug_experimental_properties(save_table)
  print("Parsed External_Identifiers_Drug, 19/74")
  External_Links_Drug <- parse_drug_external_links(save_table)
  print("Parsed External_Links_Drug, 20/74")
  SNP_Effects_Drug <- parse_drug_snp_effects(save_table)
  print("Parsed SNP_Effects_Drug, 21/74")
  SNP_Adverse_Drug_Reactions_Drug <-
    parse_drug_snp_adverse_drug_reactions(save_table)
  print("Parsed SNP_Adverse_Drug_Reactions_Drug, 22/74")
  ATC_Codes_Drug <- parse_drug_atc_codes(save_table)
  print("Parsed ATC_Codes_Drug, 23/74")
  Actions_Carrier_Drug <- parse_drug_carriers_actions(save_table)
  print("Parsed Actions_Carrier_Drug, 24/74")
  Articles_Carrier_Drug <- parse_drug_carriers_articles(save_table)
  print("Parsed Articles_Carrier_Drug, 25/74")
  Textbooks_Carrier_Drug <-
    parse_drug_carriers_textbooks(save_table)
  print("Parsed Textbooks_Carrier_Drug, 26/74")
  Links_Carrier_Drug <- parse_drug_carriers_links(save_table)
  print("Parsed Links_Carrier_Drug, 27/74")
  Polypeptides_Carrier_Drug <-
    parse_drug_carriers_polypeptides(save_table)
  print("Parsed Polypeptides_Carrier_Drug, 28/74")
  External_Identifiers_Polypeptide_Carrier_Drug <-
    parse_drug_carriers_polypeptides_external_identifiers(save_table)
  print("Parsed External_Identifiers_Polypeptide_Carrier_Drug, 29/74")
  Synonyms_Polypeptide_Carrier_Drug <-
    parse_drug_carriers_polypeptides_synonyms(save_table)
  print("Parsed Synonyms_Polypeptide_Carrier_Drug, 30/74")
  PFAMS_Polypeptide_Carrier_Drug <-
    parse_drug_carriers_polypeptides_pfams(save_table)
  print("Parsed PFAMS_Polypeptide_Carrier_Drug, 31/74")
  GO_Classifiers_Polypeptide_Carrier_Drug <-
    parse_drug_carriers_polypeptides_go_classifiers(save_table)
  print("Parsed GO_Classifiers_Polypeptide_Carrier_Drug, 32/74")
  Carriers_Drug <- parse_drug_carriers(save_table)
  print("Parsed Carriers_Drug, 33/74")
  Classifications_Drug <-
    parse_drug_classification(save_table)
  print("Parsed Classifications_Drug, 34/74")
  Actions_Enzyme_Drug <- parse_drug_enzymes_actions(save_table)
  print("Parsed Actions_Enzyme_Drug, 35/74")
  Articles_Enzyme_Drug <- parse_drug_enzymes_articles(save_table)
  print("Parsed Articles_Enzyme_Drug, 36/74")
  Textbooks_Enzyme_Drug <- parse_drug_enzymes_textbooks(save_table)
  print("Parsed Textbooks_Enzyme_Drug, 37/74")
  Links_Enzyme_Drug <- parse_drug_enzymes_links(save_table)
  print("Parsed Links_Enzyme_Drug, 38/74")
  Polypeptides_Enzyme_Drug <-
    parse_drug_enzymes_polypeptides(save_table)
  print("Parsed Polypeptides_Enzyme_Drug, 39/74")
  External_Identifiers_Polypeptide_Enzyme_Drug <-
    parse_drug_enzymes_polypeptides_external_identifiers(save_table)
  print("Parsed External_Identifiers_Polypeptides_Enzyme_Drug, 40/74")
  Synonyms_Polypeptides_Enzyme_Drug <-
    parse_drug_enzymes_polypeptides_synonyms(save_table)
  print("Parsed Synonyms_Polypeptides_Enzyme_Drug, 41/74")
  PFAMS_Polypeptides_Enzyme_Drug <-
    parse_drug_enzymes_polypeptides_pfams(save_table)
  print("Parsed PFAMS_Polypeptides_Enzyme_Drug, 42/74")
  GO_Classifiers_Polypeptides_Enzyme_Drug <-
    parse_drug_enzymes_polypeptides_go_classifiers(save_table)
  print("Parsed GO_Classifiers_Polypeptides_Enzyme_Drug, 43/74")
  Enzymes_Drug <- parse_drug_enzymes(save_table)
  print("Parsed Enzyme_Drug, 44/74")
  Manufacturers_Drug <- parse_drug_manufacturers(save_table)
  print("Parsed Manufacturers_Drug, 45/74")
  Enzymes_Pathway_Drug <- parse_drug_pathway_enzyme(save_table)
  print("Parsed Enzymes_Pathway_Drug, 46/74")
  Drugs_Pathway_Drug <- parse_drug_pathway_drugs(save_table)
  print("Parsed drug_pathway_drugs, 47/74")
  Pathways_Drug <- parse_drug_pathway(save_table)
  print("Parsed Pathways_Drug, 48/74")
  Prices_Drug <- parse_drug_prices(save_table)
  print("Parsed Prices_Drug, 49/74")
  Reactions_Drug <- parse_drug_reactions(save_table)
  print("Parsed Reactions_Drug, 50/74")
  Enzymes_Reactions_Drug <- parse_drug_reactions_enzymes(save_table)
  print("Parsed Enzymes_Reactions_Drug, 51/74")
  Sequences_Drug <- parse_drug_sequences(save_table)
  print("Parsed Sequences_Drug, 52/74")
  External_Identifiers_Polypeptide_Target_Drug <-
    parse_drug_targets_polypeptides_external_identifiers(save_table)
  print("Parsed External_Identifiers_Polypeptide_Target_Drug, 53/74")
  Synonyms_Polypeptide_Target_Drug <-
    parse_drug_targets_polypeptides_synonyms(save_table)
  print("Parsed Synonyms_Polypeptide_Target_Drug, 54/74")
  PFAMS_Polypeptide_Target_Drug <-
    parse_drug_targets_polypeptides_pfams(save_table)
  print("Parsed PFAMS_Polypeptide_Target_Drug, 55/74")
  GO_Classifiers_Polypeptide_Target_Drug <-
    parse_drug_targets_polypeptides_go_classifiers(save_table)
  print("Parsed GO_Classifiers_Polypeptide_Target_Drug attributes, 56/74")
  Actions_Target_Drug <- parse_drug_targets_actions(save_table)
  print("Parsed Actions_Target_Drug, 57/74")
  Articles_Target_Drug <- parse_drug_targets_articles(save_table)
  print("Parsed Articles_Target_Drug, 58/74")
  Textbooks_Target_Drug <- parse_drug_targets_textbooks(save_table)
  print("Parsed Textbooks_Target_Drug, 59/74")
  Links_Target_Drug <- parse_drug_targets_links(save_table)
  print("Parsed Links_Target_Drug, 60/74")
  Polypeptide_Target_Drug <-
    parse_drug_targets_polypeptides(save_table)
  print("Parsed Polypeptide_Target_Drug, 61/74")
  Targets_Drug <- parse_drug_targets(save_table)
  print("Parsed Targets_Drug, 62/74")
  Actions_Transporter_Drug <-
    parse_drug_transporters_actions(save_table)
  print("Parsed Actions_Transporter_Drug, 63/74")
  Articles_Transporter_Drug <-
    parse_drug_transporters_articles(save_table)
  print("Parsed Articles_Transporter_Drug, 64/74")
  Textbooks_Transporter_Drug <-
    parse_drug_transporters_textbooks(save_table)
  print("Parsed Textbooks_Transporter_Drug, 65/74")
  Links_Transporter_Drug <-
    parse_drug_transporters_links(save_table)
  print("Parsed Links_Transporter_Drug, 66/74")
  Polypeptides_Transporter_Drug <-
    parse_drug_transporters_polypeptides(save_table)
  print("Parsed Polypeptides_Transporter_Drug, 67/74")
  External_Identifiers_Transporter_Drug <-
    parse_drug_transporters_polypeptides_external_identifiers(save_table)
  print("Parsed External_Identifiers_Transporter_Drug, 68/74")
  Synonyms_Polypeptide_Transporter_Drug <-
    parse_drug_transporters_polypeptides_synonyms(save_table)
  print("Parsed Synonyms_Polypeptides_Transporter_Drug, 69/74")
  PFAMS_Polypeptid_Transporter_Drug <-
    parse_drug_transporters_polypeptides_pfams(save_table)
  print("Parsed PFAMS_Polypeptides_Transporter_Drug, 70/74")
  GO_Classifiers_Polypeptide_Transporter_Drug <-
    parse_drug_transporters_polypeptides_go_classifiers(save_table)
  print("Parsed GO_Classifiers_Polypeptides_Transporter_Drug, 71/74")
  Transporters_Drug <- parse_drug_transporters(save_table)
  print("Parsed Transporters_Drug, 72/74")
  International_Brands_Drug <- parse_drug_international_brands(save_table)
  print("Parsed International_Brands_Drug, 73/74")
  Salts_Drug <- parse_drug_salts(save_table)
  print("Parsed Salts_Drug, 74/74")
  return(
    list(
      Drugs = Drugs,
      Groups_Drug = Groups_Drug,
      Articles_Drug = Articles_Drug,
      Books_Drug = Books_Drug,
      Links_Drug = Links_Drug,
      Synonyms_Drug = Synonyms_Drug,
      Products_Drug = Products_Drug,
      Mixtures_Drug = Mixtures_Drug,
      Packagers_Drug = Packagers_Drug,
      Categories_Drug = Categories_Drug,
      Affected_Organisms_Drug = Affected_Organisms_Drug,
      Dosages_Drug = Dosages_Drug,
      AHFS_Codes_Drug = AHFS_Codes_Drug,
      PDB_Entries_Drug = PDB_Entries_Drug,
      Patents_Drug = Patents_Drug,
      Food_Interactions_Drug = Food_Interactions_Drug,
      Interactions_Drug = Interactions_Drug,
      Experimental_Properties_Drug = Experimental_Properties_Drug,
      External_Identifiers_Drug = External_Identifiers_Drug,
      External_Links_Drug = External_Links_Drug,
      SNP_Effects_Drug = SNP_Effects_Drug,
      SNP_Adverse_Drug_Reactions_Drug = SNP_Adverse_Drug_Reactions_Drug,
      ATC_Codes_Drug = ATC_Codes_Drug,
      Actions_Carrier_Drug = Actions_Carrier_Drug,
      Articles_Carrier_Drug = Articles_Carrier_Drug,
      Textbooks_Carrier_Drug = Textbooks_Carrier_Drug,
      Links_Carrier_Drug = Links_Carrier_Drug,
      Polypeptides_Carrier_Drug = Polypeptides_Carrier_Drug,
      External_Identifiers_Polypeptide_Carrier_Drug =
        External_Identifiers_Polypeptide_Carrier_Drug,
      Synonyms_Polypeptide_Carrier_Drug = Synonyms_Polypeptide_Carrier_Drug,
      PFAMS_Polypeptide_Carrier_Drug = PFAMS_Polypeptide_Carrier_Drug,
      GO_Classifiers_Polypeptide_Carrier_Drug =
        GO_Classifiers_Polypeptide_Carrier_Drug,
      Carriers_Drug = Carriers_Drug,
      Classifications_Drug =
      Classifications_Drug,
      Actions_Enzyme_Drug = Actions_Enzyme_Drug,
      Articles_Enzyme_Drug = Articles_Enzyme_Drug,
      Textbooks_Enzyme_Drug = Textbooks_Enzyme_Drug,
      Links_Enzyme_Drug = Links_Enzyme_Drug,
      Polypeptides_Enzyme_Drug = Polypeptides_Enzyme_Drug,
      External_Identifiers_Polypeptide_Enzyme_Drug =
        External_Identifiers_Polypeptide_Enzyme_Drug,
      Synonyms_Polypeptides_Enzyme_Drug = Synonyms_Polypeptides_Enzyme_Drug,
      PFAMS_Polypeptides_Enzyme_Drug = PFAMS_Polypeptides_Enzyme_Drug,
      GO_Classifiers_Polypeptides_Enzyme_Drug = GO_Classifiers_Polypeptides_Enzyme_Drug,
      Enzymes_Drug = Enzymes_Drug,
      Manufacturers_Drug = Manufacturers_Drug,
      Enzymes_Pathway_Drug = Enzymes_Pathway_Drug,
      Drugs_Pathway_Drug = Drugs_Pathway_Drug,
      Pathways_Drug = Pathways_Drug,
      Prices_Drug = Prices_Drug,
      Reactions_Drug = Reactions_Drug,
      Enzymes_Reactions_Drug = Enzymes_Reactions_Drug,
      Sequences_Drug = Sequences_Drug,
      External_Identifiers_Polypeptide_Target_Drug =
        External_Identifiers_Polypeptide_Target_Drug,
      Synonyms_Polypeptide_Target_Drug = Synonyms_Polypeptide_Target_Drug,
      PFAMS_Polypeptide_Target_Drug = PFAMS_Polypeptide_Target_Drug,
      GO_Classifiers_Polypeptide_Target_Drug =
        GO_Classifiers_Polypeptide_Target_Drug,
      Actions_Target_Drug = Actions_Target_Drug,
      Articles_Target_Drug = Articles_Target_Drug,
      Textbooks_Target_Drug = Textbooks_Target_Drug,
      Links_Target_Drug = Links_Target_Drug,
      Polypeptide_Target_Drug = Polypeptide_Target_Drug,
      Targets_Drug = Targets_Drug,
      Actions_Transporter_Drug = Actions_Transporter_Drug,
      Articles_Transporter_Drug = Articles_Transporter_Drug,
      Textbooks_Transporter_Drug = Textbooks_Transporter_Drug,
      Links_Transporter_Drug = Links_Transporter_Drug,
      Polypeptides_Transporter_Drug = Polypeptides_Transporter_Drug,
      External_Identifiers_Transporter_Drug =
        External_Identifiers_Transporter_Drug,
      Synonyms_Polypeptide_Transporter_Drug =
        Synonyms_Polypeptide_Transporter_Drug,
      PFAMS_Polypeptid_Transporter_Drug = PFAMS_Polypeptid_Transporter_Drug,
      GO_Classifiers_Polypeptide_Transporter_Drug =
        GO_Classifiers_Polypeptide_Transporter_Drug,
      Transporters_Drug = Transporters_Drug,
      International_Brands_Drug = International_Brands_Drug,
      Salts_Drug = Salts_Drug
    )
  )
}

#' Extracts the given drug elements and return data as list of dataframes.
#'
#' \code{parse_drug_element} returns list of dataframes of drugs selected elements.
#'
#' This functions extracts selected element of drug nodes in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' parse_drug_element_options can be called to know the valid options for
#' this method
#'
#' @param save_table boolean, save table in database if true. Default is false.
#' @param elements_options list,  options of elements to be parsed. Default is "all"
#' @return list of selected drug elements dataframes
#'
#' @examples
#' \donttest{
#' parse_drug_element()
#' parse_drug_element(c("drug_ahfs_codes", "drug_carriers"), save_table = TRUE)
#' parse_drug_element(save_table = FALSE)
#' parse_drug_element(c("drug_ahfs_codes", "drug_carriers"))
#' }
#' @export
parse_drug_element <- function(elements_options = c("all"), save_table = FALSE) {
  if (!all(elements_options %in% parse_drug_element_options())) {
    stop("Invalid options\nplease use parse_drug_element_options() to know valid options")
  }

  if ("all" %in% elements_options) {
    return(parse_drug_all(save_table = save_table))
  }
  parsed_list <- list()
  for (option in elements_options) {
    parsed_element <- switch (
      option,
      "Drugs" = parse_drug(save_table),
      "Affected_Organisms_Drug" = parse_drug_affected_organisms(save_table),
      "AHFS_Codes_Drug" = parse_drug_ahfs_codes(save_table),
      "Articles_Drug" = parse_drug_articles(save_table),
      "ATC_Codes_Drug" = parse_drug_atc_codes(save_table),
      "Books_Drug" = parse_drug_books(save_table),
      "Carriers_Drug" = parse_drug_carriers(save_table),
      "Actions_Carrier_Drug" = parse_drug_carriers_actions(save_table),
      "Articles_Carrier_Drug" = parse_drug_carriers_articles(save_table),
      "Links_Carrier_Drug" = parse_drug_carriers_links(save_table),
      "Polypeptides_Carrier_Drugs" = parse_drug_carriers_polypeptides(save_table),
      "External_Identifiers_Polypeptide_Carrier_Drug" =
        parse_drug_carriers_polypeptides_external_identifiers(save_table),
      "GO_Classifiers_Polypeptide_Carrier_Drug" =
        parse_drug_carriers_polypeptides_go_classifiers(save_table),
      "PFAMS_Polypeptide_Carrier_Drug" = parse_drug_carriers_polypeptides_pfams(save_table),
      "Synonyms_Polypeptide_Carrier_Drug" = parse_drug_carriers_polypeptides_synonyms(save_table),
      "Textbooks_Carrier_Drug" = parse_drug_carriers_textbooks(save_table),
      "Categories_Drug" = parse_drug_carriers(save_table),
      "Classifications_Drug" = parse_drug_classification(save_table),
      "Dosages_Drug" = parse_drug_dosages(save_table),
      "Enzymes_Drug" = parse_drug_enzymes(save_table),
      "Actions_Enzyme_Drug" = parse_drug_enzymes_actions(save_table),
      "Articles_Enzyme_Drug" = parse_drug_enzymes_articles(save_table),
      "Links_Enzyme_Drug" = parse_drug_enzymes_links(save_table),
      "Polypeptides_Enzyme_Drug" = parse_drug_enzymes_polypeptides(save_table),
      "External_Identifiers_Polypeptide_Enzyme_Drug" =
        parse_drug_enzymes_polypeptides_external_identifiers(save_table),
      "GO_Classifiers_Polypeptides_Enzyme_Drug" =
        parse_drug_enzymes_polypeptides_go_classifiers(save_table),
      "PFAMS_Polypeptides_Enzyme_Drug" = parse_drug_enzymes_polypeptides_pfams(save_table),
      "Synonyms_Polypeptides_Enzyme_Drug" = parse_drug_enzymes_polypeptides_synonyms(save_table),
      "Textbooks_Enzyme_Drug" = parse_drug_enzymes_textbooks(save_table),
      "Experimental_Properties_Drug" = parse_drug_experimental_properties(save_table),
      "External_Identifiers_Drug" = parse_drug_external_identifiers(save_table),
      "External_Links_Drug" = parse_drug_external_links(save_table),
      "Food_Interactions_Drug" = parse_drug_food_interactions(save_table),
      "Groups_Drugs" = parse_drug_groups(save_table),
      "Interactions_Drug" = parse_drug_interactions(save_table),
      "Links_Drug" = parse_drug_interactions(save_table),
      "Manufacturers_Drug" = parse_drug_manufacturers(save_table),
      "Mixtures_Drug" = parse_drug_mixtures(save_table),
      "Packagers_Drug" = parse_drug_packagers(save_table),
      "Patents_Drugs" = parse_drug_patents(save_table),
      "Pathways_Drug" = parse_drug_pathway(save_table),
      "Drugs_Pathway_Drug" = parse_drug_pathway_drugs(save_table),
      "Enzymes_Pathway_Drug" = parse_drug_pathway_enzyme(save_table),
      "PDB_Entries_Drug" = parse_drug_pdb_entries(save_table),
      "Prices_Drug" = parse_drug_prices(save_table),
      "Products_Drug" = parse_drug_products(save_table),
      "Reactions_Drugs" = parse_drug_reactions(save_table),
      "Enzymes_Reactions_Drug" = parse_drug_reactions_enzymes(save_table),
      "Sequences_Drug" = parse_drug_sequences(save_table),
      "SNP_Adverse_Drug_Reactions_Drug" = parse_drug_snp_adverse_drug_reactions(save_table),
      "SNP_Effects_Drug" = parse_drug_snp_effects(save_table),
      "Synonyms_Drug" = parse_drug_synonyms(save_table),
      "Targets_Drug" = parse_drug_targets(save_table),
      "Actions_Target_Drug" = parse_drug_targets_actions(save_table),
      "Articles_Target_Drug" = parse_drug_targets_articles(save_table),
      "Links_Target_Drug" = parse_drug_targets_links(save_table),
      "Polypeptide_Target_Drug" = parse_drug_targets_polypeptides(save_table),
      "External_Identifiers_Polypeptide_Target_Drug" =
        parse_drug_targets_polypeptides_external_identifiers(save_table),
      "GO_Classifiers_Polypeptide_Target_Drug" =
        parse_drug_targets_polypeptides_go_classifiers(save_table),
      "PFAMS_Polypeptide_Target_Drug" = parse_drug_targets_polypeptides_pfams(save_table),
      "Synonyms_Polypeptide_Target_Drug" = parse_drug_targets_polypeptides_synonyms(save_table),
      "Textbooks_Target_Drug" = parse_drug_targets_textbooks(save_table),
      "Transporters_Drug" = parse_drug_transporters(save_table),
      "Actions_Transporter_Drug" = parse_drug_transporters_actions(save_table),
      "Articles_Transporter_Drug" = parse_drug_targets_articles(save_table),
      "Links_Transporter_Drug" = parse_drug_transporters_links(save_table),
      "Polypeptides_Transporter_Drug" = parse_drug_enzymes_polypeptides(save_table),
      "External_Identifiers_Transporter_Drug" =
        parse_drug_transporters_polypeptides_external_identifiers(save_table),
      "GO_Classifiers_Polypeptide_Transporters_Drug" =
        parse_drug_transporters_polypeptides_go_classifiers(save_table),
      "PFAMS_Polypeptid_Transporter_Drug" = parse_drug_transporters_polypeptides_pfams(save_table),
      "Synonyms_Polypeptide_Transporter_Drug" = parse_drug_transporters_polypeptides_synonyms(save_table),
      "Textbooks_Transporter_Drug" = parse_drug_transporters_textbooks(save_table),
      "International_Brands_Drug" = parse_drug_international_brands(save_table),
      "Salts_Drug" = parse_drug_salts(save_table)
    )
    parsed_list[[option]] <- parsed_element
    print(paste("Parsed", option))
  }
  return(parsed_list)
}

#' Returns \code{parse_drug_element} valid options.
#'
#' @return list of \code{parse_drug_element} valid options
#'
#' @examples
#' \donttest{
#' parse_drug_element_options()
#' }
#' @export
parse_drug_element_options <- function() {
  elements_options <-
    c(
      "all",
      "Drugs",
      "Groups_Drug",
      "Articles_Drug",
      "Books_Drug",
      "Links_Drug",
      "Synonyms_Drug",
      "Products_Drug",
      "Mixtures_Drug",
      "Packagers_Drug",
      "Categories_Drug",
      "Affected_Organisms_Drug",
      "Dosages_Drug",
      "AHFS_Codes_Drug",
      "PDB_Entries_Drug",
      "Patents_Drug",
      "Food_Interactions_Drug",
      "Interactions_Drug",
      "Experimental_Properties_Drug",
      "External_Identifiers_Drug",
      "External_Links_Drug",
      "SNP_Effects_Drug",
      "SNP_Adverse_Drug_Reactions_Drug",
      "ATC_Codes_Drug",
      "Actions_Carrier_Drug",
      "Articles_Carrier_Drug",
      "Textbooks_Carrier_Drug",
      "Links_Carrier_Drug",
      "Polypeptides_Carrier_Drug",
      "External_Identifiers_Polypeptide_Carrier_Drug",
      "Synonyms_Polypeptide_Carrier_Drug",
      "PFAMS_Polypeptide_Carrier_Drug",
      "GO_Classifiers_Polypeptide_Carrier_Drug",
      "Carriers_Drug",
      "Classifications_Drug",
      "Actions_Enzyme_Drug",
      "Articles_Enzyme_Drug",
      "Textbooks_Enzyme_Drug",
      "Links_Enzyme_Drug",
      "Polypeptides_Enzyme_Drug",
      "External_Identifiers_Polypeptide_Enzyme_Drug",
      "Synonyms_Polypeptides_Enzyme_Drug",
      "PFAMS_Polypeptides_Enzyme_Drug",
      "GO_Classifiers_Polypeptides_Enzyme_Drug",
      "Enzymes_Drug",
      "Manufacturers_Drug",
      "Enzymes_Pathway_Drug",
      "Drugs_Pathway_Drug",
      "Pathways_Drug",
      "Prices_Drug",
      "Reactions_Drug",
      "Enzymes_Reactions_Drug",
      "Sequences_Drug",
      "External_Identifiers_Polypeptide_Target_Drug",
      "Synonyms_Polypeptide_Target_Drug",
      "PFAMS_Polypeptide_Target_Drug",
      "GO_Classifiers_Polypeptide_Target_Drug",
      "Actions_Target_Drug",
      "Articles_Target_Drug",
      "Textbooks_Target_Drug",
      "Links_Target_Drug",
      "Polypeptide_Target_Drug",
      "Targets_Drug",
      "Actions_Transporter_Drug",
      "Articles_Transporter_Drug",
      "Textbooks_Transporter_Drug",
      "Links_Transporter_Drug",
      "Polypeptides_Transporter_Drug",
      "External_Identifiers_Transporter_Drug",
      "Synonyms_Polypeptide_Transporter_Drug",
      "PFAMS_Polypeptid_Transporter_Drug",
      "GO_Classifiers_Polypeptide_Transporters_Drug",
      "Transporters_Drug",
      "International_Brands_Drug",
      "Salts_Drug"
    )
  return(elements_options)
}
