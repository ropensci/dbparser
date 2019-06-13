#' Extracts the all drug elements and return data as list of dataframes.
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
#' @param save_csv boolean, save csv version of parsed dataframe if true
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return all drug elements dataframes
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_all()
#'
#' # save in database and return parsed dataframe
#' parse_drug_all(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location,
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_all(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv,
#' # if it does not exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_all(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location,
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_all(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_all(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_all <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    Drugs <- parse_drug(save_table, save_csv, csv_path, override_csv)
    print("Parsed Drugs main attributes, 1/75")
    Groups_Drug <-
      parse_drug_groups(save_table, save_csv, csv_path, override_csv)
    print("Parsed Groups_Drug, 2/75")
    Articles_Drug <-
      parse_drug_articles(save_table, save_csv, csv_path, override_csv)
    print("Parsed Articles_Drug, 3/75")
    Books_Drug <-
      parse_drug_books(save_table, save_csv, csv_path, override_csv)
    print("Parsed Books_Drug, 4/75")
    Links_Drug <-
      parse_drug_links(save_table, save_csv, csv_path, override_csv)
    print("Parsed Links_Drug, 5/75")
    Synonyms_Drug <-
      parse_drug_synonyms(save_table, save_csv, csv_path, override_csv)
    print("Parsed Synonyms_Drug, 6/75")
    Products_Drug <-
      parse_drug_products(save_table, save_csv, csv_path, override_csv)
    print("Parsed Products_Drug, 7/75")
    Mixtures_Drug <-
      parse_drug_mixtures(save_table, save_csv, csv_path, override_csv)
    print("Parsed Mixtures_Drug, 8/75")
    Packagers_Drug <-
      parse_drug_packagers(save_table, save_csv, csv_path, override_csv)
    print("Parsed Packagers_Drug, 9/75")
    Categories_Drug <-
      parse_drug_categories(save_table, save_csv, csv_path, override_csv)
    print("Parsed Categories_Drug, 10/75")
    Affected_Organisms_Drug <-
      parse_drug_affected_organisms(save_table, save_csv, csv_path, override_csv)
    print("Parsed Affected_Organisms_Drug, 11/75")
    Dosages_Drug <-
      parse_drug_dosages(save_table, save_csv, csv_path, override_csv)
    print("Parsed Dosages_Drug, 12/75")
    AHFS_Codes_Drug <-
      parse_drug_ahfs_codes(save_table, save_csv, csv_path, override_csv)
    print("Parsed AHFS_Codes_Drug, 13/75")
    PDB_Entries_Drug <-
      parse_drug_pdb_entries(save_table, save_csv, csv_path, override_csv)
    print("Parsed PDB_Entries_Drug, 14/75")
    Patents_Drug <-
      parse_drug_patents(save_table, save_csv, csv_path, override_csv)
    print("Parsed Patents_Drug, 15/75")
    Food_Interactions_Drug <-
      parse_drug_food_interactions(save_table, save_csv, csv_path, override_csv)
    print("Parsed Food_Interactions_Drug, 16/75")
    Interactions_Drug <-
      parse_drug_interactions(save_table, save_csv, csv_path, override_csv)
    print("Parsed Interactions_Drug, 17/75")
    Experimental_Properties_Drug <-
      parse_drug_experimental_properties(save_table, save_csv, csv_path, override_csv)
    print("Parsed Experimental_Properties_Drug, 18/75")
    External_Identifiers_Drug <-
      parse_drug_experimental_properties(save_table, save_csv, csv_path, override_csv)
    print("Parsed External_Identifiers_Drug, 19/75")
    External_Links_Drug <-
      parse_drug_external_links(save_table, save_csv, csv_path, override_csv)
    print("Parsed External_Links_Drug, 20/75")
    SNP_Effects_Drug <-
      parse_drug_snp_effects(save_table, save_csv, csv_path, override_csv)
    print("Parsed SNP_Effects_Drug, 21/75")
    SNP_Adverse_Drug_Reactions_Drug <-
      parse_drug_snp_adverse_drug_reactions(save_table, save_csv, csv_path, override_csv)
    print("Parsed SNP_Adverse_Drug_Reactions_Drug, 22/75")
    ATC_Codes_Drug <-
      parse_drug_atc_codes(save_table, save_csv, csv_path, override_csv)
    print("Parsed ATC_Codes_Drug, 23/75")
    Actions_Carrier_Drug <-
      parse_drug_carriers_actions(save_table, save_csv, csv_path, override_csv)
    print("Parsed Actions_Carrier_Drug, 24/75")
    Articles_Carrier_Drug <-
      parse_drug_carriers_articles(save_table, save_csv, csv_path, override_csv)
    print("Parsed Articles_Carrier_Drug, 25/75")
    Textbooks_Carrier_Drug <-
      parse_drug_carriers_textbooks(save_table, save_csv, csv_path, override_csv)
    print("Parsed Textbooks_Carrier_Drug, 26/75")
    Links_Carrier_Drug <-
      parse_drug_carriers_links(save_table, save_csv, csv_path, override_csv)
    print("Parsed Links_Carrier_Drug, 27/75")
    Polypeptides_Carrier_Drug <-
      parse_drug_carriers_polypeptides(save_table, save_csv, csv_path, override_csv)
    print("Parsed Polypeptides_Carrier_Drug, 28/75")
    External_Identifiers_Polypeptide_Carrier_Drug <-
      parse_drug_carriers_polypeptides_external_identifiers(save_table, save_csv, csv_path, override_csv)
    print("Parsed External_Identifiers_Polypeptide_Carrier_Drug, 29/75")
    Synonyms_Polypeptide_Carrier_Drug <-
      parse_drug_carriers_polypeptides_synonyms(save_table, save_csv, csv_path, override_csv)
    print("Parsed Synonyms_Polypeptide_Carrier_Drug, 30/75")
    PFAMS_Polypeptide_Carrier_Drug <-
      parse_drug_carriers_polypeptides_pfams(save_table, save_csv, csv_path, override_csv)
    print("Parsed PFAMS_Polypeptide_Carrier_Drug, 31/75")
    GO_Classifiers_Polypeptide_Carrier_Drug <-
      parse_drug_carriers_polypeptides_go_classifiers(save_table, save_csv, csv_path, override_csv)
    print("Parsed GO_Classifiers_Polypeptide_Carrier_Drug, 32/75")
    Carriers_Drug <-
      parse_drug_carriers(save_table, save_csv, csv_path, override_csv)
    print("Parsed Carriers_Drug, 33/75")
    Classifications_Drug <-
      parse_drug_classification(save_table, save_csv, csv_path, override_csv)
    print("Parsed Classifications_Drug, 34/75")
    Actions_Enzyme_Drug <-
      parse_drug_enzymes_actions(save_table, save_csv, csv_path, override_csv)
    print("Parsed Actions_Enzyme_Drug, 35/75")
    Articles_Enzyme_Drug <-
      parse_drug_enzymes_articles(save_table, save_csv, csv_path, override_csv)
    print("Parsed Articles_Enzyme_Drug, 36/75")
    Textbooks_Enzyme_Drug <-
      parse_drug_enzymes_textbooks(save_table, save_csv, csv_path, override_csv)
    print("Parsed Textbooks_Enzyme_Drug, 37/75")
    Links_Enzyme_Drug <-
      parse_drug_enzymes_links(save_table, save_csv, csv_path, override_csv)
    print("Parsed Links_Enzyme_Drug, 38/75")
    Polypeptides_Enzyme_Drug <-
      parse_drug_enzymes_polypeptides(save_table, save_csv, csv_path, override_csv)
    print("Parsed Polypeptides_Enzyme_Drug, 39/75")
    External_Identifiers_Polypeptide_Enzyme_Drug <-
      parse_drug_enzymes_polypeptides_external_identifiers(save_table, save_csv, csv_path, override_csv)
    print("Parsed External_Identifiers_Polypeptides_Enzyme_Drug, 40/75")
    Synonyms_Polypeptides_Enzyme_Drug <-
      parse_drug_enzymes_polypeptides_synonyms(save_table, save_csv, csv_path, override_csv)
    print("Parsed Synonyms_Polypeptides_Enzyme_Drug, 41/75")
    PFAMS_Polypeptides_Enzyme_Drug <-
      parse_drug_enzymes_polypeptides_pfams(save_table, save_csv, csv_path, override_csv)
    print("Parsed PFAMS_Polypeptides_Enzyme_Drug, 42/75")
    GO_Classifiers_Polypeptides_Enzyme_Drug <-
      parse_drug_enzymes_polypeptides_go_classifiers(save_table, save_csv, csv_path, override_csv)
    print("Parsed GO_Classifiers_Polypeptides_Enzyme_Drug, 43/75")
    Enzymes_Drug <-
      parse_drug_enzymes(save_table, save_csv, csv_path, override_csv)
    print("Parsed Enzyme_Drug, 44/75")
    Manufacturers_Drug <-
      parse_drug_manufacturers(save_table, save_csv, csv_path, override_csv)
    print("Parsed Manufacturers_Drug, 45/75")
    Enzymes_Pathway_Drug <-
      parse_drug_pathway_enzyme(save_table, save_csv, csv_path, override_csv)
    print("Parsed Enzymes_Pathway_Drug, 46/75")
    Drugs_Pathway_Drug <-
      parse_drug_pathway_drugs(save_table, save_csv, csv_path, override_csv)
    print("Parsed drug_pathway_drugs, 47/75")
    Pathways_Drug <-
      parse_drug_pathway(save_table, save_csv, csv_path, override_csv)
    print("Parsed Pathways_Drug, 48/75")
    Prices_Drug <-
      parse_drug_prices(save_table, save_csv, csv_path, override_csv)
    print("Parsed Prices_Drug, 49/75")
    Reactions_Drug <-
      parse_drug_reactions(save_table, save_csv, csv_path, override_csv)
    print("Parsed Reactions_Drug, 50/75")
    Enzymes_Reactions_Drug <-
      parse_drug_reactions_enzymes(save_table, save_csv, csv_path, override_csv)
    print("Parsed Enzymes_Reactions_Drug, 51/75")
    Sequences_Drug <-
      parse_drug_sequences(save_table, save_csv, csv_path, override_csv)
    print("Parsed Sequences_Drug, 52/75")
    External_Identifiers_Polypeptide_Target_Drug <-
      parse_drug_targets_polypeptides_external_identifiers(save_table, save_csv, csv_path, override_csv)
    print("Parsed External_Identifiers_Polypeptide_Target_Drug, 53/75")
    Synonyms_Polypeptide_Target_Drug <-
      parse_drug_targets_polypeptides_synonyms(save_table, save_csv, csv_path, override_csv)
    print("Parsed Synonyms_Polypeptide_Target_Drug, 54/75")
    PFAMS_Polypeptide_Target_Drug <-
      parse_drug_targets_polypeptides_pfams(save_table, save_csv, csv_path, override_csv)
    print("Parsed PFAMS_Polypeptide_Target_Drug, 55/75")
    GO_Classifiers_Polypeptide_Target_Drug <-
      parse_drug_targets_polypeptides_go_classifiers(save_table, save_csv, csv_path, override_csv)
    print("Parsed GO_Classifiers_Polypeptide_Target_Drug attributes, 56/75")
    Actions_Target_Drug <-
      parse_drug_targets_actions(save_table, save_csv, csv_path, override_csv)
    print("Parsed Actions_Target_Drug, 57/75")
    Articles_Target_Drug <-
      parse_drug_targets_articles(save_table, save_csv, csv_path, override_csv)
    print("Parsed Articles_Target_Drug, 58/75")
    Textbooks_Target_Drug <-
      parse_drug_targets_textbooks(save_table, save_csv, csv_path, override_csv)
    print("Parsed Textbooks_Target_Drug, 59/75")
    Links_Target_Drug <-
      parse_drug_targets_links(save_table, save_csv, csv_path, override_csv)
    print("Parsed Links_Target_Drug, 60/75")
    Polypeptide_Target_Drug <-
      parse_drug_targets_polypeptides(save_table, save_csv, csv_path, override_csv)
    print("Parsed Polypeptide_Target_Drug, 61/75")
    Targets_Drug <-
      parse_drug_targets(save_table, save_csv, csv_path, override_csv)
    print("Parsed Targets_Drug, 62/75")
    Actions_Transporter_Drug <-
      parse_drug_transporters_actions(save_table, save_csv, csv_path, override_csv)
    print("Parsed Actions_Transporter_Drug, 63/75")
    Articles_Transporter_Drug <-
      parse_drug_transporters_articles(save_table, save_csv, csv_path, override_csv)
    print("Parsed Articles_Transporter_Drug, 64/75")
    Textbooks_Transporter_Drug <-
      parse_drug_transporters_textbooks(save_table, save_csv, csv_path, override_csv)
    print("Parsed Textbooks_Transporter_Drug, 65/75")
    Links_Transporter_Drug <-
      parse_drug_transporters_links(save_table, save_csv, csv_path, override_csv)
    print("Parsed Links_Transporter_Drug, 66/75")
    Polypeptides_Transporter_Drug <-
      parse_drug_transporters_polypeptides(save_table, save_csv, csv_path, override_csv)
    print("Parsed Polypeptides_Transporter_Drug, 67/75")
    External_Identifiers_Transporter_Drug <-
      parse_drug_transporters_polypeptides_external_identifiers(save_table, save_csv, csv_path, override_csv)
    print("Parsed External_Identifiers_Transporter_Drug, 68/75")
    Synonyms_Polypeptide_Transporter_Drug <-
      parse_drug_transporters_polypeptides_synonyms(save_table, save_csv, csv_path, override_csv)
    print("Parsed Synonyms_Polypeptides_Transporter_Drug, 69/75")
    PFAMS_Polypeptid_Transporter_Drug <-
      parse_drug_transporters_polypeptides_pfams(save_table, save_csv, csv_path, override_csv)
    print("Parsed PFAMS_Polypeptides_Transporter_Drug, 70/75")
    GO_Classifiers_Polypeptide_Transporter_Drug <-
      parse_drug_transporters_polypeptides_go_classifiers(save_table, save_csv, csv_path, override_csv)
    print("Parsed GO_Classifiers_Polypeptides_Transporter_Drug, 71/75")
    Transporters_Drug <-
      parse_drug_transporters(save_table, save_csv, csv_path, override_csv)
    print("Parsed Transporters_Drug, 72/75")
    International_Brands_Drug <-
      parse_drug_international_brands(save_table, save_csv, csv_path, override_csv)
    print("Parsed International_Brands_Drug, 73/75")
    Salts_Drug <-
      parse_drug_salts(save_table, save_csv, csv_path, override_csv)
    print("Parsed Salts_Drug, 74/75")
    Calculated_Properties_Drug <-
      parse_drug_calculated_properties(save_table, save_csv, csv_path, override_csv)
    print("Parsed Calculated_Properties_Drug, 75/75")
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
        Salts_Drug = Salts_Drug,
        Culculated_Properties_Drug = Calculated_Properties_Drug
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
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed dataframe if true
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @param elements_options list,  options of elements to be parsed. Default is "all"
#' @return list of selected drug elements dataframes
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_element()
#'
#' # save in database and return parsed dataframe
#' parse_drug_element(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_element(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_element(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_element(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_element(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' parse_drug_element(c("drug_ahfs_codes", "drug_carriers"), save_table = TRUE)
#' parse_drug_element(save_table = FALSE)
#' parse_drug_element(c("drug_ahfs_codes", "drug_carriers"))
#' }
#' @export
parse_drug_element <-
  function(elements_options = c("all"),
           save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
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
        "Drugs" = parse_drug(save_table, save_csv, csv_path, override_csv),
        "Affected_Organisms_Drug" = parse_drug_affected_organisms(save_table, save_csv, csv_path, override_csv),
        "AHFS_Codes_Drug" = parse_drug_ahfs_codes(save_table, save_csv, csv_path, override_csv),
        "Articles_Drug" = parse_drug_articles(save_table, save_csv, csv_path, override_csv),
        "ATC_Codes_Drug" = parse_drug_atc_codes(save_table, save_csv, csv_path, override_csv),
        "Books_Drug" = parse_drug_books(save_table, save_csv, csv_path, override_csv),
        "Carriers_Drug" = parse_drug_carriers(save_table, save_csv, csv_path, override_csv),
        "Actions_Carrier_Drug" = parse_drug_carriers_actions(save_table, save_csv, csv_path, override_csv),
        "Articles_Carrier_Drug" = parse_drug_carriers_articles(save_table, save_csv, csv_path, override_csv),
        "Links_Carrier_Drug" = parse_drug_carriers_links(save_table, save_csv, csv_path, override_csv),
        "Polypeptides_Carrier_Drugs" = parse_drug_carriers_polypeptides(save_table, save_csv, csv_path, override_csv),
        "External_Identifiers_Polypeptide_Carrier_Drug" =
          parse_drug_carriers_polypeptides_external_identifiers(save_table, save_csv, csv_path, override_csv),
        "GO_Classifiers_Polypeptide_Carrier_Drug" =
          parse_drug_carriers_polypeptides_go_classifiers(save_table, save_csv, csv_path, override_csv),
        "PFAMS_Polypeptide_Carrier_Drug" = parse_drug_carriers_polypeptides_pfams(save_table,
                                                                                  save_csv, csv_path, override_csv),
        "Synonyms_Polypeptide_Carrier_Drug" = parse_drug_carriers_polypeptides_synonyms(save_table,
                                                                                        save_csv, csv_path, override_csv),
        "Textbooks_Carrier_Drug" = parse_drug_carriers_textbooks(save_table, save_csv, csv_path, override_csv),
        "Categories_Drug" = parse_drug_carriers(save_table, save_csv, csv_path, override_csv),
        "Classifications_Drug" = parse_drug_classification(save_table, save_csv, csv_path, override_csv),
        "Dosages_Drug" = parse_drug_dosages(save_table, save_csv, csv_path, override_csv),
        "Enzymes_Drug" = parse_drug_enzymes(save_table, save_csv, csv_path, override_csv),
        "Actions_Enzyme_Drug" = parse_drug_enzymes_actions(save_table, save_csv, csv_path, override_csv),
        "Articles_Enzyme_Drug" = parse_drug_enzymes_articles(save_table, save_csv, csv_path, override_csv),
        "Links_Enzyme_Drug" = parse_drug_enzymes_links(save_table, save_csv, csv_path, override_csv),
        "Polypeptides_Enzyme_Drug" = parse_drug_enzymes_polypeptides(save_table, save_csv, csv_path, override_csv),
        "External_Identifiers_Polypeptide_Enzyme_Drug" =
          parse_drug_enzymes_polypeptides_external_identifiers(save_table, save_csv, csv_path, override_csv),
        "GO_Classifiers_Polypeptides_Enzyme_Drug" =
          parse_drug_enzymes_polypeptides_go_classifiers(save_table, save_csv, csv_path, override_csv),
        "PFAMS_Polypeptides_Enzyme_Drug" = parse_drug_enzymes_polypeptides_pfams(save_table,
                                                                                 save_csv, csv_path, override_csv),
        "Synonyms_Polypeptides_Enzyme_Drug" = parse_drug_enzymes_polypeptides_synonyms(save_table,
                                                                                       save_csv, csv_path, override_csv),
        "Textbooks_Enzyme_Drug" = parse_drug_enzymes_textbooks(save_table, save_csv, csv_path, override_csv),
        "Experimental_Properties_Drug" = parse_drug_experimental_properties(save_table, save_csv, csv_path, override_csv),
        "External_Identifiers_Drug" = parse_drug_external_identifiers(save_table, save_csv, csv_path, override_csv),
        "External_Links_Drug" = parse_drug_external_links(save_table, save_csv, csv_path, override_csv),
        "Food_Interactions_Drug" = parse_drug_food_interactions(save_table, save_csv, csv_path, override_csv),
        "Groups_Drugs" = parse_drug_groups(save_table, save_csv, csv_path, override_csv),
        "Interactions_Drug" = parse_drug_interactions(save_table, save_csv, csv_path, override_csv),
        "Links_Drug" = parse_drug_interactions(save_table, save_csv, csv_path, override_csv),
        "Manufacturers_Drug" = parse_drug_manufacturers(save_table, save_csv, csv_path, override_csv),
        "Mixtures_Drug" = parse_drug_mixtures(save_table, save_csv, csv_path, override_csv),
        "Packagers_Drug" = parse_drug_packagers(save_table, save_csv, csv_path, override_csv),
        "Patents_Drugs" = parse_drug_patents(save_table, save_csv, csv_path, override_csv),
        "Pathways_Drug" = parse_drug_pathway(save_table, save_csv, csv_path, override_csv),
        "Drugs_Pathway_Drug" = parse_drug_pathway_drugs(save_table, save_csv, csv_path, override_csv),
        "Enzymes_Pathway_Drug" = parse_drug_pathway_enzyme(save_table, save_csv, csv_path, override_csv),
        "PDB_Entries_Drug" = parse_drug_pdb_entries(save_table, save_csv, csv_path, override_csv),
        "Prices_Drug" = parse_drug_prices(save_table, save_csv, csv_path, override_csv),
        "Products_Drug" = parse_drug_products(save_table, save_csv, csv_path, override_csv),
        "Reactions_Drugs" = parse_drug_reactions(save_table, save_csv, csv_path, override_csv),
        "Enzymes_Reactions_Drug" = parse_drug_reactions_enzymes(save_table, save_csv, csv_path, override_csv),
        "Sequences_Drug" = parse_drug_sequences(save_table, save_csv, csv_path, override_csv),
        "SNP_Adverse_Drug_Reactions_Drug" = parse_drug_snp_adverse_drug_reactions(save_table,
                                                                                  save_csv, csv_path, override_csv),
        "SNP_Effects_Drug" = parse_drug_snp_effects(save_table, save_csv, csv_path, override_csv),
        "Synonyms_Drug" = parse_drug_synonyms(save_table, save_csv, csv_path, override_csv),
        "Targets_Drug" = parse_drug_targets(save_table, save_csv, csv_path, override_csv),
        "Actions_Target_Drug" = parse_drug_targets_actions(save_table, save_csv, csv_path, override_csv),
        "Articles_Target_Drug" = parse_drug_targets_articles(save_table, save_csv, csv_path, override_csv),
        "Links_Target_Drug" = parse_drug_targets_links(save_table, save_csv, csv_path, override_csv),
        "Polypeptide_Target_Drug" = parse_drug_targets_polypeptides(save_table, save_csv, csv_path, override_csv),
        "External_Identifiers_Polypeptide_Target_Drug" =
          parse_drug_targets_polypeptides_external_identifiers(save_table, save_csv, csv_path, override_csv),
        "GO_Classifiers_Polypeptide_Target_Drug" =
          parse_drug_targets_polypeptides_go_classifiers(save_table, save_csv, csv_path, override_csv),
        "PFAMS_Polypeptide_Target_Drug" = parse_drug_targets_polypeptides_pfams(save_table,
                                                                                save_csv, csv_path, override_csv),
        "Synonyms_Polypeptide_Target_Drug" = parse_drug_targets_polypeptides_synonyms(save_table,
                                                                                      save_csv, csv_path, override_csv),
        "Textbooks_Target_Drug" = parse_drug_targets_textbooks(save_table, save_csv, csv_path, override_csv),
        "Transporters_Drug" = parse_drug_transporters(save_table, save_csv, csv_path, override_csv),
        "Actions_Transporter_Drug" = parse_drug_transporters_actions(save_table, save_csv, csv_path, override_csv),
        "Articles_Transporter_Drug" = parse_drug_targets_articles(save_table, save_csv, csv_path, override_csv),
        "Links_Transporter_Drug" = parse_drug_transporters_links(save_table, save_csv, csv_path, override_csv),
        "Polypeptides_Transporter_Drug" = parse_drug_enzymes_polypeptides(save_table, save_csv, csv_path, override_csv),
        "External_Identifiers_Transporter_Drug" =
          parse_drug_transporters_polypeptides_external_identifiers(save_table, save_csv, csv_path, override_csv),
        "GO_Classifiers_Polypeptide_Transporters_Drug" =
          parse_drug_transporters_polypeptides_go_classifiers(save_table, save_csv, csv_path, override_csv),
        "PFAMS_Polypeptid_Transporter_Drug" = parse_drug_transporters_polypeptides_pfams(save_table,
                                                                                         save_csv, csv_path, override_csv),
        "Synonyms_Polypeptide_Transporter_Drug" = parse_drug_transporters_polypeptides_synonyms(save_table,
                                                                                                save_csv, csv_path,
                                                                                                override_csv),
        "Textbooks_Transporter_Drug" = parse_drug_transporters_textbooks(save_table, save_csv, csv_path, override_csv),
        "International_Brands_Drug" = parse_drug_international_brands(save_table, save_csv, csv_path, override_csv),
        "Salts_Drug" = parse_drug_salts(save_table, save_csv, csv_path, override_csv),
        "Culculated_Properties_Drug" = parse_drug_calculated_properties(save_table, save_csv, csv_path, override_csv)
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
      "Culculated_Properties_Drug",
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
