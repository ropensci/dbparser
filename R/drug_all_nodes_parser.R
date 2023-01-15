#' extracts the all drug elements and return data as list of tibbles.
#'
#' this functions extracts all element of drug nodes in \strong{DrugBank}
#' xml database. it takes two optional arguments to
#' save the returned tibble in the database \code{save_table}
#'
#' @section read_drugbank_xml_db:
#' \code{\link{read_drugbank_xml_db}} function must be called first before any
#' parser.
#'
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @return all drug elements tibbles
#' @family common
#' @examples
#' \dontrun{
#' # the same parameters and usage will be applied for any parser
#' # return only the parsed tibble
#' run_all_parsers()
#'
#' # save parsed tibble as csv if it does not exist in current location,
#' # and return parsed tibble.
#' # if the csv exist before read it and return its data.
#' run_all_parsers(save_csv = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in given location,
#' # and return parsed tibble.
#' # if the csv exist before read it and return its data.
#' run_all_parsers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # if the csv exist override it and return it.
#' run_all_parsers(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @family collective_parsers
#' @export
run_all_parsers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    return(c(
      drugs(
        save_table,
        save_csv,
        csv_path,
        override_csv
      ),
      references(
        save_table,
        save_csv,
        csv_path,
        override_csv
      ),
      cett(
        save_table,
        save_csv,
        csv_path,
        override_csv
      )
    ))
  }

#' extracts the given drug elements and return data as list of tibbles.
#'
#' \code{drug_element} returns list of tibbles of drugs selected
#' elements.
#'
#' it must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' if \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' drug_element_options can be called to know the valid options for
#' this method
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param elements_options list,  options of elements to be parsed. default is
#'  "all"
#'
#' @return list of selected drug elements tibbles
#' @family common
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_element()
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # if the csv exist before read it and return its data.
#' drug_element(save_csv = TRUE)
#'
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # if the csv exist before read it and return its data.
#' drug_element(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # if the csv exist override it and return it.
#' drug_element(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' drug_element(c("drug_ahfs_codes", "drug_carriers"), save_table = TRUE)
#' drug_element(save_table = FALSE)
#' drug_element(c("drug_ahfs_codes", "drug_carriers"))
#' }
#' @keywords internal
#' @export
drug_element <-
  function(elements_options = c("all"),
           save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    if (!all(elements_options %in% drug_element_options())) {
      stop("invalid options\nplease use drug_element_options() to
           know valid options")
    }

    if ("all" %in% elements_options) {
      return(
        run_all_parsers(
          save_table = save_table,
          save_csv = save_csv,
          csv_path = csv_path,
          override_csv = override_csv
        )
      )
    }
    parsed_list <- list()
    for (option in elements_options) {
      parsed_element <- switch(
        option,
        "drugs" = drug_general_information(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "pharmacology_drug" = drug_pharmacology(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "affected_organisms_drug" =
          drug_affected_organisms(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "ahfs_codes_drug" = drug_ahfs_codes(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "articles_drug" = drugs_articles(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "atc_codes_drug" = drug_atc_codes(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "books_drug" = drugs_textbooks(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "carriers_drug" = carriers(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "actions_carrier_drug" = carriers_actions(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "articles_carrier_drug" = carriers_articles(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "links_carrier_drug" = carriers_links(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "polypeptides_carrier_drugs" =
          carriers_polypeptides(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "carr_poly_ext_identity" =
          carriers_polypep_ex_ident(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "carr_polypeptides_go" =
          carriers_polypeptides_go(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "carr_polypeptides_pfams" =
          carriers_polypeptides_pfams(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "carr_polypeptides_syn" =
          carriers_polypeptides_syn(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "textbooks_carrier_drug" = carriers_textbooks(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "categories_drug" = carriers(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "classifications_drug" = drug_classification(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "dosages_drug" = drug_dosages(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "enzymes_drug" = enzymes(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "actions_enzyme_drug" = enzymes_actions(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "articles_enzyme_drug" = enzymes_articles(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "links_enzyme_drug" = enzymes_links(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "polypeptides_enzyme_drug" =
          enzymes_polypeptides(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "enzy_poly_ext_identity" =
          enzymes_polypep_ex_ident(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "enzy_poly_go" =
          enzymes_polypeptides_go(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "pfams_polypeptides_enzyme_drug" =
          enzymes_polypeptides_pfams(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "enzy_poly_syn" =
          enzymes_polypeptides_syn(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "textbooks_enzyme_drug" = enzymes_textbooks(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "experimental_properties_drug" =
          drug_exp_prop(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "external_identifiers_drug" =
          drug_ex_identity(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "external_links_drug" = drug_external_links(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "food_interactions_drug" = drug_food_interactions(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "groups_drugs" = drug_groups(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "interactions_drug" = drug_interactions(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "links_drug" = drug_interactions(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "manufacturers_drug" = drug_manufacturers(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "mixtures_drug" = drug_mixtures(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "packagers_drug" = drug_packagers(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "patents_drugs" = drug_patents(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "pathways_drug" = drug_pathway(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "drugs_pathway_drug" = drug_pathway_drugs(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "enzymes_pathway_drug" = drug_pathway_enzyme(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "pdb_entries_drug" = drug_pdb_entries(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "prices_drug" = drug_prices(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "products_drug" = drug_products(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "reactions_drugs" = drug_reactions(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "enzymes_reactions_drug" = drug_reactions_enzymes(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "sequences_drug" = drug_sequences(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "snp_adverse_reactions" =
          drug_snp_adverse_reactions(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "snp_effects_drug" = drug_snp_effects(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "syn_drug" = drug_syn(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "targ_drug" = targets(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "actions_target_drug" = targets_actions(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "articles_target_drug" = targets_articles(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "links_target_drug" = targets_links(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "polypeptide_target_drug" =
          targets_polypeptides(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "targ_poly_ext_identity" =
          targets_polypep_ex_ident(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "targ_poly_go" =
          targets_polypeptides_go(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "pfams_polypeptide_target_drug" =
          targets_polypeptides_pfams(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "targ_poly_syn" =
          targets_polypeptides(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "textbooks_target_drug" = targets_textbooks(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "transporters_drug" = transporters(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "actions_transporter_drug" =
          transporters_actions(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "articles_transporter_drug" = targets_articles(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "links_transporter_drug" = transporters_links(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "polypeptides_transporter_drug" =
          enzymes_polypeptides(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "trans_poly_ex_identity" =
          transporters_polypep_ex_ident(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "go_polypeptide_trans_drug" =
          transporters_polypeptides_go(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "trans_poly_pfams" =
          transporters_polypeptides_pfams(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "trans_poly_syn" =
          transporters_polypeptides_syn(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "textbooks_transporter_drug" =
          transporters_textbooks(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "international_brands_drug" =
          drug_intern_brand(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "salts_drug" = drug_salts(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "culculated_properties_drug" =
          drug_calc_prop(
            save_table,
            save_csv,
            csv_path,
            override_csv
          ),
        "attachments_drug" = drugs_attachments(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "attachments_carrier" = carriers_attachments(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "attachments_enzyme" = enzymes_attachments(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "attachments_target" = targets_attachments(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "attachments_transporter" = transporters_attachments(
          save_table,
          save_csv,
          csv_path,
          override_csv
        ),
        "references" = references(
          save_table,
          save_csv,
          csv_path,
          override_csv
        )
      )
      parsed_list[[option]] <- parsed_element
      message(paste("parsed", option))
    }
    return(parsed_list)
  }

#' returns \code{drug_element} valid options.
#'
#' @return list of \code{drug_element} valid options
#' @family common
#' @examples
#' \dontrun{
#' drug_element_options()
#' }
#' @keywords internal
#' @export
drug_element_options <- function() {
  elements_options <-
    c(
      "all",
      "drugs",
      "pharmacology_drug",
      "groups_drug",
      "articles_drug",
      "books_drug",
      "links_drug",
      "syn_drug",
      "products_drug",
      "culculated_properties_drug",
      "mixtures_drug",
      "packagers_drug",
      "categories_drug",
      "affected_organisms_drug",
      "dosages_drug",
      "ahfs_codes_drug",
      "pdb_entries_drug",
      "patents_drug",
      "food_interactions_drug",
      "interactions_drug",
      "experimental_properties_drug",
      "external_identifiers_drug",
      "external_links_drug",
      "snp_effects_drug",
      "snp_adverse_reactions",
      "atc_codes_drug",
      "actions_carrier_drug",
      "articles_carrier_drug",
      "textbooks_carrier_drug",
      "links_carrier_drug",
      "polypeptides_carrier_drug",
      "carr_poly_ext_identity",
      "carr_polypeptides_syn",
      "carr_polypeptides_pfams",
      "carr_polypeptides_go",
      "carriers_drug",
      "classifications_drug",
      "actions_enzyme_drug",
      "articles_enzyme_drug",
      "textbooks_enzyme_drug",
      "links_enzyme_drug",
      "polypeptides_enzyme_drug",
      "enzy_poly_ext_identity",
      "enzy_poly_syn",
      "pfams_polypeptides_enzyme_drug",
      "enzy_poly_go",
      "enzymes_drug",
      "manufacturers_drug",
      "enzymes_pathway_drug",
      "drugs_pathway_drug",
      "pathways_drug",
      "prices_drug",
      "reactions_drug",
      "enzymes_reactions_drug",
      "sequences_drug",
      "targ_poly_ext_identity",
      "targ_poly_syn",
      "pfams_polypeptide_target_drug",
      "targ_poly_go",
      "actions_target_drug",
      "articles_target_drug",
      "textbooks_target_drug",
      "links_target_drug",
      "polypeptide_target_drug",
      "targ_drug",
      "actions_transporter_drug",
      "articles_transporter_drug",
      "textbooks_transporter_drug",
      "links_transporter_drug",
      "polypeptides_transporter_drug",
      "trans_poly_ex_identity",
      "trans_poly_syn",
      "trans_poly_pfams",
      "go_polypeptide_trans_drug",
      "transporters_drug",
      "international_brands_drug",
      "salts_drug",
      "attachments_drug",
      "attachments_carrier",
      "attachments_enzyme",
      "attachments_target",
      "attachments_transporter",
      "references"
    )
  return(elements_options)
}

#' Run all drug  related parsers
#'
#' Run all parsers that retrieve drugs related information
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return  a list of all drugs parsed tibbles
#'
#' @family collective_parsers
#'
#' @inherit run_all_parsers examples
#' @export
drugs <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    message("Drugs Information Parsing has Started")
    message("parsing drugs gneral information")
    general_information <- drug_general_information(save_table,
                                                    save_csv,
                                                    csv_path,
                                                    override_csv)

    message("parsing drugs classifications")
    drug_classification <- drug_classification(save_table,
                                               save_csv,
                                               csv_path,
                                               override_csv)

    message("parsing drugs synonyms")
    synonyms <- drug_syn(save_table,
                         save_csv,
                         csv_path,
                         override_csv)

    message("parsing drugs products")
    products <- drug_products(save_table,
                              save_csv,
                              csv_path,
                              override_csv)

    message("parsing drugs pharmacology")
    pharmacology <- drug_pharmacology(save_table,
                                      save_csv,
                                      csv_path,
                                      override_csv)

    message("parsing drugs international brands")
    international_brands <- drug_intern_brand(save_table,
                                              save_csv,
                                              csv_path,
                                              override_csv)

    message("parsing drugs mixtures")
    mixtures <- drug_mixtures(save_table,
                              save_csv,
                              csv_path,
                              override_csv)

    message("parsing drugs packagers")
    packagers <- drug_packagers(save_table,
                                save_csv,
                                csv_path,
                                override_csv)

    message("parsing drugs manufacturers")
    manufacturers <- drug_manufacturers(save_table,
                                        save_csv,
                                        csv_path,
                                        override_csv)

    message("parsing drugs prices")
    prices <- drug_prices(save_table,
                          save_csv,
                          csv_path,
                          override_csv)

    message("parsing drugs categories")
    categories <- drug_categories(save_table,
                                  save_csv,
                                  csv_path,
                                  override_csv)

    message("parsing drugs dosages")
    dosages <- drug_dosages(save_table,
                            save_csv,
                            csv_path,
                            override_csv)

    message("parsing drugs atc_codes")
    atc_codes <- drug_atc_codes(save_table,
                                save_csv,
                                csv_path,
                                override_csv)

    message("parsing drugs patents")
    patents <- drug_patents(save_table,
                            save_csv,
                            csv_path,
                            override_csv)

    message("parsing drugs interactions")
    interactions <- drug_interactions(save_table,
                                      save_csv,
                                      csv_path,
                                      override_csv)

    message("parsing drugs sequences")
    sequences <- drug_sequences(save_table,
                                save_csv,
                                csv_path,
                                override_csv)

    message("parsing drugs calculated properties")
    calc_prop <- drug_calc_prop(save_table,
                                save_csv,
                                csv_path,
                                override_csv)

    message("parsing drugs external identity")
    ex_identity <- drug_ex_identity(save_table,
                                    save_csv,
                                    csv_path,
                                    override_csv)

    message("parsing drugs experimental properties")
    exp_prop <- drug_exp_prop(save_table,
                              save_csv,
                              csv_path,
                              override_csv)

    message("parsing drugs pathway")
    pathway <- drug_pathway(save_table,
                            save_csv,
                            csv_path,
                            override_csv)

    message("parsing drugs pathway drugs")
    pathway_drugs <- drug_pathway_drugs(save_table,
                                        save_csv,
                                        csv_path,
                                        override_csv)

    message("parsing drugs pathway enzyme")
    pathway_enzyme <- drug_pathway_enzyme(save_table,
                                          save_csv,
                                          csv_path,
                                          override_csv)

    message("parsing drugs reactions")
    reactions <- drug_reactions(save_table,
                                save_csv,
                                csv_path,
                                override_csv)

    message("parsing drugs reactions enzymes")
    reactions_enzymes <- drug_reactions_enzymes(save_table,
                                                save_csv,
                                                csv_path,
                                                override_csv)

    message("parsing drugs snp effects")
    snp_effects <- drug_snp_effects(save_table,
                                    save_csv,
                                    csv_path,
                                    override_csv)

    message("parsing drugs snp adverse reactions")
    snp_adverse_reactions <- drug_snp_adverse_reactions(save_table,
                                                        save_csv,
                                                        csv_path,
                                                        override_csv)

    message("parsing drugs food interaction")
    food_interactions <- drug_food_interactions(save_table,
                                                save_csv,
                                                csv_path,
                                                override_csv)

    message("parsing drugs pdb entries")
    pdb_entries <- drug_pdb_entries(save_table,
                                    save_csv,
                                    csv_path,
                                    override_csv)

    message("parsing drugs ahfs codes")
    ahfs_codes <- drug_ahfs_codes(save_table,
                                  save_csv,
                                  csv_path,
                                  override_csv)

    message("parsing drugs affected organisms")
    affected_organisms <- drug_affected_organisms(save_table,
                                                  save_csv,
                                                  csv_path,
                                                  override_csv)

    message("parsing drugs groups")
    groups <- drug_groups(save_table,
                          save_csv,
                          csv_path,
                          override_csv)

    message("parsing drugs external links")
    external_links <- drug_external_links(save_table,
                                          save_csv,
                                          csv_path,
                                          override_csv)

    message("parsing drugs salts")
    salts <- drug_salts(save_table,
                        save_csv,
                        csv_path,
                        override_csv)
    message("Drugs Information Parsing has Completed")
    return(
      list(
        general_information = general_information,
        drug_classification = drug_classification,
        synonyms = synonyms,
        pharmacology = pharmacology,
        international_brands = international_brands,
        mixtures = mixtures,
        packagers = packagers,
        manufacturers = manufacturers,
        prices = prices,
        categories = categories,
        dosages = dosages,
        atc_codes = atc_codes,
        patents = patents,
        interactions = interactions,
        sequences = sequences,
        calc_prop = calc_prop,
        ex_identity = ex_identity,
        exp_prop = exp_prop,
        pathway = pathway,
        pathway_drugs = pathway_drugs,
        pathway_enzyme = pathway_enzyme,
        reactions = reactions,
        reactions_enzymes = reactions_enzymes,
        snp_effects = snp_effects,
        snp_adverse_reactions = snp_adverse_reactions,
        food_interactions = food_interactions,
        pdb_entries = pdb_entries,
        ahfs_codes = ahfs_codes,
        affected_organisms = affected_organisms,
        groups = groups,
        products = products,
        external_links = external_links,
        salts = salts
      )
    )
  }

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters references element parser
#'
#' Return a list of all references for drugs, carriers, enzymes, targets or
#' transporters
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @family references
#'
#' @inherit run_all_parsers examples
#' @export
references <- function(save_table = FALSE,
                       save_csv = FALSE,
                       csv_path = ".",
                       override_csv = FALSE) {
  message("Start extracting references")
  message("parsing drugs articles")
  articles_drug <-
    drugs_articles(save_table,
                   save_csv,
                   csv_path,
                   override_csv)

  message("parsing drugs textbooks")
  books_drug <-
    drugs_textbooks(save_table,
                    save_csv,
                    csv_path,
                    override_csv)

  message("parsing drugs links")
  links_drug <-
    drugs_links(save_table,
                save_csv,
                csv_path,
                override_csv)

  message("parsing drugs attachments")
  attachments_drug <-
    drugs_attachments(save_table,
                      save_csv,
                      csv_path,
                      override_csv)

  message("parsing carriers articles")
  articles_carrier <-
    carriers_articles(save_table,
                      save_csv,
                      csv_path,
                      override_csv)

  message("parsing carriers textbooks")
  textbooks_carrier <-
    carriers_textbooks(save_table,
                       save_csv,
                       csv_path,
                       override_csv)

  message("parsing carriers links")
  links_carrier <-
    carriers_links(save_table,
                   save_csv,
                   csv_path,
                   override_csv)

  message("parsing carriers attachments")
  attachments_carrier <-
    carriers_attachments(save_table,
                         save_csv,
                         csv_path,
                         override_csv)

  message("parsing enzymes articles")
  articles_enzyme <-
    enzymes_articles(save_table,
                     save_csv,
                     csv_path,
                     override_csv)

  message("parsing enzymes textbooks")
  textbooks_enzyme <-
    enzymes_textbooks(save_table,
                      save_csv,
                      csv_path,
                      override_csv)

  message("parsing enzymes links")
  links_enzyme <-
    enzymes_links(save_table,
                  save_csv,
                  csv_path,
                  override_csv)

  message("parsing enzymes attachments")
  attachments_enzyme <-
    enzymes_attachments(save_table,
                        save_csv,
                        csv_path,
                        override_csv)

  message("parsing targets articles")
  articles_target <-
    targets_articles(save_table,
                     save_csv,
                     csv_path,
                     override_csv)

  message("parsing targets textbooks")
  textbooks_target <-
    targets_textbooks(save_table,
                      save_csv,
                      csv_path,
                      override_csv)

  message("parsing targets links")
  links_target <-
    targets_links(save_table,
                  save_csv,
                  csv_path,
                  override_csv)

  message("parsing targets attachments")
  attachments_target <-
    targets_attachments(save_table,
                  save_csv,
                  csv_path,
                  override_csv)

  message("parsing transporters articles")
  articles_transporter <-
    transporters_articles(save_table,
                          save_csv,
                          csv_path,
                          override_csv)


  message("parsing transporters textbooks")
  textbooks_transporter <-
    transporters_textbooks(save_table,
                           save_csv,
                           csv_path,
                           override_csv)

  message("parsing transporters links")
  links_transporter <-
    transporters_links(save_table,
                       save_csv,
                       csv_path,
                       override_csv)

  message("parsing transporters attachments")
  attachments_transporter <-
    transporters_attachments(save_table,
                             save_csv,
                             csv_path,
                             override_csv)
  message("Extracting references is over!")
  return(
    list(
      # drugs
      articles_drug = articles_drug,
      books_drug = books_drug,
      links_drug = links_drug,
      attachments_drug = attachments_drug,
      # carriers
      articles_carrier = articles_carrier,
      textbooks_carrier = textbooks_carrier,
      links_carrier = links_carrier,
      attachments_carrier = attachments_carrier,
      # enzymes
      articles_enzyme = articles_enzyme,
      textbooks_enzyme = textbooks_enzyme,
      links_enzyme = links_enzyme,
      attachments_enzyme = attachments_enzyme,
      # targets
      articles_target = articles_target,
      textbooks_target = textbooks_target,
      links_target = links_target,
      attachments_target = attachments_target,
      # transporters
      articles_transporter = articles_transporter,
      textbooks_transporter = textbooks_transporter,
      links_transporter = links_transporter,
      attachments_transporter = attachments_transporter
    )
  )
}

#' Run all CETT related parsers
#'
#' Run all parsers that retrieve carriers, enzymes, targets and transporters
#'  related information
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return  a list of all drugs parsed tibbles
#'
#' @family collective_parsers
#'
#' @inherit run_all_parsers examples
#' @export
cett <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    message("CETT Information Parsing has Started")
    message("parsing carriers")
    carriers <-
      carriers(save_table,
               save_csv,
               csv_path,
               override_csv)

    message("parsing carriers actions")
    carriers_actions <-
      carriers_actions(save_table,
                       save_csv,
                       csv_path,
                       override_csv)

    message("parsing carriers polypeptides")
    carriers_polypeptides <- carriers_polypeptides(save_table,
                                                   save_csv,
                                                   csv_path,
                                                   override_csv)

    message("parsing carriers polypepeptide external identy")
    carriers_polypep_ex_ident <- carriers_polypep_ex_ident(save_table,
                                                         save_csv,
                                                         csv_path,
                                                         override_csv)

    message("parsing carriers polypeptides syn")
    carriers_polypeptides_syn <- carriers_polypeptides_syn(save_table,
                                                         save_csv,
                                                         csv_path,
                                                         override_csv)

    message("parsing carriers polypeptides pfams")
    carriers_polypeptides_pfams <-
      carriers_polypeptides_pfams(save_table,
                                 save_csv,
                                 csv_path,
                                 override_csv)

    message("parsing carriers polypeptides go")
    carriers_polypeptides_go <- carriers_polypeptides_go(save_table,
                                                       save_csv,
                                                       csv_path,
                                                       override_csv)

    message("parsing enzymes")
    enzymes <-
      enzymes(save_table,
              save_csv,
              csv_path,
              override_csv)

    message("parsing enzymes actions")
    enzymes_actions <-
      enzymes_actions(save_table,
                      save_csv,
                      csv_path,
                      override_csv)

    message("parsing enzymes polypeptides")
    enzymes_polypeptides <- enzymes_polypeptides(save_table,
                                                 save_csv,
                                                 csv_path,
                                                 override_csv)

    message("parsing enzymes polypepeptide external identy")
    enzymes_polypep_ex_ident <- enzymes_polypep_ex_ident(save_table,
                                                         save_csv,
                                                         csv_path,
                                                         override_csv)

    message("parsing enzymes polypeptides syn")
    enzymes_polypeptides_syn <- enzymes_polypeptides_syn(save_table,
                                                         save_csv,
                                                         csv_path,
                                                         override_csv)

    message("parsing enzymes polypeptides pfams")
    enzymes_polypeptides_pfams <-
      enzymes_polypeptides_pfams(save_table,
                                 save_csv,
                                 csv_path,
                                 override_csv)

    message("parsing enzymes polypeptides go")
    enzymes_polypeptides_go <- enzymes_polypeptides_go(save_table,
                                                       save_csv,
                                                       csv_path,
                                                       override_csv)

    message("parsing transporters")
    transporters <-
      transporters(save_table,
                   save_csv,
                   csv_path,
                   override_csv)

    message("parsing transporters actions")
    transporters_actions <-
      transporters_actions(save_table,
                           save_csv,
                           csv_path,
                           override_csv)

    message("parsing transporters polypeptides")
    transporters_polypeptides <-
      transporters_polypeptides(save_table,
                                save_csv,
                                csv_path,
                                override_csv)

    message("parsing transporters polypepeptide external identy")
    transporters_polypep_ex_ident <-
      transporters_polypep_ex_ident(save_table,
                                    save_csv,
                                    csv_path,
                                    override_csv)

    message("parsing transporters polypeptides syn")
    transporters_polypeptides_syn <-
      transporters_polypeptides_syn(save_table,
                                    save_csv,
                                    csv_path,
                                    override_csv)

    message("parsing transporters polypeptides pfams")
    transporters_polypeptides_pfams <-
      transporters_polypeptides_pfams(save_table,
                                      save_csv,
                                      csv_path,
                                      override_csv)

    message("parsing transporters polypeptides go")
    transporters_polypeptides_go <-
      transporters_polypeptides_go(save_table,
                                   save_csv,
                                   csv_path,
                                   override_csv)

    message("parsing targets")
    targets <-
      targets(save_table,
              save_csv,
              csv_path,
              override_csv)

    message("parsing targets actions")
    targets_actions <-
      targets_actions(save_table,
                      save_csv,
                      csv_path,
                      override_csv)

    message("parsing targets polypeptides")
    targets_polypeptides <- targets_polypeptides(save_table,
                                                 save_csv,
                                                 csv_path,
                                                 override_csv)

    message("parsing targets polypepeptide external identy")
    targets_polypep_ex_ident <- targets_polypep_ex_ident(save_table,
                                                         save_csv,
                                                         csv_path,
                                                         override_csv)

    message("parsing targets polypeptides syn")
    targets_polypeptides_syn <- targets_polypeptides_syn(save_table,
                                                         save_csv,
                                                         csv_path,
                                                         override_csv)

    message("parsing targets polypeptides pfams")
    targets_polypeptides_pfams <-
      targets_polypeptides_pfams(save_table,
                                 save_csv,
                                 csv_path,
                                 override_csv)

    message("parsing targets polypeptides go")
    targets_polypeptides_go <- targets_polypeptides_go(save_table,
                                                       save_csv,
                                                       csv_path,
                                                       override_csv)
    message("CETT Information Parsing has Completed")
    return(
      list(
        carriers = carriers,
        carriers_actions = carriers_actions,
        carriers_polypeptides = carriers_polypeptides,
        carriers_polypep_ex_ident = carriers_polypep_ex_ident,
        carriers_polypeptides_syn = carriers_polypeptides_syn,
        carriers_polypeptides_pfams = carriers_polypeptides_pfams,
        carriers_polypeptides_go = carriers_polypeptides_go,
        enzymes = enzymes,
        enzymes_actions = enzymes_actions,
        enzymes_polypeptides = enzymes_polypeptides,
        enzymes_polypep_ex_ident = enzymes_polypep_ex_ident,
        enzymes_polypeptides_syn = enzymes_polypeptides_syn,
        enzymes_polypeptides_pfams = enzymes_polypeptides_pfams,
        enzymes_polypeptides_go = enzymes_polypeptides_go,
        transporters = transporters,
        transporters_actions = transporters_actions,
        transporters_polypeptides = transporters_polypeptides,
        transporters_polypep_ex_ident = transporters_polypep_ex_ident,
        transporters_polypeptides_syn = transporters_polypeptides_syn,
        transporters_polypeptides_pfams = transporters_polypeptides_pfams,
        transporters_polypeptides_go = transporters_polypeptides_go,
        targets = targets,
        targets_actions = targets_actions,
        targets_polypeptides = targets_polypeptides,
        targets_polypep_ex_ident = targets_polypep_ex_ident,
        targets_polypeptides_syn = targets_polypeptides_syn,
        targets_polypeptides_pfams = targets_polypeptides_pfams,
        targets_polypeptides_go = targets_polypeptides_go
      )
    )
  }
