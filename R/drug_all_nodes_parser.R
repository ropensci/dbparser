#' extracts the all drug elements and return data as list of tibbles.
#'
#' this functions extracts all element of drug nodes in \strong{DrugBank}
#' xml database with the option to save it in a predefined database via
#' passed database connection. it takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
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
#' @param database_connection DBI connection object that holds a connection to
#'  user defined database. If \code{save_table} is enabled without providing
#'  value for this function an error will be thrown.
#' @return all drug elements tibbles
#' @family common
#' @examples
#' \dontrun{
#' # the same parameters and usage will be applied for any parser
#' # return only the parsed tibble
#' drug_all()
#'
#' # will throw an error, as database_connection is NULL
#' drug_all(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_all(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location,
#' # and return parsed tibble.
#' # if the csv exist before read it and return its data.
#' drug_all(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv,
#' # if it does not exist in current location and return parsed tibble.
#' # if the csv exist before read it and return its data.
#' drug_all(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location,
#' # and return parsed tibble.
#' # if the csv exist before read it and return its data.
#' drug_all(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # if the csv exist override it and return it.
#' drug_all(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_all <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    # Drugs
    ## General Information
    ## Drug Classification
    ## Synonyms
    ## External Codes
    ## Pharmacology
    ## International Brands
    ## Mixtures
    ## Packagers
    ## Manufacturers
    ## Prices
    ## Categories
    ## Dosages
    ## ATC Codes
    ## Patents
    ## Drug Interactions
    ## Sequences
    ## Calculated Properties
    ## Experimental Properties
    ## External Identifiers
    ## External Links
    ## Pathways
    ## Reactions
    ## SNP Effects
    ## SNP Adverse Drug Reactions
    # Products
    # References
    ## Articles
    ## TextBooks
    ## Links
    ## Atachments
    # Salts
    # Carriers/ Enzymes/Targets/ Transporters
    ## General Information
    ## Polypeptides
    check_parameters_validation(save_table, database_connection)
    drugs <- drug(save_table,
                  save_csv,
                  csv_path,
                  override_csv,
                  database_connection)
    message("parsed drugs main attributes, 1/76")
    groups_drug <-
      drug_groups(save_table,
                  save_csv,
                  csv_path,
                  override_csv,
                  database_connection)
    message("parsed groups_drug, 2/76")
    articles_drug <-
      drug_articles(save_table,
                    save_csv,
                    csv_path,
                    override_csv,
                    database_connection)
    message("parsed articles_drug, 3/76")
    books_drug <-
      drugs_textbooks(save_table,
                 save_csv,
                 csv_path,
                 override_csv,
                 database_connection)
    message("parsed books_drug, 4/76")
    links_drug <-
      drug_links(save_table,
                 save_csv,
                 csv_path,
                 override_csv,
                 database_connection)
    message("parsed links_drug, 5/76")
    syn_drug <-
      drug_syn(save_table,
               save_csv,
               csv_path,
               override_csv,
               database_connection)
    message("parsed syn_drug, 6/76")
    products_drug <-
      drug_products(save_table,
                    save_csv,
                    csv_path,
                    override_csv,
                    database_connection)
    message("parsed products_drug, 7/76")
    mixtures_drug <-
      drug_mixtures(save_table,
                    save_csv,
                    csv_path,
                    override_csv,
                    database_connection)
    message("parsed mixtures_drug, 8/76")
    packagers_drug <-
      drug_packagers(save_table,
                     save_csv,
                     csv_path,
                     override_csv,
                     database_connection)
    message("parsed packagers_drug, 9/76")
    categories_drug <-
      drug_categories(save_table,
                      save_csv,
                      csv_path,
                      override_csv,
                      database_connection)
    message("parsed categories_drug, 10/76")
    affected_organisms_drug <-
      drug_affected_organisms(save_table,
                              save_csv,
                              csv_path,
                              override_csv,
                              database_connection)
    message("parsed affected_organisms_drug, 11/76")
    dosages_drug <-
      drug_dosages(save_table,
                   save_csv,
                   csv_path,
                   override_csv,
                   database_connection)
    message("parsed dosages_drug, 12/76")
    ahfs_codes_drug <-
      drug_ahfs_codes(save_table,
                      save_csv,
                      csv_path,
                      override_csv,
                      database_connection)
    message("parsed ahfs_codes_drug, 13/76")
    pdb_entries_drug <-
      drug_pdb_entries(save_table,
                       save_csv,
                       csv_path,
                       override_csv,
                       database_connection)
    message("parsed pdb_entries_drug, 14/76")
    patents_drug <-
      drug_patents(save_table,
                   save_csv,
                   csv_path,
                   override_csv,
                   database_connection)
    message("parsed patents_drug, 15/76")
    food_interactions_drug <-
      drug_food_interactions(save_table,
                             save_csv,
                             csv_path,
                             override_csv,
                             database_connection)
    message("parsed food_interactions_drug, 16/76")
    interactions_drug <-
      drug_interactions(save_table,
                        save_csv,
                        csv_path,
                        override_csv,
                        database_connection)
    message("parsed interactions_drug, 17/76")
    experimental_properties_drug <-
      drug_exp_prop(save_table,
                    save_csv,
                    csv_path,
                    override_csv,
                    database_connection)
    message("parsed experimental_properties_drug, 18/76")
    external_identifiers_drug <-
      drug_ex_identity(save_table,
                       save_csv,
                       csv_path,
                       override_csv,
                       database_connection)
    message("parsed external_identifiers_drug, 19/76")
    external_links_drug <-
      drug_external_links(save_table,
                          save_csv,
                          csv_path,
                          override_csv,
                          database_connection)
    message("parsed external_links_drug, 20/76")
    snp_effects_drug <-
      drug_snp_effects(save_table,
                       save_csv,
                       csv_path,
                       override_csv,
                       database_connection)
    message("parsed snp_effects_drug, 21/76")
    snp_adverse_reactions <-
      drug_snp_adverse_reactions(save_table,
                                 save_csv,
                                 csv_path,
                                 override_csv,
                                 database_connection)
    message("parsed snp_adverse_reactions, 22/76")
    atc_codes_drug <-
      drug_atc_codes(save_table,
                     save_csv,
                     csv_path,
                     override_csv,
                     database_connection)
    message("parsed atc_codes_drug, 23/76")
    actions_carrier_drug <-
      carriers_actions(save_table,
                       save_csv,
                       csv_path,
                       override_csv,
                       database_connection)
    message("parsed actions_carrier_drug, 24/76")
    articles_carrier_drug <-
      carriers_articles(save_table,
                        save_csv,
                        csv_path,
                        override_csv,
                        database_connection)
    message("parsed articles_carrier_drug, 25/76")
    textbooks_carrier_drug <-
      carriers_textbooks(save_table,
                         save_csv,
                         csv_path,
                         override_csv,
                         database_connection)
    message("parsed textbooks_carrier_drug, 26/76")
    links_carrier_drug <-
      carriers_links(save_table,
                     save_csv,
                     csv_path,
                     override_csv,
                     database_connection)
    message("parsed links_carrier_drug, 27/76")
    polypeptides_carrier_drug <-
      carriers_polypeptide(save_table,
                           save_csv,
                           csv_path,
                           override_csv,
                           database_connection)
    message("parsed polypeptides_carrier_drug, 28/76")
    carr_poly_ext_identity <-
      carriers_polypeptide_ext_id(save_table,
                                        save_csv,
                                        csv_path,
                                        override_csv,
                                        database_connection)
    message("parsed carr_poly_ext_identity, 29/76")
    carr_polypeptides_syn <-
      carriers_polypeptides_syn(save_table,
                                       save_csv,
                                       csv_path,
                                       override_csv,
                                       database_connection)
    message("parsed carr_polypeptides_syn, 30/76")
    carr_polypeptides_pfams <-
      carriers_polypeptides_pfams(save_table,
                                         save_csv,
                                         csv_path,
                                         override_csv,
                                         database_connection)
    message("parsed carr_polypeptides_pfams, 31/76")
    carr_polypeptides_go <-
      carriers_polypeptides_go(save_table,
                                      save_csv,
                                      csv_path,
                                      override_csv,
                                      database_connection)
    message("parsed carr_polypeptides_go, 32/76")
    carriers_drug <-
      carriers(save_table,
               save_csv,
               csv_path,
               override_csv,
               database_connection)
    message("parsed carriers_drug, 33/76")
    classifications_drug <-
      drug_classification(save_table,
                          save_csv,
                          csv_path,
                          override_csv,
                          database_connection)
    message("parsed classifications_drug, 34/76")
    actions_enzyme_drug <-
      enzymes_actions(save_table,
                      save_csv,
                      csv_path,
                      override_csv,
                      database_connection)
    message("parsed actions_enzyme_drug, 35/76")
    articles_enzyme_drug <-
      enzymes_articles(save_table,
                       save_csv,
                       csv_path,
                       override_csv,
                       database_connection)
    message("parsed articles_enzyme_drug, 36/76")
    textbooks_enzyme_drug <-
      enzymes_textbooks(save_table,
                        save_csv,
                        csv_path,
                        override_csv,
                        database_connection)
    message("parsed textbooks_enzyme_drug, 37/76")
    links_enzyme_drug <-
      enzymes_links(save_table,
                    save_csv,
                    csv_path,
                    override_csv,
                    database_connection)
    message("parsed links_enzyme_drug, 38/76")
    polypeptides_enzyme_drug <-
      enzymes_polypeptide(save_table,
                          save_csv,
                          csv_path,
                          override_csv,
                          database_connection)
    message("parsed polypeptides_enzyme_drug, 39/76")
    enzy_poly_ext_identity <-
      enzymes_polypeptide_ext_ident(save_table,
                                    save_csv,
                                    csv_path,
                                    override_csv,
                                    database_connection)
    message("parsed external_identifiers_polypeptides_enzyme_drug, 40/76")
    enzy_poly_syn <-
      enzymes_polypeptide_syn(save_table,
                              save_csv,
                              csv_path,
                              override_csv,
                              database_connection)
    message("parsed enzy_poly_syn, 41/76")
    pfams_polypeptides_enzyme_drug <-
      enzymes_polypeptide_pfams(save_table,
                                save_csv,
                                csv_path,
                                override_csv,
                                database_connection)
    message("parsed pfams_polypeptides_enzyme_drug, 42/76")
    enzy_poly_go <-
      enzymes_polypeptide_go(save_table,
                             save_csv,
                             csv_path,
                             override_csv,
                             database_connection)
    message("parsed enzy_poly_go, 43/76")
    enzymes_drug <-
      enzymes(save_table,
              save_csv,
              csv_path,
              override_csv,
              database_connection)
    message("parsed enzyme_drug, 44/76")
    manufacturers_drug <-
      drug_manufacturers(save_table,
                         save_csv,
                         csv_path,
                         override_csv,
                         database_connection)
    message("parsed manufacturers_drug, 45/76")
    enzymes_pathway_drug <-
      drug_pathway_enzyme(save_table,
                          save_csv,
                          csv_path,
                          override_csv,
                          database_connection)
    message("parsed enzymes_pathway_drug, 46/76")
    drugs_pathway_drug <-
      drug_pathway_drugs(save_table,
                         save_csv,
                         csv_path,
                         override_csv,
                         database_connection)
    message("parsed drug_pathway_drugs, 47/76")
    pathways_drug <-
      drug_pathway(save_table,
                   save_csv,
                   csv_path,
                   override_csv,
                   database_connection)
    message("parsed pathways_drug, 48/76")
    prices_drug <-
      drug_prices(save_table,
                  save_csv,
                  csv_path,
                  override_csv,
                  database_connection)
    message("parsed prices_drug, 49/76")
    reactions_drug <-
      drug_reactions(save_table,
                     save_csv,
                     csv_path,
                     override_csv,
                     database_connection)
    message("parsed reactions_drug, 50/76")
    enzymes_reactions_drug <-
      drug_reactions_enzymes(save_table,
                             save_csv,
                             csv_path,
                             override_csv,
                             database_connection)
    message("parsed enzymes_reactions_drug, 51/76")
    sequences_drug <-
      drug_sequences(save_table,
                     save_csv,
                     csv_path,
                     override_csv,
                     database_connection)
    message("parsed sequences_drug, 52/76")
    targ_poly_ext_identity <-
      targets_polypeptide_ext_ident(save_table,
                                    save_csv,
                                    csv_path,
                                    override_csv,
                                    database_connection)
    message("parsed targ_poly_ext_identity, 53/76")
    targ_poly_syn <-
      targets_polypeptide_syn(save_table,
                              save_csv,
                              csv_path,
                              override_csv,
                              database_connection)
    message("parsed targ_poly_syn, 54/76")
    pfams_polypeptide_target_drug <-
      targets_polypeptide_pfams(save_table,
                                save_csv,
                                csv_path,
                                override_csv,
                                database_connection)
    message("parsed pfams_polypeptide_target_drug, 55/76")
    targ_poly_go <-
      targets_polypeptide_go(save_table,
                             save_csv,
                             csv_path,
                             override_csv,
                             database_connection)
    message("parsed targ_poly_go attributes, 56/76")
    actions_target_drug <-
      targets_actions(save_table,
                      save_csv,
                      csv_path,
                      override_csv,
                      database_connection)
    message("parsed actions_target_drug, 57/76")
    articles_target_drug <-
      targets_articles(save_table,
                       save_csv,
                       csv_path,
                       override_csv,
                       database_connection)
    message("parsed articles_target_drug, 58/76")
    textbooks_target_drug <-
      targets_textbooks(save_table,
                        save_csv,
                        csv_path,
                        override_csv,
                        database_connection)
    message("parsed textbooks_target_drug, 59/76")
    links_target_drug <-
      targets_links(save_table,
                    save_csv,
                    csv_path,
                    override_csv,
                    database_connection)
    message("parsed links_target_drug, 60/76")
    polypeptide_target_drug <-
      targets_polypeptide(save_table,
                          save_csv,
                          csv_path,
                          override_csv,
                          database_connection)
    message("parsed polypeptide_target_drug, 61/76")
    targ_drug <-
      targets(save_table,
              save_csv,
              csv_path,
              override_csv,
              database_connection)
    message("parsed targ_drug, 62/76")
    actions_transporter_drug <-
      transporters_actions(save_table,
                           save_csv,
                           csv_path,
                           override_csv,
                           database_connection)
    message("parsed actions_transporter_drug, 63/76")
    articles_transporter_drug <-
      transporters_articles(save_table,
                            save_csv,
                            csv_path,
                            override_csv,
                            database_connection)
    message("parsed articles_transporter_drug, 64/76")
    textbooks_transporter_drug <-
      transporters_textbooks(save_table,
                             save_csv,
                             csv_path,
                             override_csv,
                             database_connection)
    message("parsed textbooks_transporter_drug, 65/76")
    links_transporter_drug <-
      transporters_links(save_table,
                         save_csv,
                         csv_path,
                         override_csv,
                         database_connection)
    message("parsed links_transporter_drug, 66/76")
    polypeptides_transporter_drug <-
      transporters_polypeptide(save_table,
                               save_csv,
                               csv_path,
                               override_csv,
                               database_connection)
    message("parsed polypeptides_transporter_drug, 67/76")
    trans_poly_ex_identity <-
      transporters_polypep_ex_ident(save_table,
                                    save_csv,
                                    csv_path,
                                    override_csv,
                                    database_connection)
    message("parsed trans_poly_ex_identity, 68/76")
    trans_poly_syn <-
      transporters_polypeptide_syn(save_table,
                                   save_csv,
                                   csv_path,
                                   override_csv,
                                   database_connection)
    message("parsed syn_polypeptides_transporter_drug, 69/76")
    trans_poly_pfams <-
      transporters_polypeptide_pfams(save_table,
                                     save_csv,
                                     csv_path,
                                     override_csv,
                                     database_connection)
    message("parsed pfams_polypeptides_transporter_drug, 70/76")
    trans_poly_go <-
      transporters_polypeptide_go(save_table,
                                  save_csv,
                                  csv_path,
                                  override_csv,
                                  database_connection)
    message("parsed go_polypeptides_transporter_drug, 71/76")
    transporters_drug <-
      transporters(save_table,
                   save_csv,
                   csv_path,
                   override_csv,
                   database_connection)
    message("parsed transporters_drug, 72/76")
    international_brands_drug <-
      drug_intern_brand(save_table,
                        save_csv,
                        csv_path,
                        override_csv,
                        database_connection)
    message("parsed international_brands_drug, 73/76")
    salts_drug <-
      drug_salts(save_table,
                 save_csv,
                 csv_path,
                 override_csv,
                 database_connection)
    message("parsed salts_drug, 74/76")
    calculated_properties_drug <-
      drug_calc_prop(save_table,
                     save_csv,
                     csv_path,
                     override_csv,
                     database_connection)
    message("parsed calculated_properties_drug, 75/76")
    drugs_pharmacology <- drug_pharmacology(save_table,
                  save_csv,
                  csv_path,
                  override_csv,
                  database_connection)
    message("parsed drugs main attributes, 76/76")
    return(
      list(
        drugs = drugs,
        groups_drug = groups_drug,
        articles_drug = articles_drug,
        books_drug = books_drug,
        links_drug = links_drug,
        syn_drug = syn_drug,
        products_drug = products_drug,
        mixtures_drug = mixtures_drug,
        packagers_drug = packagers_drug,
        categories_drug = categories_drug,
        affected_organisms_drug = affected_organisms_drug,
        dosages_drug = dosages_drug,
        ahfs_codes_drug = ahfs_codes_drug,
        pdb_entries_drug = pdb_entries_drug,
        patents_drug = patents_drug,
        food_interactions_drug = food_interactions_drug,
        interactions_drug = interactions_drug,
        experimental_properties_drug = experimental_properties_drug,
        external_identifiers_drug = external_identifiers_drug,
        external_links_drug = external_links_drug,
        snp_effects_drug = snp_effects_drug,
        snp_adverse_reactions = snp_adverse_reactions,
        atc_codes_drug = atc_codes_drug,
        actions_carrier_drug = actions_carrier_drug,
        articles_carrier_drug = articles_carrier_drug,
        textbooks_carrier_drug = textbooks_carrier_drug,
        links_carrier_drug = links_carrier_drug,
        polypeptides_carrier_drug = polypeptides_carrier_drug,
        carr_poly_ext_identity =
          carr_poly_ext_identity,
        carr_polypeptides_syn = carr_polypeptides_syn,
        carr_polypeptides_pfams = carr_polypeptides_pfams,
        carr_polypeptides_go =
          carr_polypeptides_go,
        carriers_drug = carriers_drug,
        classifications_drug =
          classifications_drug,
        actions_enzyme_drug = actions_enzyme_drug,
        articles_enzyme_drug = articles_enzyme_drug,
        textbooks_enzyme_drug = textbooks_enzyme_drug,
        links_enzyme_drug = links_enzyme_drug,
        polypeptides_enzyme_drug = polypeptides_enzyme_drug,
        enzy_poly_ext_identity =
          enzy_poly_ext_identity,
        enzy_poly_syn = enzy_poly_syn,
        pfams_polypeptides_enzyme_drug = pfams_polypeptides_enzyme_drug,
        enzy_poly_go =
          enzy_poly_go,
        enzymes_drug = enzymes_drug,
        manufacturers_drug = manufacturers_drug,
        enzymes_pathway_drug = enzymes_pathway_drug,
        drugs_pathway_drug = drugs_pathway_drug,
        pathways_drug = pathways_drug,
        prices_drug = prices_drug,
        reactions_drug = reactions_drug,
        enzymes_reactions_drug = enzymes_reactions_drug,
        sequences_drug = sequences_drug,
        targ_poly_ext_identity =
          targ_poly_ext_identity,
        targ_poly_syn = targ_poly_syn,
        pfams_polypeptide_target_drug = pfams_polypeptide_target_drug,
        targ_poly_go =
          targ_poly_go,
        actions_target_drug = actions_target_drug,
        articles_target_drug = articles_target_drug,
        textbooks_target_drug = textbooks_target_drug,
        links_target_drug = links_target_drug,
        polypeptide_target_drug = polypeptide_target_drug,
        targ_drug = targ_drug,
        actions_transporter_drug = actions_transporter_drug,
        articles_transporter_drug = articles_transporter_drug,
        textbooks_transporter_drug = textbooks_transporter_drug,
        links_transporter_drug = links_transporter_drug,
        polypeptides_transporter_drug = polypeptides_transporter_drug,
        trans_poly_ex_identity =
          trans_poly_ex_identity,
        trans_poly_syn =
          trans_poly_syn,
        trans_poly_pfams = trans_poly_pfams,
        trans_poly_go =
          trans_poly_go,
        transporters_drug = transporters_drug,
        international_brands_drug = international_brands_drug,
        salts_drug = salts_drug,
        culculated_properties_drug = calculated_properties_drug,
        drugs_pharmacology = drugs_pharmacology
      )
    )
  }

#' extracts the given drug elements and return data as list of tibbles.
#'
#' \code{drug_element} returns list of tibbles of drugs selected
#' elements.
#'
#' this functions extracts selected element of drug nodes in \strong{DrugBank}
#' xml database with the option to save it in a predefined database via
#' passed database connection. it takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#'  \code{database_connection}.
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
#' @param database_connection DBI connection object that holds a connection to
#'  user defined database. If \code{save_table} is enabled without providing
#'  value for this function an error will be thrown.
#'  @return list of selected drug elements tibbles
#' @family common
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_element()
#'
#' # will throw an error, as database_connection is NULL
#' drug_element(save_table = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # if the csv exist before read it and return its data.
#' drug_element(save_csv = TRUE)
#'
#'
#  # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_element(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save in database, save parsed tibble as csv if it does not
#' # exist in current location and return parsed tibble.
#' # if the csv exist before read it and return its data.
#' drug_element(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
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
#' @export
drug_element <-
  function(elements_options = c("all"),
           save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    if (!all(elements_options %in% drug_element_options())) {
      stop("invalid options\nplease use drug_element_options() to
           know valid options")
    }

    if ("all" %in% elements_options) {
      return(drug_all(save_table = save_table,
                      save_csv = save_csv,
                      csv_path = csv_path,
                      override_csv = override_csv,
                      database_connection = database_connection))
    }
    parsed_list <- list()
    for (option in elements_options) {
      parsed_element <- switch(
        option,
        "drugs" = drug(save_table, save_csv, csv_path, override_csv,
                       database_connection),
        "pharmacology_drug" = drug_pharmacology(save_table, save_csv, csv_path, override_csv,
                       database_connection),
        "affected_organisms_drug" =
          drug_affected_organisms(save_table,
                                  save_csv,
                                  csv_path,
                                  override_csv,
                                  database_connection),
        "ahfs_codes_drug" = drug_ahfs_codes(save_table, save_csv,
                                            csv_path, override_csv,
                                            database_connection),
        "articles_drug" = drug_articles(save_table, save_csv, csv_path,
                                        override_csv,
                                        database_connection),
        "atc_codes_drug" = drug_atc_codes(save_table, save_csv, csv_path,
                                          override_csv,
                                          database_connection),
        "books_drug" = drugs_textbooks(save_table, save_csv, csv_path,
                                  override_csv,
                                  database_connection),
        "carriers_drug" = carriers(save_table, save_csv, csv_path,
                                   override_csv,
                                   database_connection),
        "actions_carrier_drug" = carriers_actions(save_table,
                                                  save_csv,
                                                  csv_path,
                                                  override_csv,
                                                  database_connection),
        "articles_carrier_drug" = carriers_articles(save_table,
                                                    save_csv,
                                                    csv_path,
                                                    override_csv,
                                                    database_connection),
        "links_carrier_drug" = carriers_links(save_table,
                                              save_csv,
                                              csv_path,
                                              override_csv,
                                              database_connection),
        "polypeptides_carrier_drugs" =
          carriers_polypeptide(save_table, save_csv,
                               csv_path, override_csv,
                               database_connection),
        "carr_poly_ext_identity" =
          carriers_polypeptide_ext_id(save_table,
                                            save_csv,
                                            csv_path,
                                            override_csv,
                                            database_connection),
        "carr_polypeptides_go" =
          carriers_polypeptides_go(save_table,
                                          save_csv,
                                          csv_path,
                                          override_csv,
                                          database_connection),
        "carr_polypeptides_pfams" =
          carriers_polypeptides_pfams(save_table,
                                             save_csv, csv_path,
                                             override_csv,
                                             database_connection),
        "carr_polypeptides_syn" =
          carriers_polypeptides_syn(save_table,
                                           save_csv, csv_path,
                                           override_csv,
                                           database_connection),
        "textbooks_carrier_drug" = carriers_textbooks(save_table,
                                                      save_csv,
                                                      csv_path,
                                                      override_csv,
                                                      database_connection),
        "categories_drug" = carriers(save_table, save_csv, csv_path,
                                     override_csv,
                                     database_connection),
        "classifications_drug" = drug_classification(save_table, save_csv,
                                                     csv_path,
                                                     override_csv,
                                                     database_connection),
        "dosages_drug" = drug_dosages(save_table, save_csv, csv_path,
                                      override_csv,
                                      database_connection),
        "enzymes_drug" = enzymes(save_table, save_csv, csv_path,
                                 override_csv,
                                 database_connection),
        "actions_enzyme_drug" = enzymes_actions(save_table, save_csv,
                                                csv_path,
                                                override_csv,
                                                database_connection),
        "articles_enzyme_drug" = enzymes_articles(save_table,
                                                  save_csv,
                                                  csv_path,
                                                  override_csv,
                                                  database_connection),
        "links_enzyme_drug" = enzymes_links(save_table,
                                            save_csv,
                                            csv_path,
                                            override_csv,
                                            database_connection),
        "polypeptides_enzyme_drug" =
          enzymes_polypeptide(save_table, save_csv, csv_path,
                              override_csv,
                              database_connection),
        "enzy_poly_ext_identity" =
          enzymes_polypeptide_ext_ident(save_table,
                                        save_csv,
                                        csv_path,
                                        override_csv,
                                        database_connection),
        "enzy_poly_go" =
          enzymes_polypeptide_go(save_table, save_csv,
                                 csv_path,
                                 override_csv,
                                 database_connection),
        "pfams_polypeptides_enzyme_drug" =
          enzymes_polypeptide_pfams(save_table, save_csv, csv_path,
                                    override_csv,
                                    database_connection),
        "enzy_poly_syn" =
          enzymes_polypeptide_syn(save_table,
                                  save_csv, csv_path,
                                  override_csv,
                                  database_connection),
        "textbooks_enzyme_drug" = enzymes_textbooks(save_table,
                                                    save_csv,
                                                    csv_path,
                                                    override_csv,
                                                    database_connection),
        "experimental_properties_drug" =
          drug_exp_prop(save_table, save_csv, csv_path,
                        override_csv,
                        database_connection),
        "external_identifiers_drug" =
          drug_ex_identity(save_table, save_csv,
                           csv_path, override_csv,
                           database_connection),
        "external_links_drug" = drug_external_links(save_table, save_csv,
                                                    csv_path,
                                                    override_csv,
                                                    database_connection),
        "food_interactions_drug" = drug_food_interactions(save_table,
                                                          save_csv,
                                                          csv_path,
                                                          override_csv,
                                                          database_connection),
        "groups_drugs" = drug_groups(save_table, save_csv, csv_path,
                                     override_csv,
                                     database_connection),
        "interactions_drug" = drug_interactions(save_table, save_csv,
                                                csv_path, override_csv,
                                                database_connection),
        "links_drug" = drug_interactions(save_table, save_csv, csv_path,
                                         override_csv,
                                         database_connection),
        "manufacturers_drug" = drug_manufacturers(save_table, save_csv,
                                                  csv_path, override_csv,
                                                  database_connection),
        "mixtures_drug" = drug_mixtures(save_table, save_csv, csv_path,
                                        override_csv,
                                        database_connection),
        "packagers_drug" = drug_packagers(save_table, save_csv, csv_path,
                                          override_csv,
                                          database_connection),
        "patents_drugs" = drug_patents(save_table, save_csv, csv_path,
                                       override_csv,
                                       database_connection),
        "pathways_drug" = drug_pathway(save_table, save_csv, csv_path,
                                       override_csv,
                                       database_connection),
        "drugs_pathway_drug" = drug_pathway_drugs(save_table, save_csv,
                                                  csv_path, override_csv,
                                                  database_connection),
        "enzymes_pathway_drug" = drug_pathway_enzyme(save_table, save_csv,
                                                     csv_path,
                                                     override_csv,
                                                     database_connection),
        "pdb_entries_drug" = drug_pdb_entries(save_table, save_csv,
                                              csv_path, override_csv,
                                              database_connection),
        "prices_drug" = drug_prices(save_table, save_csv, csv_path,
                                    override_csv,
                                    database_connection),
        "products_drug" = drug_products(save_table, save_csv, csv_path,
                                        override_csv,
                                        database_connection),
        "reactions_drugs" = drug_reactions(save_table, save_csv, csv_path,
                                           override_csv,
                                           database_connection),
        "enzymes_reactions_drug" = drug_reactions_enzymes(save_table,
                                                          save_csv,
                                                          csv_path,
                                                          override_csv,
                                                          database_connection),
        "sequences_drug" = drug_sequences(save_table, save_csv, csv_path,
                                          override_csv,
                                          database_connection),
        "snp_adverse_reactions" =
          drug_snp_adverse_reactions(save_table,
                                      save_csv, csv_path,
                                      override_csv,
                                      database_connection),
        "snp_effects_drug" = drug_snp_effects(save_table, save_csv,
                                              csv_path, override_csv,
                                              database_connection),
        "syn_drug" = drug_syn(save_table, save_csv, csv_path,
                              override_csv,
                              database_connection),
        "targ_drug" = targets(save_table, save_csv, csv_path,
                              override_csv,
                              database_connection),
        "actions_target_drug" = targets_actions(save_table, save_csv,
                                                csv_path,
                                                override_csv,
                                                database_connection),
        "articles_target_drug" = targets_articles(save_table,
                                                  save_csv, csv_path,
                                                  override_csv,
                                                  database_connection),
        "links_target_drug" = targets_links(save_table, save_csv,
                                            csv_path, override_csv,
                                            database_connection),
        "polypeptide_target_drug" =
          targets_polypeptide(save_table, save_csv, csv_path,
                              override_csv,
                              database_connection),
        "targ_poly_ext_identity" =
          targets_polypeptide_ext_ident(save_table,
                                        save_csv,
                                        csv_path,
                                        override_csv,
                                        database_connection),
        "targ_poly_go" =
          targets_polypeptide_go(save_table,
                                 save_csv,
                                 csv_path,
                                 override_csv,
                                 database_connection),
        "pfams_polypeptide_target_drug" =
          targets_polypeptide_pfams(save_table,
                                    save_csv, csv_path,
                                    override_csv,
                                    database_connection),
        "targ_poly_syn" =
          targets_polypeptide(save_table,
                              save_csv, csv_path,
                              override_csv,
                              database_connection),
        "textbooks_target_drug" = targets_textbooks(save_table,
                                                    save_csv,
                                                    csv_path,
                                                    override_csv,
                                                    database_connection),
        "transporters_drug" = transporters(save_table, save_csv,
                                           csv_path, override_csv,
                                           database_connection),
        "actions_transporter_drug" =
          transporters_actions(save_table, save_csv, csv_path,
                               override_csv,
                               database_connection),
        "articles_transporter_drug" = targets_articles(save_table,
                                                       save_csv,
                                                       csv_path,
                                                       override_csv,
                                                       database_connection),
        "links_transporter_drug" = transporters_links(save_table,
                                                      save_csv,
                                                      csv_path,
                                                      override_csv,
                                                      database_connection),
        "polypeptides_transporter_drug" =
          enzymes_polypeptide(save_table, save_csv, csv_path,
                              override_csv,
                              database_connection),
        "trans_poly_ex_identity" =
          transporters_polypep_ex_ident(save_table, save_csv,
                                        csv_path, override_csv,
                                        database_connection),
        "go_polypeptide_trans_drug" =
          transporters_polypeptide_go(save_table,
                                      save_csv,
                                      csv_path,
                                      override_csv,
                                      database_connection),
        "trans_poly_pfams" =
          transporters_polypeptide_pfams(save_table,
                                         save_csv, csv_path,
                                         override_csv,
                                         database_connection),
        "trans_poly_syn" =
          transporters_polypeptide_syn(save_table,
                                       save_csv, csv_path,
                                       override_csv,
                                       database_connection),
        "textbooks_transporter_drug" =
          transporters_textbooks(save_table, save_csv,
                                 csv_path, override_csv,
                                 database_connection),
        "international_brands_drug" =
          drug_intern_brand(save_table, save_csv,
                            csv_path, override_csv,
                            database_connection),
        "salts_drug" = drug_salts(save_table, save_csv, csv_path,
                                  override_csv,
                                  database_connection),
        "culculated_properties_drug" =
          drug_calc_prop(save_table, save_csv,
                         csv_path, override_csv,
                         database_connection)
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
      "salts_drug"
    )
  return(elements_options)
}
