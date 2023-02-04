#' parseDrugBank
#'  parses given DrugBank XML database into a dvobject. dvobject is a list of
#'  data.frames in which each data.frame represents a part of parsed data (i.e
#'  drugs, prices, carriers, ...)
#'
#' @param db_path \strong{string}, full path for the
#'  \strong{DrugBank} xml or zip file.
#'
#' @return dvobject
#' @family parsers
#' @export
parseDrugBank <- function(db_path,
                          drug_options       = drug_node_options(),
                          parse_salts        = TRUE,
                          parse_products     = TRUE,
                          references_options = references_node_options(),
                          cett_options       = c("carriers",
                                                 "enzymes",
                                                 "targets",
                                                 "transporters")) {
  dvobject  <- init_dvobject()
  parsed_db <- read_drugbank_xml_db(db_path = db_path)

  if (!is.null(parsed_db)) {
    message("Completed loading DrugBank DB into memory")
    message("...........................................")
    pkg_env$root <- XML::xmlRoot(parsed_db)
    dvobject     <- add_drugbank_info(dvobject  = dvobject)
    message("Parsing Drugs elements")
    dvobject[["drugs"]] <- parse_drug_nodes(drug_options)

    if(parse_salts) {
      message("...........................................")
      message("parsing drugs salts")
      dvobject[["salts"]] <- drug_salts()
    }

    if(parse_products) {
      message("...........................................")
      message("parsing drugs products")
      dvobject[["products"]] <- drug_products()
    }

    if(length(references_options) > 0) {
      message("...........................................")
      message("parsing references")
      dvobject[["references"]] <- parse_references_node(references_options)
    }

    if (length(cett_options) > 0) {
      message("...........................................")
      dvobject[["cett"]] <- parse_cett_node(cett_options)
    }
  }

  dvobject
}

#' extracts the all drug elements and return data as list of tibbles.
#'
#' this functions extracts all element of drug nodes in \strong{DrugBank}
#' xml database.
#'
#' @return all drug elements tibbles
#' @family common
#' @examples
#' \dontrun{
#' # the same parameters and usage will be applied for any parser
#' # return only the parsed tibble
#' run_all_parsers()
#'
#' }
#' @family collective_parsers
#' @export
run_all_parsers <- function() {
    c(drugs(),
      references(),
      cett())
  }

#' extracts the given drug elements and return data as list of tibbles.
#'
#' \code{drug_element} returns list of tibbles of <- selected
#' elements.
#'
#' drug_element_options can be called to know the valid options for
#' this method
#'
#' @param elements_options list,  options of elements to be parsed. default is
#'  "all"
#'
#' @return list of selected drug elements tibbles
#' @family common
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_element()
#' drug_element(c("drug_ahfs_codes", "drug_carriers"))
#' }
#' @keywords internal
#' @export
drug_element <-
  function(elements_options = c("all")) {
    if (!all(elements_options %in% drug_element_options())) {
      stop("invalid options\nplease use drug_element_options() to
           know valid options")
    }

    if ("all" %in% elements_options) {
      return(run_all_parsers())
    }
    parsed_list <- list()
    for (option in elements_options) {
      parsed_element <- switch(
        option,
        "drugs" = drug_general_information(),
        "pharmacology_drug" = drug_pharmacology(),
        "affected_organisms_drug" =
          drug_affected_organisms(),
        "ahfs_codes_drug" = drug_ahfs_codes(),
        "articles_drug" = drugs_articles(),
        "atc_codes_drug" = drug_atc_codes(),
        "books_drug" = drugs_textbooks(),
        "carriers_drug" = carriers(),
        "actions_carrier_drug" = carriers_actions(),
        "articles_carrier_drug" = carriers_articles(),
        "links_carrier_drug" = carriers_links(),
        "polypeptides_carrier_drugs" = carriers_polypeptides(),
        "carr_poly_ext_identity" = carriers_polypep_ex_ident(),
        "carr_polypeptides_go" = carriers_polypeptides_go(),
        "carr_polypeptides_pfams" = carriers_polypeptides_pfams(),
        "carr_polypeptides_syn" = carriers_polypeptides_syn(),
        "textbooks_carrier_drug" = carriers_textbooks(),
        "categories_drug" = carriers(),
        "classifications_drug" = drug_classification(),
        "dosages_drug" = drug_dosages(),
        "enzymes_drug" = enzymes(),
        "actions_enzyme_drug" = enzymes_actions(),
        "articles_enzyme_drug" = enzymes_articles(),
        "links_enzyme_drug" = enzymes_links(),
        "polypeptides_enzyme_drug" = enzymes_polypeptides(),
        "enzy_poly_ext_identity" = enzymes_polypep_ex_ident(),
        "enzy_poly_go" = enzymes_polypeptides_go(),
        "pfams_polypeptides_enzyme_drug" = enzymes_polypeptides_pfams(),
        "enzy_poly_syn" = enzymes_polypeptides_syn(),
        "textbooks_enzyme_drug" = enzymes_textbooks(),
        "experimental_properties_drug" = drug_exp_prop(),
        "external_identifiers_drug" = drug_ex_identity(),
        "external_links_drug" = drug_external_links(),
        "food_interactions_drug" = drug_food_interactions(),
        "groups_drugs" = drug_groups(),
        "interactions_drug" = drug_interactions(),
        "links_drug" = drug_interactions(),
        "manufacturers_drug" = drug_manufacturers(),
        "mixtures_drug" = drug_mixtures(),
        "packagers_drug" = drug_packagers(),
        "patents_drugs" = drug_patents(),
        "pathways_drug" = drug_pathway(),
        "drugs_pathway_drug" = drug_pathway_drugs(),
        "enzymes_pathway_drug" = drug_pathway_enzyme(),
        "pdb_entries_drug" = drug_pdb_entries(),
        "prices_drug" = drug_prices(),
        "products_drug" = drug_products(),
        "reactions_drugs" = drug_reactions(),
        "enzymes_reactions_drug" = drug_reactions_enzymes(),
        "sequences_drug" = drug_sequences(),
        "snp_adverse_reactions" = drug_snp_adverse_reactions(),
        "snp_effects_drug" = drug_snp_effects(),
        "syn_drug" = drug_syn(),
        "targ_drug" = targets(),
        "actions_target_drug" = targets_actions(),
        "articles_target_drug" = targets_articles(),
        "links_target_drug" = targets_links(),
        "polypeptide_target_drug" = targets_polypeptides(),
        "targ_poly_ext_identity" = targets_polypep_ex_ident(),
        "targ_poly_go" = targets_polypeptides_go(),
        "pfams_polypeptide_target_drug" = targets_polypeptides_pfams(),
        "targ_poly_syn" = targets_polypeptides(),
        "textbooks_target_drug" = targets_textbooks(),
        "transporters_drug" = transporters(),
        "actions_transporter_drug" = transporters_actions(),
        "articles_transporter_drug" = targets_articles(),
        "links_transporter_drug" = transporters_links(),
        "polypeptides_transporter_drug" = enzymes_polypeptides(),
        "trans_poly_ex_identity" = transporters_polypep_ex_ident(),
        "go_polypeptide_trans_drug" = transporters_polypeptides_go(),
        "trans_poly_pfams" = transporters_polypeptides_pfams(),
        "trans_poly_syn" = transporters_polypeptides_syn(),
        "textbooks_transporter_drug" = transporters_textbooks(),
        "international_brands_drug" = drug_intern_brand(),
        "salts_drug" = drug_salts(),
        "culculated_properties_drug" = drug_calc_prop(),
        "attachments_drug" = drugs_attachments(),
        "attachments_carrier" = carriers_attachments(),
        "attachments_enzyme" = enzymes_attachments(),
        "attachments_target" = targets_attachments(),
        "attachments_transporter" = transporters_attachments(),
        "references" = references()
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
}

#' returns durg node valid options.
#'
#' @return list of \code{drug_element} valid options
#' @family common
#' @examples
#' \dontrun{
#' drug_node_options()
#' }
#' @keywords internal
#' @export
drug_node_options <- function() {
  c("drug_classification", "synonyms", "pharmacology", "international_brands",
    "mixtures", "packagers", "manufacturers", "prices", "categories", "dosages",
    "atc_codes", "patents", "interactions", "sequences","calculated_properties",
    "experimental_properties", "external_identifiers", "external_links",
    "pathway", "drug_interactions", "snp_effects", "groups", "pdb_entries",
    "ahfs_codes", "snp_adverse_reactions", "food_interactions",
     "affected_organisms")
}


#' returns references node valid options.
#'
#' @return list of \code{drug_element} valid options
#' @family common
#' @examples
#' \dontrun{
#' references_node_options()
#' }
#' @keywords internal
#' @export
references_node_options <- function() {
  c("drug_books", "drug_articles", "drug_links", "drug_attachments",
    "carrier_books", "carrier_articles", "carrier_links", "carrier_attachments",
    "enzyme_books", "enzyme_articles", "enzyme_links", "enzyme_attachments",
    "target_books", "target_articles", "target_links", "target_attachments",
    "transporter_books", "transporter_articles", "transporter_links",
    "transporter_attachments")
}

#' Run all drug  related parsers
#'
#' Run all parsers that retrieve drugs related information
#'
#' @param drug_options
#'
#' @return a list of all drugs parsed tibbles
#'
#' @family collective_parsers
#'
#' @inherit run_all_parsers examples
#' @export
parse_drug_nodes <- function(drug_options) {
  drugs <- list()
  message("Drugs Information Parsing has Started")
  message("parsing drugs gneral information")
  drugs[["general_information"]] <- drug_general_information()

  if ("drug_classification" %in% drug_options) {
    message("parsing drugs classifications")
    drugs[["drug_classification"]] <- drug_classification()
  }

  if ("synonyms" %in% drug_options) {
    message("parsing drugs synonyms")
    drugs[["synonyms"]] <- drug_syn()
  }

  if ("pharmacology" %in% drug_options) {
    message("parsing drugs pharmacology")
    drugs[["pharmacology"]] <- drug_pharmacology()
  }

  if ("international_brands" %in% drug_options) {
    message("parsing drugs international brands")
    drugs[["international_brands"]] <- drug_intern_brand()
  }

  if ("mixtures" %in% drug_options) {
    message("parsing drugs mixtures")
    drugs[["mixtures"]] <- drug_mixtures()
  }

  if ("packagers" %in% drug_options) {
    message("parsing drugs packagers")
    drugs[["packagers"]] <- drug_packagers()
  }

  if ("manufacturers" %in% drug_options) {
    message("parsing drugs manufacturers")
    drugs[["manufacturers"]] <- drug_manufacturers()
  }

  if ("prices" %in% drug_options) {
    message("parsing drugs prices")
    drugs[["prices"]] <- drug_prices()
  }

  if ("categories" %in% drug_options) {
    message("parsing drugs categories")
    drugs[["categories"]] <- drug_categories()
  }

  if ("dosages" %in% drug_options) {
    message("parsing drugs dosages")
    drugs[["dosages"]] <- drug_dosages()
  }

  if ("atc_codes" %in% drug_options) {
    message("parsing drugs atc_codes")
    drugs[["atc_codes"]] <- drug_atc_codes()
  }

  if ("patents" %in% drug_options) {
    message("parsing drugs patents")
    drugs[["patents"]] <- drug_patents()
  }

  if ("drug_interactions" %in% drug_options) {
    message("parsing drugs interactions")
    drugs[["drug_interactions"]] <- drug_interactions()
  }

  if ("sequences" %in% drug_options) {
    message("parsing drugs sequences")
    drugs[["sequences"]] <- drug_sequences()
  }

  if ("calculated_properties" %in% drug_options) {
    message("parsing drugs calculated properties")
    drugs[["calculated_properties"]] <- drug_calc_prop()
  }

  if ("experimental_properties" %in% drug_options) {
    message("parsing drugs external identity")
    drugs[["experimental_properties"]] <- drug_ex_identity()
  }

  if ("external_identifiers" %in% drug_options) {
    message("parsing drugs experimental properties")
    drugs[["external_identifiers"]] <- drug_exp_prop()
  }

  if ("pathway" %in% drug_options) {
    pathway <- list()
    message("parsing drugs pathway general information")
    pathway[["general_information"]] <- drug_pathway()

    message("parsing drugs pathway drugs")
    pathway[["pathway_drugs"]] <- drug_pathway_drugs()

    message("parsing drugs pathway enzyme")
    pathway[["pathway_enzyme"]] <- drug_pathway_enzyme()

    drugs[["pathway"]] <- pathway
  }

  if ("reactions" %in% drug_options) {
    reactions <- list()
    message("parsing drugs reactions general information")
    reactions[["general_information"]] <- drug_reactions()

    message("parsing drugs reactions enzymes")
    reactions[["reactions_enzymes"]] <- drug_reactions_enzymes()

    drugs[["reactions"]] <- reactions
  }

  if ("snp_effects" %in% drug_options) {
    message("parsing drugs snp effects")
    drugs[["snp_effects"]] <- drug_snp_effects()
  }

  if ("snp_adverse_reactions" %in% drug_options) {
    message("parsing drugs snp adverse reactions")
    drugs[["snp_adverse_reactions"]] <- drug_snp_adverse_reactions()
  }

  if ("food_interactions" %in% drug_options) {
    message("parsing drugs food interaction")
    drugs[["food_interactions"]] <- drug_food_interactions()
  }

  if ("pdb_entries" %in% drug_options) {
    message("parsing drugs pdb entries")
    drugs[["pdb_entries"]] <- drug_pdb_entries()
  }

  if ("ahfs_codes" %in% drug_options) {
    message("parsing drugs ahfs codes")
    drugs[["ahfs_codes"]] <- drug_ahfs_codes()
  }

  if ("affected_organisms" %in% drug_options) {
    message("parsing drugs affected organisms")
    drugs[["affected_organisms"]] <- drug_affected_organisms()
  }

  if ("groups" %in% drug_options) {
    message("parsing drugs groups")
    drugs[["groups"]] <- drug_groups()
  }

  if ("external_links" %in% drug_options) {
    message("parsing drugs external links")
    drugs[["external_links"]] <- drug_external_links()
  }

  drugs
}


#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters references element parser
#'
#' Return a list of all references for drugs, carriers, enzymes, targets or
#' transporters
#'
#' @family references
#'
#' @inherit run_all_parsers examples
#' @export
parse_references_node <- function(references_options) {
  references   <- list()
  drugs        <- list()
  carriers     <- list()
  enzymes      <- list()
  targets      <- list()
  transporters <- list()

  if ("drug_books" %in% references_options) {
    message("parsing drugs textbooks")
    drugs[["books"]] <- drugs_textbooks()
  }

  if ("drug_articles" %in% references_options) {
    message("parsing drugs articles")
    drugs[["articles"]] <- drugs_articles()
  }

  if ("drug_links" %in% references_options) {
    message("parsing drugs links")
    drugs[["links"]] <- drugs_links()
  }

  if ("drug_attachments" %in% references_options) {
    message("parsing drugs attachments")
    drugs[["attachments"]] <- drugs_attachments()
  }

  if ("carrier_books" %in% references_options) {
    message("parsing carriers textbooks")
    carriers[["books"]] <- carriers_textbooks()
  }

  if ("carrier_articles" %in% references_options) {
    message("parsing carriers articles")
    carriers[["articles"]] <- carriers_articles()
  }

  if ("carrier_links" %in% references_options) {
    message("parsing carriers links")
    carriers[["links"]] <- carriers_links()
  }

  if ("carrier_attachments" %in% references_options) {
    message("parsing carriers attachments")
    carriers[["attachments"]] <- carriers_attachments()
  }

  if ("enzyme_books" %in% references_options) {
    message("parsing enzymes textbooks")
    enzymes[["books"]] <- enzymes_textbooks()
  }

  if ("enzyme_articles" %in% references_options) {
    message("parsing enzymes articles")
    enzymes[["articles"]] <- enzymes_articles()
  }

  if ("enzyme_links" %in% references_options) {
    message("parsing enzymes links")
    enzymes[["links"]] <- enzymes_links()
  }

  if ("enzyme_attachments" %in% references_options) {
    message("parsing enzymes attachments")
    enzymes[["attachments"]] <- enzymes_attachments()
  }

  if ("target_books" %in% references_options) {
    message("parsing targets textbooks")
    targets[["books"]] <- targets_textbooks()
  }

  if ("target_articles" %in% references_options) {
    message("parsing targets articles")
    targets[["articles"]] <- targets_articles()
  }

  if ("target_links" %in% references_options) {
    message("parsing targets links")
    targets[["links"]] <- targets_links()
  }

  if ("target_attachments" %in% references_options) {
    message("parsing targets attachments")
    targets[["attachments"]] <- targets_attachments()
  }

  if ("transporter_books" %in% references_options) {
    message("parsing transporters textbooks")
    transporters[["books"]] <- transporters_textbooks()
  }

  if ("transporter_articles" %in% references_options) {
    message("parsing transporters articles")
    transporters[["articles"]] <- transporters_articles()
  }

  if ("transporter_links" %in% references_options) {
    message("parsing transporters links")
    transporters[["links"]] <- transporters_links()
  }

  if ("transporter_attachments" %in% references_options) {
    message("parsing transporters attachments")
    transporters[["attachments"]] <- transporters_attachments()
  }

  if (length(drugs) > 0) {
    references[["drugs"]] <- drugs
  }

  if (length(carriers) > 0) {
    references[["carriers"]] <- carriers
  }

  if (length(enzymes) > 0) {
    references[["enzymes"]] <- enzymes
  }

  if (length(targets) > 0) {
    references[["targets"]] <- targets
  }

  if (length(transporters) > 0) {
    references[["transporters"]] <- transporters
  }

  message("Extracting references is over!")
  references
}

#' Run all CETT related parsers
#'
#' Run all parsers that retrieve carriers, enzymes, targets and transporters
#'  related information
#'
#' @return  a list of all drugs parsed tibbles
#'
#' @family collective_parsers
#'
#' @inherit run_all_parsers examples
#' @export
parse_cett_node <- function(cett_options) {
  cett         <- list()
  carriers     <- list()
  enzymes      <- list()
  targets      <- list()
  transporters <- list()

  message("CETT Information Parsing has Started")

  if ("carriers" %in% cett_options) {
    polypepeptide <- list()

    message("parsing carriers general information")
    carriers[["general_information"]] <- carriers()

    message("parsing carriers actions")
    carriers[["actions"]] <- carriers_actions()

    message("parsing carriers polypeptides general information")
    polypepeptide[["general_information"]] <- carriers_polypeptides()

    message("parsing carriers polypepeptide external identy")
    polypepeptide[["external_identy"]] <- carriers_polypep_ex_ident()

    message("parsing carriers polypeptides synonyms")
    polypepeptide[["synonyms"]] <- carriers_polypeptides_syn()

    message("parsing carriers polypeptides pfams")
    polypepeptide[["pfams"]] <- carriers_polypeptides_pfams()

    message("parsing carriers polypeptides go")
    polypepeptide[["go"]] <- carriers_polypeptides_go()

    carriers[["polypeptides"]] <- polypepeptide
    cett[["carriers"]]         <- carriers
  }

  if ("enzymes" %in% cett_options) {
    polypepeptide <- list()

    message("parsing enzymes general information")
    enzymes[["general_information"]] <- enzymes()

    message("parsing enzymes actions")
    enzymes[["actions"]] <- enzymes_actions()

    message("parsing enzymes polypeptides general information")
    polypepeptide[["general_information"]] <- enzymes_polypeptides()

    message("parsing enzymes polypepeptide external identy")
    polypepeptide[["external_identy"]] <- enzymes_polypep_ex_ident()

    message("parsing enzymes polypeptides synonyms")
    polypepeptide[["synonyms"]] <- enzymes_polypeptides_syn()

    message("parsing enzymes polypeptides pfams")
    polypepeptide[["pfams"]] <- enzymes_polypeptides_pfams()

    message("parsing enzymes polypeptides go")
    polypepeptide[["go"]] <- enzymes_polypeptides_go()

    enzymes[["polypeptides"]] <- polypepeptide
    cett[["enzymes"]]         <- enzymes
  }

  if ("targets" %in% cett_options) {
    polypepeptide <- list()

    message("parsing targets general information")
    targets[["general_information"]] <- targets()

    message("parsing targets actions")
    targets[["actions"]] <- targets_actions()

    message("parsing targets polypeptides general information")
    polypepeptide[["general_information"]] <- targets_polypeptides()

    message("parsing targets polypepeptide external identy")
    polypepeptide[["external_identy"]] <- targets_polypep_ex_ident()

    message("parsing targets polypeptides synonyms")
    polypepeptide[["synonyms"]] <- targets_polypeptides_syn()

    message("parsing targets polypeptides pfams")
    polypepeptide[["pfams"]] <- targets_polypeptides_pfams()

    message("parsing targets polypeptides go")
    polypepeptide[["go"]] <- targets_polypeptides_go()

    targets[["polypeptides"]] <- polypepeptide
    cett[["targets"]]         <- targets
  }

  if ("transporters" %in% cett_options) {
    polypepeptide <- list()

    message("parsing transporters general information")
    transporters[["general_information"]] <- transporters()

    message("parsing transporters actions")
    transporters[["actions"]] <- transporters_actions()

    message("parsing transporters polypeptides general information")
    polypepeptide[["general_information"]] <- transporters_polypeptides()

    message("parsing transporters polypepeptide external identy")
    polypepeptide[["external_identy"]] <- transporters_polypep_ex_ident()

    message("parsing transporters polypeptides synonyms")
    polypepeptide[["synonyms"]] <- transporters_polypeptides_syn()

    message("parsing transporters polypeptides pfams")
    polypepeptide[["pfams"]] <- transporters_polypeptides_pfams()

    message("parsing transporters polypeptides go")
    polypepeptide[["go"]] <- transporters_polypeptides_go()

    transporters[["polypeptides"]] <- polypepeptide
    cett[["transporters"]]         <- transporters
  }

  cett
}
