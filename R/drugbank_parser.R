#' parseDrugBank
#'
#'  parses given DrugBank XML database into a dvobject. dvobject is a list of
#'  data.frames in which each data.frame represents a part of parsed data (i.e
#'  drugs, prices, carriers, ...)
#'
#' @param db_path \strong{string}, full path for the \strong{DrugBank} xml or
#'  zip file.
#' @param drug_options \strong{character vector}, list of sub drug related nodes
#' names options to parse (default = NULL). Check \code{drug_node_options()}
#' for all available options. If its value is `NULL` ONLY `drug_general_information`
#' will be placed in the returned dvobject.
#' @param parse_salts \strong{boolean}, parse salts info (default = FALSE)
#' @param parse_products  \strong{boolean}, parse products info (default = FALSE)
#' @param references_options \strong{character vector}, list of sub references
#' related nodes names options to parse (default = NULL).
#' Check \code{references_node_options()} for all available options.
#' @param cett_options \strong{character vector}, list of sub cett related nodes
#' names options to parse (default = NULL). Check \code{cett_nodes_options()}
#' for all available options.
#'
#' @return dvobject
#' @family parsers
#' @export
parseDrugBank <- function(db_path,
                          drug_options       = NULL,
                          parse_salts        = FALSE,
                          parse_products     = FALSE,
                          references_options = NULL,
                          cett_options       = NULL) {
  if (all(!is.null(drug_options),
          !is.na(drug_options),
          (!all(drug_options %in% drug_node_options())))) {
    message(paste("Options: '", paste(setdiff(drug_options,
                                              drug_node_options()),
                                      collapse = ", "),
                  "' are invalid, setting 'drug_options' to default value"))
    drug_options <- NULL
  }


  if (all(!is.null(references_options),
          !is.na(references_options),
          (!all(references_options %in% references_node_options())))) {
    message(paste("Options: '", paste(setdiff(references_options,
                                              references_node_options()),
                                      collapse = ", "),
                  "' are invalid, setting 'references_options' to default value"))
    references_options <- NULL
  }

  if (all(!is.null(cett_options),
          !is.na(cett_options),
          (!all(cett_options %in% cett_nodes_options())))) {
    message(paste("Options: '", paste(setdiff(cett_options,
                                              cett_nodes_options()),
                                      collapse = ", "),
                  "' are invalid, setting 'cett_options' to default value"))
    cett_options <- NULL
  }

  if ((length(parse_salts) > 1) ||
      is.na(parse_salts) ||
      !is.logical(parse_salts)) {
    message("'parse_salts' must have single logical value. Setting 'parse_salts' to default value")
    parse_salts <- FALSE
  }

  if ((length(parse_products) > 1) ||
      is.na(parse_products) ||
      !is.logical(parse_products)) {
    message("'parse_products' must have single logical value. Setting 'parse_products' to default value")
    parse_products <- FALSE
  }

  dvobject  <- init_dvobject()
  parsed_db <- read_drugbank_xml_db(db_path = db_path)

  if (!is.null(parsed_db)) {
    message("Completed loading DrugBank DB into memory")
    message("...........................................")
    pkg_env$root <- XML::xmlRoot(parsed_db)
    dvobject     <- add_database_info(dvobject         = dvobject,
                                      db_version       = XML::xmlGetAttr(
                                        node = pkg_env$root,
                                        name = "version"),
                                      db_exported_date = XML::xmlGetAttr(
                                        node = pkg_env$root,
                                        name = "exported-on"))
    message("parsing drugs elements")

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



#' returns drug node valid options.
#'
#' @return list of drug valid options
#' @family parsers
#' @export
drug_node_options <- function() {
  c("drug_classification", "synonyms", "pharmacology", "international_brands",
    "mixtures", "packagers", "manufacturers", "prices", "categories", "dosages",
    "atc_codes", "patents", "reactions", "sequences","calculated_properties",
    "experimental_properties", "external_identifiers", "external_links",
    "pathway", "drug_interactions", "snp_effects", "groups", "pdb_entries",
    "ahfs_codes", "snp_adverse_reactions", "food_interactions",
     "affected_organisms")
}


#' returns references node valid options.
#'
#' @return list of references valid options
#' @family parsers
#' @export
references_node_options <- function() {
  c("drug_books", "drug_articles", "drug_links", "drug_attachments",
    "carrier_books", "carrier_articles", "carrier_links", "carrier_attachments",
    "enzyme_books", "enzyme_articles", "enzyme_links", "enzyme_attachments",
    "target_books", "target_articles", "target_links", "target_attachments",
    "transporter_books", "transporter_articles", "transporter_links",
    "transporter_attachments")
}

#' returns carriers, enzymes,targets and transporters node valid options.
#'
#' @return list of CETT valid options
#' @family parsers
#' @export
cett_nodes_options <- function() {
  c("carriers",
    "enzymes",
    "targets",
    "transporters")
}

#' Run all drug  related parsers
#'
#' Run all parsers that retrieve drugs related information
#'
#' @param drug_options - list of options to parse
#'
#' @return a list of all drugs parsed tibbles
#'
#' @keywords internal
parse_drug_nodes <- function(drug_options) {
  drugs <- list()
  message("drugs information parsing has started")
  message("parsing drugs general information")
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
    drugs[["experimental_properties"]] <- drug_exp_prop()
  }

  if ("external_identifiers" %in% drug_options) {
    message("parsing drugs experimental properties")
    drugs[["external_identifiers"]] <- drug_ex_identity()
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
#' @return  a list of all references for drugs, carriers, enzymes, targets or
#' transporters
#'
#' @keywords internal
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
#' @keywords internal
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
