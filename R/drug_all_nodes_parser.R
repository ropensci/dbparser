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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return all drug elements dataframes
#'
#' @examples
#' parse_drug_all()
#' parse_drug_all(TRUE)
#' parse_drug_all(save_table = FALSE)
#' @export
parse_drug_all <- function(save_table = FALSE) {
  drugs <- parse_drug(save_table)
  print("Parsed drugs main attributes, 1/72")
  drug_groups <- parse_drug_groups(save_table)
  print("Parsed drug_groups, 2/72")
  drug_articles <- parse_drug_articles(save_table)
  print("Parsed drug_articles, 3/72")
  drug_books <- parse_drug_books(save_table)
  print("Parsed drug_books, 4/72")
  drug_links <- parse_drug_links(save_table)
  print("Parsed drug_links, 5/72")
  drug_synonyms <- parse_drug_synonyms(save_table)
  print("Parsed drug_synonyms, 6/72")
  drug_products <- parse_drug_products(save_table)
  print("Parsed drug_products, 7/72")
  drug_mixtures <- parse_drug_mixtures(save_table)
  print("Parsed drug_mixtures, 8/72")
  drug_packagers <- parse_drug_packagers(save_table)
  print("Parsed drug_packagers, 9/72")
  drug_categories <- parse_drug_categories(save_table)
  print("Parsed drug_categories, 10/72")
  drug_affected_organisms <-
    parse_drug_affected_organisms(save_table)
  print("Parsed drug_affected_organisms, 11/72")
  drug_dosages <- parse_drug_dosages(save_table)
  print("Parsed drug_dosages, 12/72")
  drug_ahfs_codes <- parse_drug_ahfs_codes(save_table)
  print("Parsed drug_ahfs_codes, 13/72")
  drug_pdb_entries <- parse_drug_pdb_entries(save_table)
  print("Parsed drug_pdb_entries, 14/72")
  drug_patents <- parse_drug_patents(save_table)
  print("Parsed drug_patents, 15/72")
  drug_food_interactions <- parse_drug_food_interactions(save_table)
  print("Parsed drug_food_interactions, 16/72")
  drug_interactions <- parse_drug_interactions(save_table)
  print("Parsed drug_interactions, 17/72")
  experimental_properties <-
    parse_drug_experimental_properties(save_table)
  print("Parsed drug_experimental_properties, 18/72")
  drug_external_identifiers <-
    parse_drug_experimental_properties(save_table)
  print("Parsed drug_external_identifiers, 19/72")
  drug_external_links <- parse_drug_external_links(save_table)
  print("Parsed drug_external_links, 20/72")
  drug_snp_effects <- parse_drug_snp_effects(save_table)
  print("Parsed drug_snp_effects, 21/72")
  drug_snp_adverse_drug_reactions <-
    parse_drug_snp_adverse_drug_reactions(save_table)
  print("Parsed drug_snp_adverse_drug_reactions, 22/72")
  drug_atc_codes <- parse_drug_atc_codes(save_table)
  print("Parsed drug_atc_codes, 23/72")
  drug_carriers_actions <- parse_drug_carriers_actions(save_table)
  print("Parsed drug_carriers_actions, 24/72")
  drug_carriers_articles <- parse_drug_carriers_articles(save_table)
  print("Parsed drug_carriers_articles, 25/72")
  drug_carriers_textbooks <-
    parse_drug_carriers_textbooks(save_table)
  print("Parsed drug_carriers_textbooks, 26/72")
  drug_carriers_links <- parse_drug_carriers_links(save_table)
  print("Parsed drug_carriers_links, 27/72")
  drug_carriers_polypeptides <-
    parse_drug_carriers_polypeptides(save_table)
  print("Parsed drug_carriers_polypeptides, 28/72")
  drug_carriers_polypeptides_external_identifiers <-
    parse_drug_carriers_polypeptides_external_identifiers(save_table)
  print("Parsed drug_carriers_polypeptides_external_identifiers, 29/72")
  drug_carriers_polypeptides_synonyms <-
    parse_drug_carriers_polypeptides_synonyms(save_table)
  print("Parsed drug_carriers_polypeptides_synonyms, 30/72")
  drug_carriers_polypeptides_pfams <-
    parse_drug_carriers_polypeptides_pfams(save_table)
  print("Parsed drug_carriers_polypeptides_pfams, 31/72")
  drug_carriers_polypeptides_go_classifiers <-
    parse_drug_carriers_polypeptides_go_classifiers(save_table)
  print("Parsed drug_carriers_polypeptides_go_classifiers, 32/72")
  drug_carriers <- parse_drug_carriers(save_table)
  print("Parsed drug_carriers, 33/72")
  classficationsparse_drug_classfications <-
    parse_drug_classfications(save_table)
  print("Parsed drugs main attributes, 34/72")
  drug_enzymes_actions <- parse_drug_enzymes_actions(save_table)
  print("Parsed drug_enzymes_actions, 35/72")
  drug_enzymes_articles <- parse_drug_enzymes_articles(save_table)
  print("Parsed drug_enzymes_articles, 36/72")
  drug_enzymes_textbooks <- parse_drug_enzymes_textbooks(save_table)
  print("Parsed drug_enzymes_textbooks, 37/72")
  drug_enzymes_links <- parse_drug_enzymes_links(save_table)
  print("Parsed drug_enzymes_links, 38/72")
  drug_enzymes_polypeptides <-
    parse_drug_enzymes_polypeptides(save_table)
  print("Parsed drug_enzymes_polypeptides, 39/72")
  drug_enzymes_polypeptides_external_identifiers <-
    parse_drug_enzymes_polypeptides_external_identifiers(save_table)
  print("Parsed drug_enzymes_polypeptides_external_identifiers, 40/72")
  drug_enzymes_polypeptides_synonyms <-
    parse_drug_enzymes_polypeptides_synonyms(save_table)
  print("Parsed drug_enzymes_polypeptides_synonyms, 41/72")
  drug_enzymes_polypeptides_pfams <-
    parse_drug_enzymes_polypeptides_pfams(save_table)
  print("Parsed drug_enzymes_polypeptides_pfams, 42/72")
  drug_enzymes_polypeptides_go_classifiers <-
    parse_drug_enzymes_polypeptides_go_classifiers(save_table)
  print("Parsed drug_enzymes_polypeptides_go_classifiers, 43/72")
  drug_enzymes <- parse_drug_enzymes(save_table)
  print("Parsed drug_enzymes, 44/72")
  drug_manufacturers <- parse_drug_manufacturers(save_table)
  print("Parsed drug_manufacturers, 45/72")
  drug_pathway_enzyme <- parse_drug_pathway_enzyme(save_table)
  print("Parsed drug_pathway_enzyme, 46/72")
  drug_pathway_drugs <- parse_drug_pathway_drugs(save_table)
  print("Parsed drug_pathway_drugs, 47/72")
  drug_pathway <- parse_drug_pathway(save_table)
  print("Parsed drug_pathway, 48/72")
  drug_prices <- parse_drug_prices(save_table)
  print("Parsed drug_prices, 49/72")
  drug_reactions <- parse_drug_reactions(save_table)
  print("Parsed drug_reactions, 50/72")
  drug_reactions_enzymes <- parse_drug_reactions_enzymes(save_table)
  print("Parsed drug_reactions_enzymes, 51/72")
  drug_sequences <- parse_drug_sequences(save_table)
  print("Parsed drug_sequences, 52/72")
  drug_targets_polypeptides_external_identifiers <-
    parse_drug_targets_polypeptides_external_identifiers(save_table)
  print("Parsed drug_targets_polypeptides_external_identifiers, 53/72")
  drug_targets_polypeptides_synonyms <-
    parse_drug_targets_polypeptides_synonyms(save_table)
  print("Parsed drug_targets_polypeptides_synonyms, 54/72")
  drug_targets_polypeptides_pfams <-
    parse_drug_targets_polypeptides_pfams(save_table)
  print("Parsed drug_targets_polypeptides_pfams, 55/72")
  drug_targets_polypeptides_go_classifiers <-
    parse_drug_targets_polypeptides_go_classifiers(save_table)
  print("Parsed drug_targets_polypeptides_go_classifiers attributes, 56/72")
  drug_targets_actions <- parse_drug_targets_actions(save_table)
  print("Parsed drug_targets_actions, 57/72")
  drug_targets_articles <- parse_drug_targets_articles(save_table)
  print("Parsed drug_targets_articles, 58/72")
  drug_targets_textbooks <- parse_drug_targets_textbooks(save_table)
  print("Parsed drug_targets_textbooks, 59/72")
  drug_targets_links <- parse_drug_targets_links(save_table)
  print("Parsed drug_targets_links, 60/72")
  drug_targets_polypeptide <-
    parse_drug_targets_polypeptides(save_table)
  print("Parsed drug_targets_polypeptide, 61/72")
  drug_targets <- parse_drug_targets(save_table)
  print("Parsed drug_targets, 62/72")
  drug_transporters_actions <-
    parse_drug_transporters_actions(save_table)
  print("Parsed drug_transporters_actions, 63/72")
  drug_transporters_articles <-
    parse_drug_transporters_articles(save_table)
  print("Parsed drug_transporters_articles, 64/72")
  drug_transporters_textbooks <-
    parse_drug_transporters_textbooks(save_table)
  print("Parsed drug_transporters_textbooks, 65/72")
  drug_transporters_links <-
    parse_drug_transporters_links(save_table)
  print("Parsed drug_transporters_links, 66/72")
  drug_transporters_polypeptides <-
    parse_drug_transporters_polypeptides(save_table)
  print("Parsed drug_transporters_polypeptides, 67/72")
  drug_transporters_polypeptides_external_identifiers <-
    parse_drug_transporters_polypeptides_external_identifiers(save_table)
  print("Parsed drug_transporters_polypeptides_external_identifiers, 68/72")
  drug_transporters_polypeptides_synonyms <-
    parse_drug_transporters_polypeptides_synonyms(save_table)
  print("Parsed drug_transporters_polypeptides_synonyms, 69/72")
  drug_transporters_polypeptides_pfams <-
    parse_drug_transporters_polypeptides_pfams(save_table)
  print("Parsed drug_transporters_polypeptides_pfams, 70/72")
  drug_transporters_polypeptides_go_classifiers <-
    parse_drug_transporters_polypeptides_go_classifiers(save_table)
  print("Parsed drugs_transporters_polypeptides_go_classifiers, 71/72")
  drug_transporters <- parse_drug_transporters(save_table)
  print("Parsed drugs_transporters, 72/72")
  return(
    list(
      drugs = drugs,
      drug_groups = drug_groups,
      drug_articles = drug_articles,
      drug_books = drug_books,
      drug_links = drug_links,
      drug_synonyms = drug_synonyms,
      drug_products = drug_products,
      drug_mixtures = drug_mixtures,
      drug_packagers = drug_packagers,
      drug_categories = drug_categories,
      drug_affected_organisms = drug_affected_organisms,
      drug_dosages = drug_dosages,
      drug_ahfs_codes = drug_ahfs_codes,
      drug_pdb_entries = drug_pdb_entries,
      drug_patents = drug_patents,
      drug_food_interactions = drug_food_interactions,
      drug_interactions = drug_interactions,
      experimental_properties = experimental_properties,
      drug_external_identifiers = drug_external_identifiers,
      drug_external_links = drug_external_links,
      drug_snp_effects = drug_snp_effects,
      drug_snp_adverse_drug_reactions = drug_snp_adverse_drug_reactions,
      drug_atc_codes = drug_atc_codes,
      drug_carriers_actions = drug_carriers_actions,
      drug_carriers_articles = drug_carriers_articles,
      drug_carriers_textbooks = drug_carriers_textbooks,
      drug_carriers_links = drug_carriers_links,
      drug_carriers_polypeptides = drug_carriers_polypeptides,
      drug_carriers_polypeptides_external_identifiers =
        drug_carriers_polypeptides_external_identifiers,
      drug_carriers_polypeptides_synonyms = drug_carriers_polypeptides_synonyms,
      drug_carriers_polypeptides_pfams = drug_carriers_polypeptides_pfams,
      drug_carriers_polypeptides_go_classifiers =
        drug_carriers_polypeptides_go_classifiers,
      drug_carriers = drug_carriers,
      classficationsparse_drug_classfications =
        classficationsparse_drug_classfications,
      drug_enzymes_actions = drug_enzymes_actions,
      drug_enzymes_articles = drug_enzymes_articles,
      drug_enzymes_textbooks = drug_enzymes_textbooks,
      drug_enzymes_links = drug_enzymes_links,
      drug_enzymes_polypeptides = drug_enzymes_polypeptides,
      drug_enzymes_polypeptides_external_identifiers =
        drug_enzymes_polypeptides_external_identifiers,
      drug_enzymes_polypeptides_synonyms = drug_enzymes_polypeptides_synonyms,
      drug_enzymes_polypeptides_pfams = drug_enzymes_polypeptides_pfams,
      drug_enzymes_polypeptides_go_classifiers = drug_enzymes_polypeptides_go_classifiers,
      drug_enzymes = drug_enzymes,
      drug_manufacturers = drug_manufacturers,
      drug_pathway_enzyme = drug_pathway_enzyme,
      drug_pathway_drugs = drug_pathway_drugs,
      drug_pathway = drug_pathway,
      drug_prices = drug_prices,
      drug_reactions = drug_reactions,
      drug_reactions_enzymes = drug_reactions_enzymes,
      drug_sequences = drug_sequences,
      drug_targets_polypeptides_external_identifiers =
        drug_targets_polypeptides_external_identifiers,
      drug_targets_polypeptides_synonyms = drug_targets_polypeptides_synonyms,
      drug_targets_polypeptides_pfams = drug_targets_polypeptides_pfams,
      drug_targets_polypeptides_go_classifiers =
        drug_targets_polypeptides_go_classifiers,
      drug_targets_actions = drug_targets_actions,
      drug_targets_articles = drug_targets_articles,
      drug_targets_textbooks = drug_targets_textbooks,
      drug_targets_links = drug_targets_links,
      drug_targets_polypeptide = drug_targets_polypeptide,
      drug_targets = drug_targets,
      drug_transporters_actions = drug_transporters_actions,
      drug_transporters_articles = drug_transporters_articles,
      drug_transporters_textbooks = drug_transporters_textbooks,
      drug_transporters_links = drug_transporters_links,
      drug_transporters_polypeptides = drug_transporters_polypeptides,
      drug_transporters_polypeptides_external_identifiers =
        drug_transporters_polypeptides_external_identifiers,
      drug_transporters_polypeptides_synonyms =
        drug_transporters_polypeptides_synonyms,
      drug_transporters_polypeptides_pfams = drug_transporters_polypeptides_pfams,
      drug_transporters_polypeptides_go_classifiers =
        drug_transporters_polypeptides_go_classifiers,
      drug_transporters = drug_transporters

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
#' no need to call it again before calling this funtion.
#'
#' parse_drug_element_options can be called to know the valid options for
#' this method
#'
#' @param save_table boolean, save table in database if true. Default is false.
#' @param elements_options list,  options of elements to be parsed. Default is "all"
#' @return list of selected drug elements dataframes
#'
#' @examples
#' parse_drug_element()
#' parse_drug_element(c("drug_ahfs_codes", "drug_carriers"), save_table = TRUE)
#' parse_drug_element(save_table = FALSE)
#' parse_drug_element(c("drug_ahfs_codes", "drug_carriers"))
#' @export
parse_drug_element <- function(elements_options = c("all"), save_table = FALSE) {
  if (!elements_options %in% parse_drug_element_options()) {
    message("Invalid options\nplease use parse_drug_element_options() to know valid options")
    return()
  }

  if ("all" %in% elements_options) {
    return(parse_drug_all(save_table = save_table))
  }
  parsed_list <- list()
  for (option in elements_options) {
    parsed_element <- swtich (
      option,
      "drugs" = parse_drug(save_table),
      "drug_affected_organisms" = parse_drug_affected_organisms(save_table),
      "drug_ahfs_codes" = parse_drug_ahfs_codes(save_table),
      "drug_articles" = parse_drug_articles(save_table),
      "drug_atc_codes" = parse_drug_atc_codes(save_table),
      "drug_books" = parse_drug_books(save_table),
      "drug_carriers" = parse_drug_carriers(save_table),
      "drug_carriers_actions" = parse_drug_carriers_actions(save_table),
      "drug_carriers_articles" = parse_drug_carriers_articles(save_table),
      "drug_carriers_links" = parse_drug_carriers_links(save_table),
      "drug_carriers_polypeptides" = parse_drug_carriers_polypeptides(save_table),
      "drug_carriers_polypeptides_external_identifiers" =
        parse_drug_carriers_polypeptides_external_identifiers(save_table),
      "drug_carriers_polypeptides_go_classifiers" =
        parse_drug_carriers_polypeptides_go_classifiers(save_table),
      "drug_carriers_polypeptides_pfams" = parse_drug_carriers_polypeptides_pfams(save_table),
      "drug_carriers_polypeptides_synonyms" = parse_drug_carriers_polypeptides_synonyms(save_table),
      "drug_carriers_textbooks" = parse_drug_carriers_textbooks(save_table),
      "drug_categories" = parse_drug_carriers(save_table),
      "drug_classfications" = parse_drug_classfications(save_table),
      "drug_dosages" = parse_drug_dosages(save_table),
      "drug_enzymes" = parse_drug_enzymes(save_table),
      "drug_enzymes_actions" = parse_drug_enzymes_actions(save_table),
      "drug_enzymes_articles" = parse_drug_enzymes_articles(save_table),
      "drug_enzymes_links" = parse_drug_enzymes_links(save_table),
      "drug_enzymes_polypeptides" = parse_drug_enzymes_polypeptides(save_table),
      "drug_enzymes_polypeptides_external_identifiers" =
        parse_drug_enzymes_polypeptides_external_identifiers(save_table),
      "drug_enzymes_polypeptides_go_classifiers" =
        parse_drug_enzymes_polypeptides_go_classifiers(save_table),
      "drug_enzymes_polypeptides_pfams" = parse_drug_enzymes_polypeptides_pfams(save_table),
      "drug_enzymes_polypeptides_synonyms" = parse_drug_enzymes_polypeptides_synonyms(save_table),
      "drug_enzymes_textbooks" = parse_drug_enzymes_textbooks(save_table),
      "drug_experimental_properties" = parse_drug_experimental_properties(save_table),
      "drug_external_identifiers" = parse_drug_external_identifiers(save_table),
      "drug_external_links" = parse_drug_external_links(save_table),
      "drug_food_interactions" = parse_drug_food_interactions(save_table),
      "drug_groups" = parse_drug_groups(save_table),
      "drug_interactions" = parse_drug_interactions(save_table),
      "drug_links" = parse_drug_interactions(save_table),
      "drug_manufacturers" = parse_drug_manufacturers(save_table),
      "drug_mixtures" = parse_drug_mixtures(save_table),
      "drug_packagers" = parse_drug_packagers(save_table),
      "drug_patents" = parse_drug_patents(save_table),
      "drug_pathway" = parse_drug_pathway(save_table),
      "drug_pathway_drugs" = parse_drug_pathway_drugsy(save_table),
      "drug_pathway_enzyme" = parse_drug_pathway_enzyme(save_table),
      "drug_pdb_entries" = parse_drug_pdb_entries(save_table),
      "drug_prices" = parse_drug_prices(save_table),
      "drug_products" = parse_drug_products(save_table),
      "drug_reactions" = parse_drug_reactions(save_table),
      "drug_reactions_enzymes" = parse_drug_reactions_enzymes(save_table),
      "drug_sequences" = parse_drug_sequences(save_table),
      "drug_snp_adverse_drug_reactions" = parse_drug_snp_adverse_drug_reactions(save_table),
      "drug_snp_effects" = parse_drug_snp_effects(save_table),
      "drug_synonyms" = parse_drug_synonyms(save_table),
      "drug_targets" = parse_drug_targets(save_table),
      "drug_targets_actions" = parse_drug_targets_actions(save_table),
      "drug_targets_articles" = parse_drug_targets_articles(save_table),
      "drug_targets_links" = parse_drug_targets_links(save_table),
      "drug_targets_polypeptides" = parse_drug_targets_polypeptides(save_table),
      "drug_targets_polypeptides_external_identifiers" =
        parse_drug_targets_polypeptides_external_identifiers(save_table),
      "drug_targets_polypeptides_go_classifiers" =
        parse_drug_targets_polypeptides_go_classifiers(save_table),
      "drug_targets_polypeptides_pfams" = parse_drug_targets_polypeptides_pfams(save_table),
      "drug_targets_polypeptides_synonyms" = parse_drug_targets_polypeptides_synonyms(save_table),
      "drug_targets_textbooks" = parse_drug_targets_textbooks(save_table),
      "drug_transporters" = parse_drug_transporters(save_table),
      "drug_transporters_actions" = parse_drug_transporters_actions(save_table),
      "drug_transporters_articles" = parse_drug_targets_articles(save_table),
      "drug_transporters_links" = parse_drug_transporters_links(save_table),
      "drug_transporters_polypeptides" = parse_drug_enzymes_polypeptides(save_table),
      "drug_transporters_polypeptides_external_identifiers" =
        parse_drug_transporters_polypeptides_external_identifiers(save_table),
      "drug_transporters_polypeptides_go_classifiers" =
        parse_drug_transporters_polypeptides_go_classifiers(save_table),
      "drug_transporters_polypeptides_pfams" = parse_drug_transporters_polypeptides_pfams(save_table),
      "drug_transporters_polypeptides_synonyms" = parse_drug_transporters_polypeptides_synonyms(save_table),
      "drug_transporters_textbooks" = parse_drug_transporters_textbooks(save_table)
    )
    parsed_list <- append(parsed_list, parsed_element)
    print(paste("Parsed", option))
  }
  return(parsed_list)
}

#' Returns \code{parse_drug_element} valid options.
#'
#' @return list of \code{parse_drug_element} valid options
#'
#' @examples
#' parse_drug_element_options()
#' @export
parse_drug_element_options <- function() {
  elements_options <-
    c(
      "all",
      "drugs",
      "drug_affected_organisms",
      "drug_ahfs_codes",
      "drug_articles",
      "drug_atc_codes",
      "drug_books",
      "drug_carriers",
      "drug_carriers_actions",
      "drug_carriers_articles",
      "drug_carriers_links",
      "drug_carriers_polypeptides",
      "drug_carriers_polypeptides_external_identifiers",
      "drug_carriers_polypeptides_go_classifiers",
      "drug_carriers_polypeptides_pfams",
      "drug_carriers_polypeptides_synonyms",
      "drug_carriers_textbooks",
      "drug_categories",
      "drug_classfications",
      "drug_dosages",
      "drug_enzymes",
      "drug_enzymes_actions",
      "drug_enzymes_articles",
      "drug_enzymes_links",
      "drug_enzymes_polypeptides",
      "drug_enzymes_polypeptides_external_identifiers",
      "drug_enzymes_polypeptides_go_classifiers",
      "drug_enzymes_polypeptides_pfams",
      "drug_enzymes_polypeptides_synonyms",
      "drug_enzymes_textbooks",
      "drug_experimental_properties",
      "drug_external_identifiers",
      "drug_external_links",
      "drug_food_interactions",
      "drug_groups",
      "drug_interactions",
      "drug_links",
      "drug_manufacturers",
      "drug_mixtures",
      "drug_packagers",
      "drug_patents",
      "drug_pathway",
      "drug_pathway_drugs",
      "drug_pathway_enzyme",
      "drug_pdb_entries",
      "drug_prices",
      "drug_products",
      "drug_reactions",
      "drug_reactions_enzymes",
      "drug_sequences",
      "drug_snp_adverse_drug_reactions",
      "drug_snp_effects",
      "drug_synonyms",
      "drug_targets",
      "drug_targets_actions",
      "drug_targets_articles",
      "drug_targets_links",
      "drug_targets_polypeptides",
      "drug_targets_polypeptides_external_identifiers",
      "drug_targets_polypeptides_go_classifiers",
      "drug_targets_polypeptides_pfams",
      "drug_targets_polypeptides_synonyms",
      "drug_targets_textbooks",
      "drug_transporters",
      "drug_transporters_actions",
      "drug_transporters_articles",
      "drug_transporters_links",
      "drug_transporters_polypeptides",
      "drug_transporters_polypeptides_external_identifiers",
      "drug_transporters_polypeptides_go_classifiers",
      "drug_transporters_polypeptides_pfams",
      "drug_transporters_polypeptides_synonyms",
      "drug_transporters_textbooks"
    )
  return(elements_options)
}
