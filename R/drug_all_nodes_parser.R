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
  drugs <- parse_drug(save_table)
  print("Parsed drugs main attributes, 1/74")
  drug_groups <- parse_drug_groups(save_table)
  print("Parsed drug_groups, 2/74")
  drug_articles <- parse_drug_articles(save_table)
  print("Parsed drug_articles, 3/74")
  drug_books <- parse_drug_books(save_table)
  print("Parsed drug_books, 4/74")
  drug_links <- parse_drug_links(save_table)
  print("Parsed drug_links, 5/74")
  drug_synonyms <- parse_drug_synonyms(save_table)
  print("Parsed drug_synonyms, 6/74")
  drug_products <- parse_drug_products(save_table)
  print("Parsed drug_products, 7/74")
  drug_mixtures <- parse_drug_mixtures(save_table)
  print("Parsed drug_mixtures, 8/74")
  drug_packagers <- parse_drug_packagers(save_table)
  print("Parsed drug_packagers, 9/74")
  drug_categories <- parse_drug_categories(save_table)
  print("Parsed drug_categories, 10/74")
  drug_affected_organisms <-
    parse_drug_affected_organisms(save_table)
  print("Parsed drug_affected_organisms, 11/74")
  drug_dosages <- parse_drug_dosages(save_table)
  print("Parsed drug_dosages, 12/74")
  drug_ahfs_codes <- parse_drug_ahfs_codes(save_table)
  print("Parsed drug_ahfs_codes, 13/74")
  drug_pdb_entries <- parse_drug_pdb_entries(save_table)
  print("Parsed drug_pdb_entries, 14/74")
  drug_patents <- parse_drug_patents(save_table)
  print("Parsed drug_patents, 15/74")
  drug_food_interactions <- parse_drug_food_interactions(save_table)
  print("Parsed drug_food_interactions, 16/74")
  drug_interactions <- parse_drug_interactions(save_table)
  print("Parsed drug_interactions, 17/74")
  drug_experimental_properties <-
    parse_drug_experimental_properties(save_table)
  print("Parsed drug_experimental_properties, 18/74")
  drug_external_identifiers <-
    parse_drug_experimental_properties(save_table)
  print("Parsed drug_external_identifiers, 19/74")
  drug_external_links <- parse_drug_external_links(save_table)
  print("Parsed drug_external_links, 20/74")
  drug_snp_effects <- parse_drug_snp_effects(save_table)
  print("Parsed drug_snp_effects, 21/74")
  drug_snp_adverse_drug_reactions <-
    parse_drug_snp_adverse_drug_reactions(save_table)
  print("Parsed drug_snp_adverse_drug_reactions, 22/74")
  drug_atc_codes <- parse_drug_atc_codes(save_table)
  print("Parsed drug_atc_codes, 23/74")
  drug_carriers_actions <- parse_drug_carriers_actions(save_table)
  print("Parsed drug_carriers_actions, 24/74")
  drug_carriers_articles <- parse_drug_carriers_articles(save_table)
  print("Parsed drug_carriers_articles, 25/74")
  drug_carriers_textbooks <-
    parse_drug_carriers_textbooks(save_table)
  print("Parsed drug_carriers_textbooks, 26/74")
  drug_carriers_links <- parse_drug_carriers_links(save_table)
  print("Parsed drug_carriers_links, 27/74")
  drug_carriers_polypeptides <-
    parse_drug_carriers_polypeptides(save_table)
  print("Parsed drug_carriers_polypeptides, 28/74")
  drug_carriers_polypeptides_external_identifiers <-
    parse_drug_carriers_polypeptides_external_identifiers(save_table)
  print("Parsed drug_carriers_polypeptides_external_identifiers, 29/74")
  drug_carriers_polypeptides_synonyms <-
    parse_drug_carriers_polypeptides_synonyms(save_table)
  print("Parsed drug_carriers_polypeptides_synonyms, 30/74")
  drug_carriers_polypeptides_pfams <-
    parse_drug_carriers_polypeptides_pfams(save_table)
  print("Parsed drug_carriers_polypeptides_pfams, 31/74")
  drug_carriers_polypeptides_go_classifiers <-
    parse_drug_carriers_polypeptides_go_classifiers(save_table)
  print("Parsed drug_carriers_polypeptides_go_classifiers, 32/74")
  drug_carriers <- parse_drug_carriers(save_table)
  print("Parsed drug_carriers, 33/74")
  drug_classification <-
    parse_drug_classification(save_table)
  print("Parsed drugs main attributes, 34/74")
  drug_enzymes_actions <- parse_drug_enzymes_actions(save_table)
  print("Parsed drug_enzymes_actions, 35/74")
  drug_enzymes_articles <- parse_drug_enzymes_articles(save_table)
  print("Parsed drug_enzymes_articles, 36/74")
  drug_enzymes_textbooks <- parse_drug_enzymes_textbooks(save_table)
  print("Parsed drug_enzymes_textbooks, 37/74")
  drug_enzymes_links <- parse_drug_enzymes_links(save_table)
  print("Parsed drug_enzymes_links, 38/74")
  drug_enzymes_polypeptides <-
    parse_drug_enzymes_polypeptides(save_table)
  print("Parsed drug_enzymes_polypeptides, 39/74")
  drug_enzymes_polypeptides_external_identifiers <-
    parse_drug_enzymes_polypeptides_external_identifiers(save_table)
  print("Parsed drug_enzymes_polypeptides_external_identifiers, 40/74")
  drug_enzymes_polypeptides_synonyms <-
    parse_drug_enzymes_polypeptides_synonyms(save_table)
  print("Parsed drug_enzymes_polypeptides_synonyms, 41/74")
  drug_enzymes_polypeptides_pfams <-
    parse_drug_enzymes_polypeptides_pfams(save_table)
  print("Parsed drug_enzymes_polypeptides_pfams, 42/74")
  drug_enzymes_polypeptides_go_classifiers <-
    parse_drug_enzymes_polypeptides_go_classifiers(save_table)
  print("Parsed drug_enzymes_polypeptides_go_classifiers, 43/74")
  drug_enzymes <- parse_drug_enzymes(save_table)
  print("Parsed drug_enzymes, 44/74")
  drug_manufacturers <- parse_drug_manufacturers(save_table)
  print("Parsed drug_manufacturers, 45/74")
  drug_pathway_enzyme <- parse_drug_pathway_enzyme(save_table)
  print("Parsed drug_pathway_enzyme, 46/74")
  drug_pathway_drugs <- parse_drug_pathway_drugs(save_table)
  print("Parsed drug_pathway_drugs, 47/74")
  drug_pathway <- parse_drug_pathway(save_table)
  print("Parsed drug_pathway, 48/74")
  drug_prices <- parse_drug_prices(save_table)
  print("Parsed drug_prices, 49/74")
  drug_reactions <- parse_drug_reactions(save_table)
  print("Parsed drug_reactions, 50/74")
  drug_reactions_enzymes <- parse_drug_reactions_enzymes(save_table)
  print("Parsed drug_reactions_enzymes, 51/74")
  drug_sequences <- parse_drug_sequences(save_table)
  print("Parsed drug_sequences, 52/74")
  drug_targets_polypeptides_external_identifiers <-
    parse_drug_targets_polypeptides_external_identifiers(save_table)
  print("Parsed drug_targets_polypeptides_external_identifiers, 53/74")
  drug_targets_polypeptides_synonyms <-
    parse_drug_targets_polypeptides_synonyms(save_table)
  print("Parsed drug_targets_polypeptides_synonyms, 54/74")
  drug_targets_polypeptides_pfams <-
    parse_drug_targets_polypeptides_pfams(save_table)
  print("Parsed drug_targets_polypeptides_pfams, 55/74")
  drug_targets_polypeptides_go_classifiers <-
    parse_drug_targets_polypeptides_go_classifiers(save_table)
  print("Parsed drug_targets_polypeptides_go_classifiers attributes, 56/74")
  drug_targets_actions <- parse_drug_targets_actions(save_table)
  print("Parsed drug_targets_actions, 57/74")
  drug_targets_articles <- parse_drug_targets_articles(save_table)
  print("Parsed drug_targets_articles, 58/74")
  drug_targets_textbooks <- parse_drug_targets_textbooks(save_table)
  print("Parsed drug_targets_textbooks, 59/74")
  drug_targets_links <- parse_drug_targets_links(save_table)
  print("Parsed drug_targets_links, 60/74")
  drug_targets_polypeptide <-
    parse_drug_targets_polypeptides(save_table)
  print("Parsed drug_targets_polypeptide, 61/74")
  drug_targets <- parse_drug_targets(save_table)
  print("Parsed drug_targets, 62/74")
  drug_transporters_actions <-
    parse_drug_transporters_actions(save_table)
  print("Parsed drug_transporters_actions, 63/74")
  drug_transporters_articles <-
    parse_drug_transporters_articles(save_table)
  print("Parsed drug_transporters_articles, 64/74")
  drug_transporters_textbooks <-
    parse_drug_transporters_textbooks(save_table)
  print("Parsed drug_transporters_textbooks, 65/74")
  drug_transporters_links <-
    parse_drug_transporters_links(save_table)
  print("Parsed drug_transporters_links, 66/74")
  drug_transporters_polypeptides <-
    parse_drug_transporters_polypeptides(save_table)
  print("Parsed drug_transporters_polypeptides, 67/74")
  drug_transporters_polypeptides_external_identifiers <-
    parse_drug_transporters_polypeptides_external_identifiers(save_table)
  print("Parsed drug_transporters_polypeptides_external_identifiers, 68/74")
  drug_transporters_polypeptides_synonyms <-
    parse_drug_transporters_polypeptides_synonyms(save_table)
  print("Parsed drug_transporters_polypeptides_synonyms, 69/74")
  drug_transporters_polypeptides_pfams <-
    parse_drug_transporters_polypeptides_pfams(save_table)
  print("Parsed drug_transporters_polypeptides_pfams, 70/74")
  drug_transporters_polypeptides_go_classifiers <-
    parse_drug_transporters_polypeptides_go_classifiers(save_table)
  print("Parsed drugs_transporters_polypeptides_go_classifiers, 71/74")
  drug_transporters <- parse_drug_transporters(save_table)
  print("Parsed drugs_transporters, 72/74")
  drug_international_brands <- parse_drug_international_brands(save_table)
  print("Parsed drugs_international_brands, 73/74")
  drug_salts <- parse_drug_salts(save_table)
  print("Parsed drugs_international_brands, 74/74")
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
      drug_experimental_properties = drug_experimental_properties,
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
      drug_classification =
      drug_classification,
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
      drug_transporters = drug_transporters,
      drug_international_brands = drug_international_brands,
      drug_salts = drug_salts

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
    message("Invalid options\nplease use parse_drug_element_options() to know valid options")
    return()
  }

  if ("all" %in% elements_options) {
    return(parse_drug_all(save_table = save_table))
  }
  parsed_list <- list()
  for (option in elements_options) {
    parsed_element <- switch (
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
      "drug_classification" = parse_drug_classification(save_table),
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
      "drug_pathway_drugs" = parse_drug_pathway_drugs(save_table),
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
      "drug_transporters_textbooks" = parse_drug_transporters_textbooks(save_table),
      "drug_international_brands" = parse_drug_international_brands(save_table),
      "drug_salts" = parse_drug_salts(save_table)
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
      "drugs",
      "drug_affected_organisms",
      "drug_ahfs_codes",
      "drug_articles",
      "drug_atc_codes",
      "drug_books",
      "drug_carriers",
      "drug_carriers_actions",
      "drug_carriers_articles",
      "drug_carriers_links",      "drug_carriers_polypeptides",

      "drug_carriers_polypeptides_external_identifiers",
      "drug_carriers_polypeptides_go_classifiers",
      "drug_carriers_polypeptides_pfams",
      "drug_carriers_polypeptides_synonyms",
      "drug_carriers_textbooks",
      "drug_categories",
      "drug_classification",
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
      "drug_transporters_textbooks",
      "drug_international_brands",
      "drug_salts"
    )
  return(elements_options)
}
