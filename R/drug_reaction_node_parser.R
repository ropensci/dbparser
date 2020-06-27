# Extract drug reactions df
get_reactions_rec <- function(r, drug_key) {
  tibble(
    sequence = xmlValue(r[["sequence"]]),
    left_drugbank_id = xmlValue(r[["left-element"]][["drugbank-id"]]),
    left_drugbank_name = xmlValue(r[["left-element"]][["name"]]),
    right_drugbank_id = xmlValue(r[["right-element"]][["drugbank-id"]]),
    right_drugbank_name = xmlValue(r[["right-element"]][["name"]]),
    parent_key = drug_key
  )
}
get_reactions_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["reactions"]]),
    ~ get_reactions_rec(., xmlValue(rec[["drugbank-id"]]))
  ))
}

# Extract drug reactions enzymes df
get_reactions_enzymes_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["reactions"]]),
    ~ drug_sub_df(., "enzymes", id = NULL)
  ))
}

#' Drug Reactions Parsers
#'
#' Extract the sequential representation of the metabolic reactions that this
#'  drug molecule is involved in. Depending on available information, this may
#'  include metabolizing enzymes, reaction type, substrates, products,
#'  pharmacological activity of metabolites, and a structural representation of
#'   the biochemical reactions.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 5 variables:
#' \describe{
#'   \item{sequence}{	Reactions are displayed within a numerical sequence}
#'   \item{left_drugbank_name}{The substrate of the reaction. Maybe a drug or a
#'    metabolite.}
#'   \item{rightt_drugbank_name}{	The product of the reaction. Maybe a drug or a
#'    metabolite.}
#'   \item{left_drugbank_id}{}
#'   \item{right_drugbank_id}{}
#'   \item{parent_id}{drugbank id}
#' }
#' @family drugs
#' @inherit drug_all examples
#' @export
drug_reactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_reactions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_reactions <- readr::read_csv(path)
    } else {
      drug_reactions <-
        map_df(pkg_env$children, ~ get_reactions_df(.x)) %>%
        unique()

      write_csv(drug_reactions, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_reactions,
        table_name = "drug_reactions",
        foreign_key = "parent_key"
      )
    }
    return(drug_reactions %>% as_tibble())
  }

#' Drug Reactions Enzymes Parsers
#'
#' EEnzymes involved in metabolizing this drug
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{name}{}
#'   \item{uniprot-id}{}
#'   \item{parent_id}{drugbank id}
#' }
#' @family drugs
#' @inherit drug_all examples
#' @export
drug_reactions_enzymes <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_reactions_enzymes", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_reactions_enzymes <- readr::read_csv(path)
    } else {
      drug_reactions_enzymes <-
        map_df(pkg_env$children, ~ get_reactions_enzymes_df(.x)) %>%
        unique()

      write_csv(drug_reactions_enzymes, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_reactions_enzymes,
        table_name = "drug_reactions_enzymes",
        save_table_only = TRUE
      )
    }
    return(drug_reactions_enzymes %>% as_tibble())
  }
