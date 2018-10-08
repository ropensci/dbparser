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
  return(map_df(xmlChildren(rec[["reactions"]]),
                ~ get_reactions_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug reactions enzymes df
get_reactions_enzymes_df <- function(rec) {
  return(map_df(xmlChildren(rec[["reactions"]]),
                ~ drug_sub_df(.x, "enzymes", id = NULL)))
}

#' Extracts the drug reactions element and return data as data frame.
#'
#' \code{parse_drug_reactions} returns data frame of drug reactions elements.
#'
#' This functions extracts the groups element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug reactions node attributs date frame
#'
#' @examples
#' parse_drug_reactions()
#' parse_drug_reactions(FALSE)
#' parse_drug_reactions(save_table = FALSE)
#' @export
parse_drug_reactions <- function(save_table = TRUE) {
  drug_reactions <- map_df(children, ~ get_reactions_df(.x))
  if (save_table) {
    save_drug_sub(
      con = con,
      df = drug_reactions,
      table_name = "drug_reactions",
      foreign_key = "drug_key"
    )
  }
  return(drug_reactions)
}

#' Extracts the drug reactions enzymes element and return data as data frame.
#'
#' \code{parse_drug_reactions_enzymes} returns data frame of drug reactions enzymes elements.
#'
#' This functions extracts the reactions enzymes element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug reactions enzymes node attributs date frame
#'
#' @examples
#' parse_drug_reactions_enzymes()
#' parse_drug_reactions_enzymes(FALSE)
#' parse_drug_reactions_enzymes(save_table = FALSE)
#' @export
parse_drug_reactions_enzymes <- function(save_table = TRUE) {
  drug_reactions_enzymes <-
    map_df(children, ~ get_reactions_enzymes_df(.x))
  if (save_table) {
    save_drug_sub(
      con = con,
      df = drug_reactions_enzymes,
      table_name = "drug_reactions_enzymes",
      save_table_only = TRUE
    )
  }
  return(drug_reactions_enzymes)
}
