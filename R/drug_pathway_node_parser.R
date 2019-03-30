# Extract drug pathways df
get_pathway_rec <- function(r, drug_key) {
  tibble(
    smpdb_id = xmlValue(r[["smpdb-id"]]),
    name = xmlValue(r[["name"]]),
    category = xmlValue(r[["category"]]),
    parent_key = drug_key
  )
}

get_pathways_df <- function(rec) {
  return(map_df(xmlChildren(rec[["pathways"]]),
                ~ get_pathway_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug pathways drugs df
get_pathways_drugs_df <- function(rec) {
  return(map_df(xmlChildren(rec[["pathways"]]),
                ~ drug_sub_df(.x, "drugs", id = "smpdb-id")))
}

# Extract drug pathways enzymes df
get_pathways_enzymes_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["pathways"]]),
    ~ drug_sub_df(.x, "enzymes", id = "smpdb-id")
  ))
}

#' Extracts the drug pathway enzyme element and return data as data frame.
#'
#' \code{parse_drug_pathway_enzyme} returns data frame of drug pathway enzyme elements.
#'
#' This functions extracts the pathway enzyme element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug pathway enzyme node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_pathway_enzyme()
#' parse_drug_pathway_enzyme(TRUE)
#' parse_drug_pathway_enzyme(save_table = FALSE)
#' }
#' @export
parse_drug_pathway_enzyme <- function(save_table = FALSE) {
  drug_pathway_enzymes <-
    map_df(pkg.env$children, ~ get_pathways_enzymes_df(.x))

  if (nrow(drug_pathway_enzymes) > 0) {
    colnames(drug_pathway_enzymes) <- c("enzyme", "pathway_id")
  }

  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_pathway_enzymes,
      table_name = "drug_pathway_enzyme",
      save_table_only = TRUE
    )
  }
  return(tibble::as_tibble(drug_pathway_enzymes))
}

#' Extracts the drug pathway drugs element and return data as data frame.
#'
#' \code{parse_drug_pathway_drugs} returns data frame of drug pathway drugs elements.
#'
#' This functions extracts the pathway drugs element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug pathway drugs node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_pathway_drugs()
#' parse_drug_pathway_drugs(TRUE)
#' parse_drug_pathway_drugs(save_table = FALSE)
#' }
#' @export
parse_drug_pathway_drugs <- function(save_table = FALSE) {
  drug_pathway_drugs <- map_df(pkg.env$children, ~ get_pathways_drugs_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_pathway_drugs,
      table_name = "drug_pathway_drugs",
      save_table_only = TRUE
    )
  }
  return(tibble::as_tibble(drug_pathway_drugs))
}

#' Extracts the drug pathway element and return data as data frame.
#'
#' \code{parse_drug_pathway} returns data frame of drug pathway elements.
#'
#' This functions extracts the groups element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug pathway node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_pathway()
#' parse_drug_pathway(TRUE)
#' parse_drug_pathway(save_table = FALSE)
#' }
#' @export
parse_drug_pathway <- function(save_table = FALSE) {
  drug_pathway <- map_df(pkg.env$children, ~ get_pathways_df(.x))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_pathway,
                  table_name = "drug_pathway")
  }
  return(drug_pathway)
}
