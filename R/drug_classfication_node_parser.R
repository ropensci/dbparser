# Extract drug classifications df
drug_classifications_df <- function(rec) {
  if (is.null(rec[["classification"]]))
    return()
  a <- xmlToList(rec[["classification"]])
  return(tibble(
    parent_key = xmlValue(rec["drugbank-id"][[1]]),
    classifications = paste(names(a), a, collapse = ";")
  ))
}

#' Extracts the drug classifications element and return data as data frame.
#'
#' \code{parse_drug_classification} returns data frame of drug classifications elements.
#'
#' This functions extracts the classifications element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug classifications node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_classification()
#' parse_drug_classification(TRUE)
#' parse_drug_classification(save_table = FALSE)
#' }
#' @export
parse_drug_classification <- function(save_table = FALSE) {
  drug_classifications <- map_df(pkg.env$children, ~ drug_classifications_df(.x))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_classifications,
                  table_name = "drug_classifications")
  }
  return(drug_classifications)
}
