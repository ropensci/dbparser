# Extract drug sequences df
get_sequence_rec <- function(r, drug_key) {
  tibble(
    sequence = xmlValue(r),
    format = ifelse(is.null(xmlGetAttr(r, name = "format")),
                    NA,
                    xmlGetAttr(r, name = "format")),
    parent_key = drug_key
  )
}

get_sequences_df <- function(rec) {
  if (is.null(rec[["sequences"]]))
    return()
  return(map_df(xmlChildren(rec[["sequences"]]),
                ~ get_sequence_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}


#' Extracts the drug sequences element and return data as data frame.
#'
#' \code{parse_drug_sequences} returns data frame of drug sequences elements.
#'
#' This functions extracts the sequences element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug sequences node attributs date frame
#'
#' @examples
#' parse_drug_sequences()
#' parse_drug_sequences(TRUE)
#' parse_drug_sequences(save_table = FALSE)
#' @export
parse_drug_sequences <- function(save_table = FALSE) {
  drug_sequences <- map_df(pkg.env$children, ~ get_sequences_df(.x))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_sequences,
                  table_name = "drug_sequences")
  }
  return(drug_sequences)
}
