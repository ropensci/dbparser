# Extract drug manufacturers df
get_manufacturer_rec <- function(r, drug_key) {
  tibble(
    name = xmlValue(r),
    url = xmlGetAttr(r, name = "url"),
    generic = xmlGetAttr(r, name = "generic"),
    parent_key = drug_key
  )
}

get_manufactures_df <- function(rec) {
  return(map_df(xmlChildren(rec[["manufacturers"]]), ~ get_manufacturer_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

#' Extracts the drug manufacturers element and return data as data frame.
#'
#' \code{parse_drug_manufacturers} returns data frame of drug manufacturers elements.
#'
#' This functions extracts the manufacturers element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug manufacturers node attributs date frame
#'
#' @examples
#' parse_drug_manufacturers()
#' parse_drug_manufacturers(FALSE)
#' parse_drug_manufacturers(save_table = FALSE)
#' @export
parse_drug_manufacturers <- function(save_table = TRUE) {
  drug_manufacturers <-
    map_df(children, ~ drug_sub_df(.x, "manufacturers"))
  if (save_table) {
    save_drug_sub(con = con,
                  df = drug_manufacturers,
                  table_name = "drug_manufacturers")
  }
  return(drug_manufacturers)
}
