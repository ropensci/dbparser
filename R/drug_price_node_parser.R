# Extract drug prices df
get_price_rec <- function(r, drug_key) {
  tibble(
    description = xmlValue(r[["description"]]),
    currency = xmlGetAttr(r[["cost"]], name = "currency"),
    cost = xmlValue(r[["cost"]]),
    unit = xmlValue(r[["unit"]]),
    parent_key = drug_key
  )
}

get_prices_df <- function(rec) {
  return(map_df(xmlChildren(rec[["prices"]]), ~ get_price_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

#' Extracts the drug prices element and return data as data frame.
#'
#' \code{parse_drug_prices} returns data frame of drug prices elements.
#'
#' This functions extracts the prices element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug prices node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_prices()
#' parse_drug_prices(TRUE)
#' parse_drug_prices(save_table = FALSE)
#' }
#' @export
parse_drug_prices <- function(save_table = FALSE) {
  drug_prices <-
    map_df(pkg.env$children, ~ get_prices_df(.x)) %>%
    unique()

  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_prices,
                  table_name = "drug_prices")
  }
  return(drug_prices)
}
