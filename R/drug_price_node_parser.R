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
#' @param save_csv boolean, save csv version of parsed dataframe if true
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug prices node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_prices()
#'
#' # save in database and return parsed dataframe
#' parse_drug_prices(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_prices(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_prices(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_prices(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_prices(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_prices <- function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_prices", csv_path)
  if (override_csv & file.exists(path)) {
    return(readr::read_csv(path))
  }

  drug_prices <-
    map_df(pkg.env$children, ~ get_prices_df(.x)) %>%
    unique()

  write_csv(drug_prices, save_csv, csv_path)

  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_prices,
                  table_name = "drug_prices")
  }
  return(drug_prices)
}
