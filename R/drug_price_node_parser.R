# Extract drug prices df
get_price_rec <- function(r, drug_key) {
  tibble_row(
    description = xmlValue(r[["description"]]),
    currency = xmlGetAttr(r[["cost"]], name = "currency"),
    cost = xmlValue(r[["cost"]]),
    unit = xmlValue(r[["unit"]]),
    parent_key = drug_key
  )
}

get_prices_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["prices"]]),
    ~ get_price_rec(., xmlValue(rec[["drugbank-id"]]))
  ))
}

#' Drug Prices Parsers
#'
#' Collection of related actions
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 5 variables:
#' \describe{
#'   \item{description}{}
#'   \item{cost}{}
#'   \item{currency}{}
#'   \item{unit}{}
#'   \item{parent_id}{drugbank id}
#' }
#' @family drugs
#' @inherit drug_all examples
#' @export
drug_prices <- function(save_table = FALSE, save_csv = FALSE,
                              csv_path = ".", override_csv = FALSE,
                        database_connection = NULL) {
  check_parameters_validation(save_table, database_connection)
  path <-
    get_dataset_full_path("drug_prices", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_prices <- readr::read_csv(path)
  } else {
    drug_prices <-
      map_df(pkg_env$children, ~ get_prices_df(.x)) %>%
      unique()

    write_csv(drug_prices, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(
      con = database_connection,
      df = drug_prices,
      table_name = "drug_prices"
    )
  }
  return(drug_prices)
}
