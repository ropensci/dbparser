# Extract drug prices df
get_price_rec <- function(r, drug_key) {
  tibble::tibble(
    description = XML::xmlValue(r[["description"]]),
    currency = XML::xmlGetAttr(r[["cost"]], name = "currency"),
    cost = XML::xmlValue(r[["cost"]]),
    unit = XML::xmlValue(r[["unit"]]),
    parent_key = drug_key
  )
}

get_prices_df <- function(rec) {
  return(purrr::map_df(XML::xmlChildren(rec[["prices"]]), ~ get_price_rec(., XML::xmlValue(rec["drugbank-id"][[1]]))))
}
