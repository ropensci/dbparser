# Extract drug prices df
get_price_rec <- function(r, drug_key) {
    tibble(description = xmlValue(r[["description"]]), currency = xmlGetAttr(r[["cost"]], name = "currency"), cost = xmlValue(r[["cost"]]), 
        unit = xmlValue(r[["unit"]]), parent_key = drug_key)
}
get_prices_df <- function(rec) {
    return(map_df(xmlChildren(rec[["prices"]]), ~get_price_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}
