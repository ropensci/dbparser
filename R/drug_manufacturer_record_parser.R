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
  return(map_df(
    xmlChildren(rec[["manufacturers"]]), ~get_manufacturer_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}
