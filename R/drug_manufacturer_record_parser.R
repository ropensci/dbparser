# Extract drug manufacturers df
get_manufacturer_rec <- function(r, drug_key) {
  tibble::tibble(
    name = XML::xmlValue(r),
    url = XML::xmlGetAttr(r, name = "url"),
    generic = XML::xmlGetAttr(r, name = "generic"),
    parent_key = drug_key
    )
}
get_manufactures_df <- function(rec) {
  return(purrr::map_df(
    XML::xmlChildren(rec[["manufacturers"]]), ~get_manufacturer_rec(., XML::xmlValue(rec["drugbank-id"][[1]]))))
}
