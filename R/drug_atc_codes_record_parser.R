# Extract drug atc-codes df
get_atc_codes_rec <- function(r, drug_key) {
  tibble(
    atc_code = xmlGetAttr(r, name = "code"),
    level_1 = xmlValue(r[[1]]),
    code_1 = xmlGetAttr(r[[1]], name = "code"),
    level_2 = xmlValue(r[[2]]),
    code_2 = xmlGetAttr(r[[2]], name = "code"),
    level_3 = xmlValue(r[[3]]),
    code_3 = xmlGetAttr(r[[3]], name = "code"),
    level_4 = xmlValue(r[[4]]),
    code_4 = xmlGetAttr(r[[4]], name = "code"),
    parent_key = drug_key
  )
}
get_atc_codes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["atc-codes"]]),
                 ~ get_atc_codes_rec(.x,
                                     xmlValue(rec["drugbank-id"][[1]]))))
}
