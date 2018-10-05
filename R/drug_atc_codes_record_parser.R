get_atc_codes_rec <- function(r, drug_key) {
  tibble(
    atc_code = XML::xmlGetAttr(r, name = "code"),
    level_1 = XML::xmlValue(r[[1]]),
    code_1 = XML::xmlGetAttr(r[[1]], name = "code"),
    level_2 = XML::xmlValue(r[[2]]),
    code_2 = XML::xmlGetAttr(r[[2]], name = "code"),
    level_3 = XML::xmlValue(r[[3]]),
    code_3 = XML::xmlGetAttr(r[[3]], name = "code"),
    level_4 = XML::xmlValue(r[[4]]),
    code_4 = XML::xmlGetAttr(r[[4]], name = "code"),
    parent_key = drug_key
  )
}

get_atc_codes_df <- function(rec) {
  return (purrr::map_df(xmlChildren(rec[["atc-codes"]]),
                 ~ get_atc_codes_rec(.x,
                                     XML::xmlValue(rec["drugbank-id"][[1]]))))
}
