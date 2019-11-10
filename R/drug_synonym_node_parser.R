get_synonym_rec <- function(rec, parent_key) {
  return(
    tibble(
      parent_key = parent_key,
      synonym = xmlValue(rec),
      language = xmlGetAttr(rec, name = "language"),
      coder = xmlGetAttr(rec,
                         name = "coder")
    )
  )
}

get_syn_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["synonyms"]]),
    ~ get_synonym_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
  ))
}
