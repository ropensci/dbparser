get_synonym_rec <- function(rec, parent_key) {
  return(
    tibble::tibble(
      parent_key = parent_key,
      synonym = XML::xmlValue(rec),
      language = XML::xmlGetAttr(rec, name = "language"),
      coder = xmlGetAttr(rec,
                         name = "coder")
    )
  )
}

get_synonyms_df <- function(rec) {
  return(purrr::map_df(
    XML::xmlChildren(rec[["synonyms"]]),
    ~ get_synonym_rec(.x, XML::xmlValue(rec["drugbank-id"][[1]]))
  ))
}
