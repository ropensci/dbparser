# Extract drug sequences df
get_sequence_rec <- function(r, drug_key) {
  tibble(
    sequence = xmlValue(r),
    format = ifelse(
      is.null(xmlGetAttr(r, name = "format")),
      NA,
      xmlGetAttr(r, name = "format")
    ),
    parent_key = drug_key
  )
}

get_sequences_df <- function(rec) {
  if (is.null(rec[["sequences"]]))
    return()
  return(map_df(
    xmlChildren(rec[["sequences"]]),
    ~ get_sequence_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
  ))
}
