# Extract drug sequences df
get_sequence_rec <- function(r, drug_key) {
  tibble::tibble(
    sequence = XML::xmlValue(r),
    format = ifelse(
      is.null(XML::xmlGetAttr(r, name = "format")),
      NA,
      XML::xmlGetAttr(r, name = "format")
    ),
    parent_key = drug_key
  )
}

get_sequences_df <- function(rec) {
  if (is.null(rec[["sequences"]]))
    return()
  return(purr::map_df(
    XML::xmlChildren(rec[["sequences"]]),
    ~ get_sequence_rec(.x, XML::xmlValue(rec["drugbank-id"][[1]]))
  ))
}
