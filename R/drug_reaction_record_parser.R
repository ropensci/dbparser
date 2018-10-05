# Extract drug reactions df
get_reactions_rec <- function(r, drug_key) {
  tibble::tibble(
    sequence = XML::xmlValue(r[["sequence"]]),
    left_drugbank_id = XML::xmlValue(r[["left-element"]][["drugbank-id"]]),
    left_drugbank_name = XML::xmlValue(r[["left-element"]][["name"]]),
    right_drugbank_id = XML::xmlValue(r[["right-element"]][["drugbank-id"]]),
    right_drugbank_name = XML::xmlValue(r[["right-element"]][["name"]]),
    parent_key = drug_key
  )
}
get_reactions_df <- function(rec) {
  return(purrr::map_df(
    XML::xmlChildren(rec[["reactions"]]),
    ~ get_reactions_rec(., XML::xmlValue(rec["drugbank-id"][[1]]))
  ))
}

# Extract drug reactions enzymes df
get_reactions_enzymes_df <- function(rec) {
  return(purrr::map_df(
    XML::xmlChildren(rec[["reactions"]]),
    ~ drug_sub_df(.x, "enzymes", id = NULL)
  ))
}
