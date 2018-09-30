# Extract drug reactions df
get_reactions_rec <- function(r, drug_key) {
    tibble(sequence = xmlValue(r[["sequence"]]), left_drugbank_id = xmlValue(r[["left-element"]][["drugbank-id"]]), left_drugbank_name = xmlValue(r[["left-element"]][["name"]]), 
        right_drugbank_id = xmlValue(r[["right-element"]][["drugbank-id"]]), right_drugbank_name = xmlValue(r[["right-element"]][["name"]]), 
        parent_key = drug_key)
}
get_reactions_df <- function(rec) {
    return(map_df(xmlChildren(rec[["reactions"]]), ~get_reactions_rec(., xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug reactions enzymes df
get_reactions_enzymes_df <- function(rec) {
    return(map_df(xmlChildren(rec[["reactions"]]), ~drug_sub_df(.x, "enzymes", id = NULL)))
}
