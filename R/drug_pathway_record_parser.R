# Extract drug pathways df
get_pathway_rec <- function(r, drug_key) {
  tibble(
    smpdb_id = xmlValue(r[["smpdb-id"]]),
    name = xmlValue(r[["name"]]),
    category = xmlValue(r[["category"]]),
    parent_key = drug_key)
}

get_pathways_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["pathways"]]),
    ~get_pathway_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug pathways drugs df
get_pathways_drugs_df <- function(rec) {
  return(map_df(
      xmlChildren(rec[["pathways"]]),
      ~drug_sub_df(.x, "drugs", id = "smpdb-id")))
}

# Extract drug pathways enzymes df
get_pathways_enzymes_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["pathways"]]),
    ~drug_sub_df(.x, "enzymes", id = "smpdb-id")))
}
