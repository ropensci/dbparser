get_enzyme_rec <- function(r, drug_key) {
  tibble::tibble(
    id = XML::xmlValue(r[["id"]]),
    name = XML::xmlValue(r[["name"]]),
    organism = XML::xmlValue(r[["organism"]]),
    known_action = XML::xmlValue(r[["known-action"]]),
    inhibition_strength = XML::xmlValue(r[["inhibition-strength"]]),
    induction_strength = XML::xmlValue(r[["induction-strength"]]),
    position = ifelse(is.null(XML::xmlGetAttr(r, name = "position")),
                      NA, XML::xmlGetAttr(r, name = "position")), parent_key = drug_key)
}

get_enzymes_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~get_enzyme_rec(.x, XML::xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug enzymes actions df
get_enzymes_actions_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~drug_sub_df(.x, "actions", id = "id")))
}

# Extract drug articles df
get_enzymes_articles_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")))
}

# Extract drug textbooks df
get_enzymes_textbooks_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")))
}

# Extract drug links df
get_enzymes_links_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~drug_sub_df(.x, "references", seconadary_node = "links", id = "id")))
}

get_enzymes_polypeptide_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~get_polypeptide_rec(.x)))
}

get_enzymes_polypeptide_external_identifiers_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~get_polypeptide_external_identifiers(.x)))
}

get_enzymes_polypeptide_synonyms_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~get_polypeptide_synonyms(.x)))
}

get_enzymes_polypeptide_pfams_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~get_polypeptide_pfams(.x)))
}

get_enzymes_polypeptide_go_classifiers_df <- function(rec) {
    return(purrr::map_df(XML::xmlChildren(rec[["enzymes"]]),
                         ~get_polypeptide_go_classifiers(.x)))
}
