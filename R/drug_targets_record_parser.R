get_targets_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~get_organizm_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

get_targets_actions_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~drug_sub_df(.x, "actions", id = "id")))
}

get_targets_articles_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")))
}

get_targets_textbooks_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")))
}

get_targets_links_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~drug_sub_df(.x, "references", seconadary_node = "links", id = "id")))
}

get_targets_polypeptide_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~get_polypeptide_rec(.x)))
}

get_targets_polypeptide_external_identifiers_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~get_polypeptide_external_identifiers(.x)))
}

get_targets_polypeptide_synonyms_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~get_polypeptide_synonyms(.x)))
}

get_targets_polypeptide_pfams_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~get_polypeptide_pfams(.x)))
}

get_targets_polypeptide_go_classifiers_df <- function(rec) {
    return(map_df(xmlChildren(rec[["targets"]]), ~get_polypeptide_go_classifiers(.x)))
}
