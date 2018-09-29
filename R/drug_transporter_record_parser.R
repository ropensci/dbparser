get_transporters_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_organizm_rec(.x,
                                    xmlValue(rec["drugbank-id"][[1]]))))
}

get_transporters_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~ drug_sub_df(.x, "actions", id = "id")))
}

get_transporters_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_transporters_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_transporters_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}

get_transporters_polypeptide_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_polypeptide_rec(.x)))
}

get_transporters_polypeptide_external_identifiers_df <-
  function(rec) {
    return (map_df(
      xmlChildren(rec[["transporters"]]),
      ~ get_polypeptide_external_identifiers(.x)
    ))
  }

get_transporters_polypeptide_synonyms_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_polypeptide_synonyms(.x)))
}

get_transporters_polypeptide_pfams_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_polypeptide_pfams(.x)))
}

get_transporters_polypeptide_go_classifiers_df <- function(rec) {
  return (map_df(xmlChildren(rec[["transporters"]]),
                 ~ get_polypeptide_go_classifiers(.x)))
}


