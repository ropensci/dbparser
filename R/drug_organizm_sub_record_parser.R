# Extract drug carriers df
get_organizm_rec <- function(r, drug_key) {
  tibble(
    id = xmlValue(r[["id"]]),
    name = xmlValue(r[["name"]]),
    organism = xmlValue(r[["organism"]]),
    known_action = xmlValue(r[["known-action"]]),
    position = ifelse(is.null(xmlGetAttr(r, name = "position")), NA,
                      xmlGetAttr(r, name = "position")),
    parent_key = drug_key
    )
}
