# Extract drug carriers df
get_organizm_rec <- function(r, drug_key) {
  tibble::tibble(
    id = XML::xmlValue(r[["id"]]),
    name = XML::xmlValue(r[["name"]]),
    organism = XML::xmlValue(r[["organism"]]),
    known_action = XML::xmlValue(r[["known-action"]]),
    position = ifelse(is.null(XML::xmlGetAttr(r, name = "position")), NA,
                      XML::xmlGetAttr(r, name = "position")),
    parent_key = drug_key
    )
}
