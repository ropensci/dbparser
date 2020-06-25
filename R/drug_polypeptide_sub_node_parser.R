# Extract drug polypeptid syn df
get_polypeptide_syn <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
        xmlGetAttr(p, name = "id")
      )
    polypeptide_syn <- p[["synonyms"]]
    if (xmlSize(polypeptide_syn) > 0) {
      tibble(
        syn = paste(xmlApply(polypeptide_syn, xmlValue),
          collapse = ","
        ),
        polypeptide_id = polypeptide_id
      )
    }
  }
}
