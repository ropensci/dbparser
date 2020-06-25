# Extract drug poly_ex_identity df
get_poly_ex_identity <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
        xmlGetAttr(p, name = "id")
      )
    poly_ex_identity <-
      xmlToDataFrame(p[["external-identifiers"]], stringsAsFactors = FALSE)
    poly_ex_identity$polypeptide_id <-
      polypeptide_id
    return(poly_ex_identity)
  }
}

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

# Extract drug polypeptide go-classifiers df
get_polypeptide_go <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
        xmlGetAttr(p, name = "id")
      )
    if (length(xmlChildren(p[["pfams"]])) > 0) {
      first_cell <- xmlValue(xmlChildren(p[["pfams"]])[[1]])
      if (first_cell != "\n    " &&
        !is.null(p[["go-classifiers"]]) &&
        xmlValue(p[["go-classifiers"]]) != "\n    ") {
        polypeptide_go <-
          xmlToDataFrame(xmlChildren(p[["go-classifiers"]]),
            stringsAsFactors = FALSE
          )
        if (nrow(polypeptide_go) > 0) {
          polypeptide_go$polypeptide_id <-
            polypeptide_id
        }
        return(polypeptide_go)
      }
    }
  }
}
