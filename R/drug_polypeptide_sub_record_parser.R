# Extract drug polypeptide df
get_polypeptide_rec <- function(r) {
  parent_id = XML::xmlValue(r[["id"]])
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    tibble::tibble(
      id = ifelse(is.null(XML::xmlGetAttr(p, name = "id")), NA, XML::xmlGetAttr(p, name = "id")),
      source = ifelse(is.null(XML::xmlGetAttr(p,
                                         name = "source")), NA, XML::xmlGetAttr(p, name = "source")),
      name = XML::xmlValue(p[["name"]]),
      general_function = XML::xmlValue(p[["general-function"]]),
      specific_function = XML::xmlValue(p[["specific-function"]]),
      gene_name = XML::xmlValue(p[["gene-name"]]),
      locus = XML::xmlValue(p[["locus"]]),
      cellular_location = XML::xmlValue(p[["cellular-location"]]),
      transmembrane_regions = XML::xmlValue(p[["transmembrane-regions"]]),
      signal_regions = XML::xmlValue(p[["signal-regions"]]),
      theoretical_pi = XML::xmlValue(p[["theoretical-pi"]]),
      molecular_weight = XML::xmlValue(p[["molecular-weight"]]),
      chromosome_location = XML::xmlValue(p[["chromosome_location"]]),
      organism = XML::xmlValue(p[["organism"]]),
      organism_ncbi_taxonomy_id = XML::xmlGetAttr(p[["organism"]],
                                             name = "ncbi-taxonomy-id"),
      amino_acid_sequence = XML::xmlValue(p[["amino-acid-sequence"]]),
      amindo_acid_format = XML::xmlGetAttr(p[["amino-acid-sequence"]],
                                      name = "format"),
      gene_sequence = XML::xmlValue(p[["gene-sequence"]]),
      gene_format = XML::xmlGetAttr(p[["gene-sequence"]],
                               name = "format"),
      parent_id = parent_id
    )
  }
}

# Extract drug polypeptide_external_identifiers df
get_polypeptide_external_identifiers <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(XML::xmlGetAttr(p, name = "id")), NA, XML::xmlGetAttr(p, name = "id"))
    polypeptide_external_identifiers <-
      XML::xmlToDataFrame(p[["external-identifiers"]])
    polypeptide_external_identifiers$polypeptide_id <-
      polypeptide_id
    return(polypeptide_external_identifiers)
  }
}

# Extract drug polypeptid synonyms df
get_polypeptide_synonyms <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(XML::xmlGetAttr(p, name = "id")), NA, XML::xmlGetAttr(p, name = "id"))
    polypeptide_synonyms <- p[["synonyms"]]
    if (xmlSize(polypeptide_synonyms) > 0) {
      tibble::tibble(synonyms = paste(xmlApply(polypeptide_synonyms, XML::xmlValue), collapse = ","),
             polypeptide_id = polypeptide_id)
    }
  }
}

# Extract drug polypeptide pfams df
get_polypeptide_pfams <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(XML::xmlGetAttr(p, name = "id")), NA, XML::xmlGetAttr(p, name = "id"))
    firstCell <- XML::xmlValue(XML::xmlChildren(p[["pfams"]])[[1]])
    if (firstCell != "\n    ") {
      polypeptide_pfams <- XML::xmlToDataFrame(XML::xmlChildren(p[["pfams"]]))
      polypeptide_pfams$polypeptide_id <- polypeptide_id
      return(polypeptide_pfams)
    }
  }
}

# Extract drug polypeptide go-classifiers df
get_polypeptide_go_classifiers <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(XML::xmlGetAttr(p, name = "id")), NA, XML::xmlGetAttr(p, name = "id"))
    firstCell <- XML::xmlValue(XML::xmlChildren(p[["pfams"]])[[1]])
    if (firstCell != "\n    " &&
        !is.null(p[["go-classifiers"]]) &&
        XML::xmlValue(p[["go-classifiers"]]) != "\n    ") {
      polypeptide_go_classifiers <-
        XML::xmlToDataFrame(XML::xmlChildren(p[["go-classifiers"]]))
      polypeptide_go_classifiers$polypeptide_id <-
        polypeptide_id
      return(polypeptide_go_classifiers)
    }
  }
}
