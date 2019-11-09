# Extract drug polypeptide df
get_polypeptide_rec <- function(r) {
  parent_id <- xmlValue(r[["id"]])
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    tibble(
      id = ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
                  xmlGetAttr(p, name = "id")),
      source = ifelse(is.null(xmlGetAttr(p,
                                         name = "source")), NA,
                      xmlGetAttr(p, name = "source")),
      name = xmlValue(p[["name"]]),
      general_function = xmlValue(p[["general-function"]]),
      specific_function = xmlValue(p[["specific-function"]]),
      gene_name = xmlValue(p[["gene-name"]]),
      locus = xmlValue(p[["locus"]]),
      cellular_location = xmlValue(p[["cellular-location"]]),
      transmembrane_regions = xmlValue(p[["transmembrane-regions"]]),
      signal_regions = xmlValue(p[["signal-regions"]]),
      theoretical_pi = xmlValue(p[["theoretical-pi"]]),
      molecular_weight = xmlValue(p[["molecular-weight"]]),
      chromosome_location = xmlValue(p[["chromosome_location"]]),
      organism = xmlValue(p[["organism"]]),
      organism_ncbi_taxonomy_id = xmlGetAttr(p[["organism"]],
                                             name = "ncbi-taxonomy-id"),
      amino_acid_sequence = xmlValue(p[["amino-acid-sequence"]]),
      amino_acid_format = xmlGetAttr(p[["amino-acid-sequence"]],
                                     name = "format"),
      gene_sequence = xmlValue(p[["gene-sequence"]]),
      gene_format = xmlGetAttr(p[["gene-sequence"]],
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
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
             xmlGetAttr(p, name = "id"))
    polypeptide_external_identifiers <-
      xmlToDataFrame(p[["external-identifiers"]], stringsAsFactors = FALSE)
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
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
             xmlGetAttr(p, name = "id"))
    polypeptide_synonyms <- p[["synonyms"]]
    if (xmlSize(polypeptide_synonyms) > 0) {
      tibble(synonyms = paste(xmlApply(polypeptide_synonyms, xmlValue),
                              collapse = ","),
             polypeptide_id = polypeptide_id)
    }
  }
}

# Extract drug polypeptide pfams df
get_polypeptide_pfams <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
             xmlGetAttr(p, name = "id"))
    if (length(xmlChildren(p[["pfams"]])) > 0) {
      firstCell <- xmlValue(xmlChildren(p[["pfams"]])[[1]])
      if (firstCell != "\n    ") {
        polypeptide_pfams <-
          xmlToDataFrame(xmlChildren(p[["pfams"]]), stringsAsFactors = FALSE)
        polypeptide_pfams$polypeptide_id <- polypeptide_id
        return(polypeptide_pfams)
      }
    }
  }
}

# Extract drug polypeptide go-classifiers df
get_polypeptide_go_classifiers <- function(r) {
  p <- r[["polypeptide"]]
  if (!is.null(p)) {
    polypeptide_id <-
      ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
             xmlGetAttr(p, name = "id"))
    if (length(xmlChildren(p[["pfams"]])) > 0) {
      firstCell <- xmlValue(xmlChildren(p[["pfams"]])[[1]])
      if (firstCell != "\n    " &&
          !is.null(p[["go-classifiers"]]) &&
          xmlValue(p[["go-classifiers"]]) != "\n    ") {
        polypeptide_go_classifiers <-
          xmlToDataFrame(xmlChildren(p[["go-classifiers"]]),
                         stringsAsFactors = FALSE)
        if (nrow(polypeptide_go_classifiers) > 0) {
          polypeptide_go_classifiers$polypeptide_id <-
            polypeptide_id
        }
        return(polypeptide_go_classifiers)
      }
    }
  }
}
