CETTPolyOtherParser <-
  R6::R6Class(
    "CETTPolyOtherParser",
    inherit = AbstractParser,
    private = list(
      parse_record = function() {
        drugs     <-  xmlChildren(pkg_env$root)
        pb        <- progress_bar$new(total = xmlSize(drugs))
        cett_type <- strsplit(private$tibble_name, "_")[[1]][1]

        parsed_table <- map_df(drugs,
               ~ private$org(., cett_type, pb)) %>%
          unique()

        if (NROW(parsed_table) > 0) {
          parent_id_name <- paste0(substr(x     = cett_type,
                                          start = 1,
                                          stop  = nchar(cett_type)-1),
                                   "_id")
          parsed_table <- parsed_table %>%
            rename(!!parent_id_name := parent_key)
        }

      },
      org = function(rec, cett_type, pb) {
        pb$tick()
        map_df(
          xmlChildren(rec[[cett_type]]),
          ~ drug_sub_df(., "polypeptide", private$object_node, "id")
        )
      }
    )
  )

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide PFAMS parsers
#'
#' Extract descriptions of identified polypeptide PFAMS targets, enzymes,
#'  carriers, or transporters.
#'
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{name}{The sequence of the associated gene.}
#'   \item{identifier}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @keywords internal
#' @name cett_poly_pfms_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide External Identifiers
#' parsers
#'
#' Extract descriptions of identified polypeptide external identifiers for
#' targets, enzymes, carriers, or transporters.
#'
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{resource}{Name of the source database.}
#'   \item{identifier}{Identifier for this drug in the given resource.}
#'   \item{parent_key}{polypeptide id}
#' }
#' @keywords internal
#' @name cett_ex_identity_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide GO Classifier
#' parsers
#'
#' Extract descriptions of identified polypeptide go classifier for targets,
#'  enzymes, carriers, or transporters.
#'
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{category}{}
#'   \item{description}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @keywords internal
#' @name cett_go_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide Synonyms parsers
#'
#' Extract descriptions of identified polypeptide synonyms for targets,
#'  enzymes, carriers, or transporters.
#'
#'
#' @return a tibble with 2 variables:
#' \describe{
#'   \item{synonym}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @keywords internal
#' @name cett_poly_syn_doc
NULL

#' @rdname cett_poly_pfms_doc
carriers_polypeptides_pfams <- function() {
    CETTPolyOtherParser$new(
      "carriers_polypeptides_pfams",
      "pfams"
    )$parse()
  }

#' @rdname cett_poly_pfms_doc
enzymes_polypeptides_pfams <- function() {
    CETTPolyOtherParser$new(
      "enzymes_polypeptides_pfams",
      "pfams"
    )$parse()
  }

#' @rdname cett_poly_pfms_doc
targets_polypeptides_pfams <- function() {
    CETTPolyOtherParser$new(
      "targets_polypeptides_pfams",
      "pfams"
    )$parse()
  }

#' @rdname cett_poly_pfms_doc
transporters_polypeptides_pfams <- function() {
    CETTPolyOtherParser$new(
      "transporters_polypeptides_pfams",
      "pfams"
    )$parse()
  }

#' @rdname cett_ex_identity_doc
carriers_polypep_ex_ident <-
  function() {
    CETTPolyOtherParser$new(
      "carriers_polypeptides_ext_id",
      "external-identifiers"
    )$parse()
  }

#' @rdname cett_ex_identity_doc
enzymes_polypep_ex_ident <- function() {
    CETTPolyOtherParser$new(
      "enzymes_polypeptides_ext_id",
      "external-identifiers"
    )$parse()
  }

#' @rdname cett_ex_identity_doc
targets_polypep_ex_ident <- function() {
    CETTPolyOtherParser$new(
      "targets_polypeptides_ext_id",
      "external-identifiers"
    )$parse()
  }

#' @rdname cett_ex_identity_doc
transporters_polypep_ex_ident <- function() {
    CETTPolyOtherParser$new(
      "transporters_polypeptides_ext_id",
      "external-identifiers"
    )$parse()
  }

#' @rdname cett_go_doc
carriers_polypeptides_go <- function() {
    CETTPolyOtherParser$new(
      "carriers_polypeptides_go",
      "go-classifiers"
    )$parse()
  }

#' @rdname cett_go_doc
enzymes_polypeptides_go <- function() {
    CETTPolyOtherParser$new(
      "enzymes_polypeptides_go",
      "go-classifiers"
    )$parse()
  }

#' @rdname cett_go_doc
targets_polypeptides_go <- function() {
    CETTPolyOtherParser$new(
      "targets_polypeptides_go",
      "go-classifiers"
    )$parse()
  }

#' @rdname cett_go_doc
transporters_polypeptides_go <- function() {
    CETTPolyOtherParser$new(
      "transporters_polypeptides_go",
      "go-classifiers"
    )$parse()
  }

#' @rdname cett_poly_syn_doc
carriers_polypeptides_syn <- function() {
    syn <- CETTPolyOtherParser$new(
      "carriers_polypeptides_syn",
      "synonyms"
    )$parse()

    if (NROW(syn) > 0) {
      colnames(syn) <- c("synonym", "carrier_id")
    }

    syn
  }

#' @rdname cett_poly_syn_doc
enzymes_polypeptides_syn <- function() {
    syn <- CETTPolyOtherParser$new(
      "enzymes_polypeptides_syn",
      "synonyms"
    )$parse()

    if (NROW(syn) > 0) {
      colnames(syn) <- c("synonym", "enzyme_id")
    }

    syn
  }

#' @rdname cett_poly_syn_doc
targets_polypeptides_syn <- function() {
    syn <- CETTPolyOtherParser$new(
      "targets_polypeptides_syn",
      "synonyms"
    )$parse()

    if (NROW(syn) > 0) {
      colnames(syn) <- c("synonym", "target_id")
    }

    syn
  }

#' @rdname cett_poly_syn_doc
transporters_polypeptides_syn <- function() {
    syn <- CETTPolyOtherParser$new(
      "transporters_polypeptides_syn",
      "synonyms"
    )$parse()

    if (NROW(syn) > 0) {
      colnames(syn) <- c("synonym", "transporter_id")
    }

    syn
  }
