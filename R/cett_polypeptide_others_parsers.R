CETTPolyOtherParser <-
  R6::R6Class(
    "CETTPolyOtherParser",
    inherit = AbstractParser,
    private = list(
      parse_record = function() {
        drugs <-  xmlChildren(pkg_env$root)
        pb <- progress_bar$new(total = xmlSize(drugs))
        cett_type <- strsplit(private$tibble_name, "_")[[1]][1]
        return(
          map_df(drugs,
                 ~ private$org(., cett_type, pb)) %>%
          unique())
      },
      org = function(rec, cett_type, pb) {
        pb$tick()
        return(map_df(
          xmlChildren(rec[[cett_type]]),
          ~ drug_sub_df(., "polypeptide", private$object_node, "id")
        ))
      }
    )
  )

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide PFAMS parsers
#'
#' Extract descriptions of identified polypeptide PFAMS targets, enzymes,
#'  carriers, or transporters.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{name}{The sequence of the associated gene.}
#'   \item{identifier}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @family cett
#' @inherit run_all_parsers examples
#' @name cett_poly_pfms_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide External Identifiers
#' parsers
#'
#' Extract descriptions of identified polypeptide external identifiers for
#' targets, enzymes, carriers, or transporters.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{resource}{Name of the source database.}
#'   \item{identifier}{Identifier for this drug in the given resource.}
#'   \item{parent_key}{polypeptide id}
#' }
#' @family cett
#' @inherit run_all_parsers examples
#' @name cett_ex_identity_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide GO Classifier
#' parsers
#'
#' Extract descriptions of identified polypeptide go classifier for targets,
#'  enzymes, carriers, or transporters.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{category}{}
#'   \item{description}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @family cett
#' @inherit run_all_parsers examples
#' @name cett_go_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide Synonyms parsers
#'
#' Extract descriptions of identified polypeptide synonyms for targets,
#'  enzymes, carriers, or transporters.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return a tibble with 2 variables:
#' \describe{
#'   \item{synonym}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @family cett
#' @inherit run_all_parsers examples
#' @name cett_poly_syn_doc
NULL

#' @rdname cett_poly_pfms_doc
#' @export
carriers_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "carriers_polypeptides_pfams",
      "pfams"
    )$parse()
  }

#' @rdname cett_poly_pfms_doc
#' @export
enzymes_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "enzymes_polypeptides_pfams",
      "pfams"
    )$parse()
  }

#' @rdname cett_poly_pfms_doc
#' @export
targets_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "targets_polypeptides_pfams",
      "pfams"
    )$parse()
  }

#' @rdname cett_poly_pfms_doc
#' @export
transporters_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "transporters_polypeptides_pfams",
      "pfams"
    )$parse()
  }

#' @rdname cett_ex_identity_doc
#' @export
carriers_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "carriers_polypeptides_ext_id",
      "external-identifiers"
    )$parse()
  }

#' @rdname cett_ex_identity_doc
#' @export
enzymes_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "enzymes_polypeptides_ext_id",
      "external-identifiers"
    )$parse()
  }

#' @rdname cett_ex_identity_doc
#' @export
targets_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "targets_polypeptides_ext_id",
      "external-identifiers"
    )$parse()
  }

#' @rdname cett_ex_identity_doc
#' @export
transporters_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "transporters_polypeptides_ext_id",
      "external-identifiers"
    )$parse()
  }

#' @rdname cett_go_doc
#' @export
carriers_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "carriers_polypeptides_go",
      "go-classifiers"
    )$parse()
  }

#' @rdname cett_go_doc
#' @export
enzymes_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "enzymes_polypeptides_go",
      "go-classifiers"
    )$parse()
  }

#' @rdname cett_go_doc
#' @export
targets_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "targets_polypeptides_go",
      "go-classifiers"
    )$parse()
  }

#' @rdname cett_go_doc
#' @export
transporters_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "transporters_polypeptides_go",
      "go-classifiers"
    )$parse()
  }

#' @rdname cett_poly_syn_doc
#' @export
carriers_polypeptides_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    syn <- CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "carriers_polypeptides_syn",
      "synonyms"
    )$parse()

    if (nrow(syn) > 0) {
      colnames(syn) <- c("synonym", "parent_key")
    }

    return(syn)
  }

#' @rdname cett_poly_syn_doc
#' @export
enzymes_polypeptides_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    syn <- CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "enzymes_polypeptides_syn",
      "synonyms"
    )$parse()

    if (nrow(syn) > 0) {
      colnames(syn) <- c("synonym", "parent_key")
    }

    return(syn)
  }

#' @rdname cett_poly_syn_doc
#' @export
targets_polypeptides_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    syn <- CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "targets_polypeptides_syn",
      "synonyms"
    )$parse()

    if (nrow(syn) > 0) {
      colnames(syn) <- c("synonym", "parent_key")
    }

    return(syn)
  }

#' @rdname cett_poly_syn_doc
#' @export
transporters_polypeptides_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    syn <- CETTPolyOtherParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "transporters_polypeptides_syn",
      "synonyms"
    )$parse()

    if (nrow(syn) > 0) {
      colnames(syn) <- c("synonym", "parent_key")
    }

    return(syn)
  }
