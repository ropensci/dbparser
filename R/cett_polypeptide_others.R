#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide PFAMS parsers
#'
#' Extract descriptions of identified polypeptide PFAMS targets, enzymes,
#'  carriers, or transporters.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{name}{The sequence of the associated gene.}
#'   \item{identifier}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @family cett
#' @inherit drug_all examples
#' @name cett_poly_pfms_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide External Identifiers
#' parsers
#'
#' Extract descriptions of identified polypeptide external identifiers for
#' targets, enzymes, carriers, or transporters.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{resource}{Name of the source database.}
#'   \item{identifier}{Identifier for this drug in the given resource.}
#'   \item{parent_key}{polypeptide id}
#' }
#' @family cett
#' @inherit drug_all examples
#' @name cett_ex_identity_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide GO Classifier
#' parsers
#'
#' Extract descriptions of identified polypeptide go classifier for targets,
#'  enzymes, carriers, or transporters.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{category}{}
#'   \item{description}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @family cett
#' @inherit drug_all examples
#' @name cett_go_doc
NULL

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide Synonyms parsers
#'
#' Extract descriptions of identified polypeptide synonyms for targets,
#'  enzymes, carriers, or transporters.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 2 variables:
#' \describe{
#'   \item{synonym}{}
#'   \item{parent_key}{polypeptide id}
#' }
#' @family cett
#' @inherit drug_all examples
#' @name cett_poly_syn_doc
NULL

org <- function(rec, cett_type, child_type) {
  return(map_df(
    xmlChildren(rec[[cett_type]]),
    ~ drug_sub_df(., "polypeptide", child_type, "id")
  ))
}

children_parser <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL,
           tibble_name,
           child_type) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path(tibble_name, csv_path)
    if (!override_csv & file.exists(path)) {
      child_tbl <- readr::read_csv(path)
    } else {
      cett_type <- strsplit(tibble_name, "_")[[1]][1]
      child_tbl <-
        map_df(xmlChildren(pkg_env$root),
               ~ org(., cett_type, child_type)) %>%
        unique()

      write_csv(child_tbl, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = child_tbl,
        table_name = tibble_name,
        save_table_only = TRUE
      )
    }
    return(child_tbl %>% as_tibble())
  }

#' @rdname cett_poly_pfms_doc
#' @export
carriers_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "carriers_polypeptides_pfams",
      "pfams"
    )
  }

#' @rdname cett_poly_pfms_doc
#' @export
enzymes_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "enzymes_polypeptides_pfams",
      "pfams"
    )
  }

#' @rdname cett_poly_pfms_doc
#' @export
targets_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "targets_polypeptides_pfams",
      "pfams"
    )
  }

#' @rdname cett_poly_pfms_doc
#' @export
transporters_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "transporters_polypeptides_pfams",
      "pfams"
    )
  }

#' @rdname cett_ex_identity_doc
#' @export
carriers_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "carriers_polypeptides_ext_id",
      "external-identifiers"
    )
  }

#' @rdname cett_ex_identity_doc
#' @export
enzymes_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "enzymes_polypeptides_ext_id",
      "external-identifiers"
    )
  }

#' @rdname cett_ex_identity_doc
#' @export
targets_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "targets_polypeptides_ext_id",
      "external-identifiers"
    )
  }

#' @rdname cett_ex_identity_doc
#' @export
transporters_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "transporters_polypeptides_ext_id",
      "external-identifiers"
    )
  }

#' @rdname cett_go_doc
#' @export
carriers_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "carriers_polypeptides_go",
      "go-classifiers"
    )
  }

#' @rdname cett_go_doc
#' @export
enzymes_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "enzymes_polypeptides_go",
      "go-classifiers"
    )
  }

#' @rdname cett_go_doc
#' @export
targets_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "targets_polypeptides_go",
      "go-classifiers"
    )
  }

#' @rdname cett_go_doc
#' @export
transporters_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "transporters_polypeptides_go",
      "go-classifiers"
    )
  }

#' @rdname cett_poly_syn_doc
#' @export
carriers_polypeptides_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    syn <- children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "carriers_polypeptides_syn",
      "synonyms"
    )

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
           override_csv = FALSE,
           database_connection = NULL) {
    syn <- children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "enzymes_polypeptides_syn",
      "synonyms"
    )

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
           override_csv = FALSE,
           database_connection = NULL) {
    syn <- children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "targets_polypeptides_syn",
      "synonyms"
    )

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
           override_csv = FALSE,
           database_connection = NULL) {
    syn <- children_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "transporters_polypeptides_syn",
      "synonyms"
    )

    if (nrow(syn) > 0) {
      colnames(syn) <- c("synonym", "parent_key")
    }

    return(syn)
  }
