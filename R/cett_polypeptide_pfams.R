#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide PFAMS parsers
#'
#' Extract descriptions of identified polypeptide PFAMS targets, enzymes, carriers,
#'  or transporters.
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

pfams_rec <- function(r) {
  p <- r[["polypeptide"]]
  if (is.null(p)) {
    return()
  }

  return(map_df(xmlChildren(p),
                ~ drug_sub_df(., "pfams", id = "id")))
}


org_pfams <- function(rec, cett_type) {
  message(cett_type)
  message(xmlValue(rec[[cett_type]]))
  return(map_df(xmlChildren(rec[[cett_type]]),
                ~ drug_sub_df(., "polypeptide", "pfams", "id")))
}

pfams_parser <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL,
           tibble_name) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path(tibble_name, csv_path)
    if (!override_csv & file.exists(path)) {
      pfams_tbl <- readr::read_csv(path)
    } else {
      cett_type <- strsplit(tibble_name, "_")[[1]][1]
      pfams_tbl <-
        map_df(xmlChildren(pkg_env$root),
               ~ org_pfams(., cett_type)) %>%
        unique()

      write_csv(pfams_tbl, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = pfams_tbl,
        table_name = tibble_name,
        save_table_only = TRUE
      )
    }
    return(pfams_tbl %>% as_tibble())
  }

#' @rdname cett_poly_pfms_doc
#' @export
carriers_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    pfams_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "carriers_polypeptides_pfams"
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
    pfams_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "enzymes_polypeptides_pfams"
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
    pfams_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "targets_polypeptides_pfams"
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
    pfams_parser(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "transporters_polypeptides_pfams"
    )
  }
