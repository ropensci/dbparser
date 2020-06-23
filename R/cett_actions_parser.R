
actions_rec <- function(rec, cett_type) {
  return(map_df(xmlChildren(rec[[cett_type]]),
                ~ drug_sub_df(., "actions", id = "id")))
}

#' Carriers/ Enzymes/ Targets/ Transporters Actions parsers
#'
#' Collection of related actions
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 2 variables:
#' \describe{
#'   \item{action}{describe related action}
#'   \item{\emph{parent_id}}{drug/ carrier/ target/ enzyme/ transporter id}
#' }
#' @family cett
#' @inherit drug_all examples
#' @name cett_actions_doc
NULL

actions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL,
           tibble_name) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path(tibble_name, csv_path)
    if (!override_csv & file.exists(path)) {
      actions_tbl <- readr::read_csv(path)
    } else {
      cett_type <- strsplit(tibble_name, "_")[[1]][1]
      actions_tbl <-
        map_df(pkg_env$children, ~ actions_rec(., cett_type)) %>% unique()

      write_csv(actions_tbl, save_csv, csv_path)
    }


    if (nrow(actions_tbl) > 0) {
      colnames(actions_tbl) <- c("action", "parent_id")
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = actions_tbl,
        table_name = tibble_name,
        save_table_only = TRUE
      )
    }
    return(actions_tbl %>% as_tibble())
  }

#' @export
carriers_actions <- function(save_table = FALSE,
                             save_csv = FALSE,
                             csv_path = ".",
                             override_csv = FALSE,
                             database_connection = NULL) {
  actions(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "carriers_actions"
  )
}

#' @export
enzymes_actions <- function(save_table = FALSE,
                            save_csv = FALSE,
                            csv_path = ".",
                            override_csv = FALSE,
                            database_connection = NULL) {
  actions(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "enzymes_actions"
  )
}

#' @export
targets_actions <- function(save_table = FALSE,
                            save_csv = FALSE,
                            csv_path = ".",
                            override_csv = FALSE,
                            database_connection = NULL) {
  actions(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "targets_actions"
  )
}

#' @export
transporters_actions <- function(save_table = FALSE,
                            save_csv = FALSE,
                            csv_path = ".",
                            override_csv = FALSE,
                            database_connection = NULL) {
  actions(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "transporters_actions"
  )
}
