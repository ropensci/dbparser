# Extract drug carriers df
organizm_rec <- function(r, drug_key) {
  org <- tibble_row(
    id = xmlValue(r[["id"]]),
    name = xmlValue(r[["name"]]),
    organism = xmlValue(r[["organism"]]),
    known_action = xmlValue(r[["known-action"]]),
    position = ifelse(is.null(xmlGetAttr(r, name = "position")), NA,
                      xmlGetAttr(r, name = "position")
    ),
    parent_key = drug_key
  )
  if (!is.null(r[["inhibition-strength"]])) {
    org[["inhibition-strength"]] <- xmlValue(r[["inhibition-strength"]])
  }

  if (!is.null(r[["induction-strength"]])) {
    org[["induction-strength"]] <- xmlValue(r[["induction-strength"]])
  }

  return(org)
}

cett_rec <- function(rec, tibble_name) {
  drugbank_id <- xmlValue(rec[["drugbank-id"]])
  return(map_df(
    xmlChildren(rec[[tibble_name]]),
    ~ organizm_rec(., drugbank_id)))
}

#' Carriers/ Enzymes/ Targets/ Transporters parsers
#'
#' Protein targets of drug action, enzymes that are inhibited/induced or
#' involved in metabolism, and carrier or transporter proteins involved in
#' movement of the drug across biological membranes.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 6 variables:
#' \describe{
#'   \item{id}{Universal Protein Resource (UniProt) Identifier for the record}
#'   \item{name}{related name}
#'   \item{organism}{Organism that the protein comes from.}
#'   \item{known_action}{Whether the pharmacological action of the drug is due
#'    to this target interaction.}
#'   \item{inhibition-strength}{Whether the strength of enzyme inhibition is
#'   strong, moderate, or unknown. Only applies to enzymes}
#'   \item{induction-strength}{Whether the strength of enzyme induction is
#'   strong or unknown. Only applies to enzymes}
#'   \item{position}{related position}
#'   \item{parent_id}{drugbank id}
#' }
#' @family cett
#' @inherit drug_all examples
#' @name cett_doc
NULL

cett_parser <-
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
      cett_tbl <- readr::read_csv(path)
    } else {
      cett_tbl <-
        map_df(xmlChildren(pkg_env$root), ~ cett_rec(., tibble_name)) %>%
        unique()
      write_csv(cett_tbl, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = cett_tbl,
        table_name = tibble_name,
        foreign_key = "parent_key"
      )
    }
    return(cett_tbl %>% as_tibble())
  }

#' @rdname cett_doc
#' @export
carriers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    cett_parser(save_table,
               save_csv,
               csv_path,
               override_csv,
               database_connection,
               "carriers")
  }

#' @rdname cett_doc
#' @export
enzymes <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    cett_parser(save_table,
                save_csv,
                csv_path,
                override_csv,
                database_connection,
                "enzymes")
  }

#' @rdname cett_doc
#' @export
targets <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    cett_parser(save_table,
                save_csv,
                csv_path,
                override_csv,
                database_connection,
                "targets")
  }

#' @rdname cett_doc
#' @export
transporters <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    cett_parser(save_table,
                save_csv,
                csv_path,
                override_csv,
                database_connection,
                "transporters")
  }
