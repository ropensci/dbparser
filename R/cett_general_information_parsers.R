CETTGeneralInformationParser <-
  R6::R6Class(
    "CETTGeneralInformationParser",
    inherit = AbstractParser,
    private = list(
      parse_record = function() {
        drugs <-  xmlChildren(pkg_env$root)
        pb <- progress_bar$new(total = xmlSize(drugs))
        cett_tbl <-
          map_df(drugs, ~ private$cett_rec(., private$tibble_name, pb)) %>%
          unique()
        return(cett_tbl)
      },
      cett_rec = function(rec, tibble_name, pb) {
        pb$tick()
        drugbank_id <- xmlValue(rec[["drugbank-id"]])
        return(map_df(xmlChildren(rec[[tibble_name]]),
                      ~ private$organizm_rec(., drugbank_id)))
      },
      organizm_rec = function(r, drug_key) {
        org <- tibble_row(
          id = xmlValue(r[["id"]]),
          name = xmlValue(r[["name"]]),
          organism = xmlValue(r[["organism"]]),
          known_action = xmlValue(r[["known-action"]]),
          position = ifelse(is.null(xmlGetAttr(r, name = "position")), NA,
                            xmlGetAttr(r, name = "position")),
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
    )
  )

#' Carriers/ Enzymes/ Targets/ Transporters parsers
#'
#' Protein targets of drug action, enzymes that are inhibited/induced or
#' involved in metabolism, and carrier or transporter proteins involved in
#' movement of the drug across biological membranes.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return a tibble with 6 variables (8 for enzymes):
#' \describe{
#'   \item{id}{Universal Protein Resource (UniProt) Identifier for the record}
#'   \item{name}{related name}
#'   \item{organism}{Organism that the protein comes from.}
#'   \item{known_action}{Whether the pharmacological action of the drug is due
#'    to this target interaction.}
#'   \item{inhibition-strength}{Whether the strength of enzyme inhibition is
#'   strong, moderate, or unknown. \strong{Only applies to enzymes}}
#'   \item{induction-strength}{Whether the strength of enzyme induction is
#'   strong or unknown. \strong{Only applies to enzymes}}
#'   \item{position}{related position}
#'   \item{parent_id}{drugbank id}
#' }
#' @family cett
#' @inherit run_all_parsers examples
#' @name cett_doc
NULL

#' @rdname cett_doc
#' @export
carriers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    CETTGeneralInformationParser$new(save_table,
                                     save_csv,
                                     csv_path,
                                     override_csv,
                                     database_connection,
                                     "carriers")$parse()
  }

#' @rdname cett_doc
#' @export
enzymes <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    CETTGeneralInformationParser$new(save_table,
                                     save_csv,
                                     csv_path,
                                     override_csv,
                                     database_connection,
                                     "enzymes")$parse()
  }

#' @rdname cett_doc
#' @export
targets <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    CETTGeneralInformationParser$new(save_table,
                                     save_csv,
                                     csv_path,
                                     override_csv,
                                     database_connection,
                                     "targets")$parse()
  }

#' @rdname cett_doc
#' @export
transporters <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    CETTGeneralInformationParser$new(save_table,
                                     save_csv,
                                     csv_path,
                                     override_csv,
                                     database_connection,
                                     "transporters")$parse()
  }
