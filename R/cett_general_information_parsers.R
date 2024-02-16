CETTGeneralInformationParser <-
  R6::R6Class(
    "CETTGeneralInformationParser",
    inherit = AbstractParser,
    private = list(
      parse_record = function() {
        drugs    <-  xmlChildren(pkg_env$root)
        pb       <- progress_bar$new(total = xmlSize(drugs))

        cett_table <- map_df(drugs, ~ private$cett_rec(., private$tibble_name, pb)) %>%
          unique()

        if (NROW(cett_table) > 0) {
          cett_table <- rename(cett_table, !!(paste0(substr(x     = private$tibble_name,
                                                            start = 1,
                                                            stop  = nchar(private$tibble_name)-1),
                                                     "_id")) := id)
        }
      },
      cett_rec = function(rec, tibble_name, pb) {
        pb$tick()
        drugbank_id <- xmlValue(rec[["drugbank-id"]])
        map_df(xmlChildren(rec[[tibble_name]]),
               ~ private$organizm_rec(., drugbank_id))
      },
      organizm_rec = function(r, drug_key) {
        org <- tibble_row(
          id           = xmlValue(r[["id"]]),
          name         = xmlValue(r[["name"]]),
          organism     = xmlValue(r[["organism"]]),
          known_action = xmlValue(r[["known-action"]]),
          position     = ifelse(is.null(xmlGetAttr(r, name = "position")),
                                NA,
                                xmlGetAttr(r, name = "position")),
          drugbank_id  = drug_key
        )

        if (!is.null(r[["inhibition-strength"]])) {
          org[["inhibition-strength"]] <- xmlValue(r[["inhibition-strength"]])
        }

        if (!is.null(r[["induction-strength"]])) {
          org[["induction-strength"]] <- xmlValue(r[["induction-strength"]])
        }

        org
      }
    )
  )

#' Carriers/ Enzymes/ Targets/ Transporters parsers
#'
#' Protein targets of drug action, enzymes that are inhibited/induced or
#' involved in metabolism, and carrier or transporter proteins involved in
#' movement of the drug across biological membranes.
#'
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
#' @keywords internal
#' @name cett_doc
NULL

#' @rdname cett_doc
carriers <- function() {
    CETTGeneralInformationParser$new("carriers")$parse()
  }


#' @rdname cett_doc
enzymes <- function() {
    CETTGeneralInformationParser$new("enzymes")$parse()
  }


#' @rdname cett_doc
targets <- function() {
    CETTGeneralInformationParser$new("targets")$parse()
}


#' @rdname cett_doc
transporters <- function() {
    CETTGeneralInformationParser$new(
                                     "transporters")$parse()
}
