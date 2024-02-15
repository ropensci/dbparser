ReactionsParser <- R6::R6Class(
  "ReactionsParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb    <- progress_bar$new(total = xmlSize(drugs))
      map_df(drugs, ~ private$get_reactions_df(., pb))
    },
    get_reactions_df = function(rec, pb) {
      pb$tick()
      map_df(
        xmlChildren(rec[["reactions"]]),
        ~ private$get_reactions_rec(., xmlValue(rec[["drugbank-id"]]))
      )
    },
    get_reactions_rec = function(r, drug_key) {
      tibble(
        sequence = xmlValue(r[["sequence"]]),
        left_drugbank_id    = xmlValue(r[["left-element"]][["drugbank-id"]]),
        left_drugbank_name  = xmlValue(r[["left-element"]][["name"]]),
        right_drugbank_id   = xmlValue(r[["right-element"]][["drugbank-id"]]),
        right_drugbank_name = xmlValue(r[["right-element"]][["name"]]),
        drugbank_id         = drug_key
      )
    }
  )
)

ReactionsEnzymesParser <- R6::R6Class(
  "ReactionsEnzymesParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb    <- progress_bar$new(total = xmlSize(drugs))
      map_df(drugs, ~ private$get_reactions_enzymes_df(., pb))
    },
    get_reactions_enzymes_df = function(rec, pb) {
      pb$tick()
      map_df(
        xmlChildren(rec[["reactions"]]),
        ~ drug_sub_df(., "enzymes", id = NULL)
      )
    }
  )
)

#' Drug Reactions Parsers
#'
#' Extract the sequential representation of the metabolic reactions that this
#'  drug molecule is involved in. Depending on available information, this may
#'  include metabolizing enzymes, reaction type, substrates, products,
#'  pharmacological activity of metabolites, and a structural representation of
#'   the biochemical reactions.
#'
#' @return a tibble with 5 variables:
#' \describe{
#'   \item{sequence}{	Reactions are displayed within a numerical sequence}
#'   \item{left_drugbank_name}{The substrate of the reaction. Maybe a drug or a
#'    metabolite.}
#'   \item{rightt_drugbank_name}{	The product of the reaction. Maybe a drug or a
#'    metabolite.}
#'   \item{left_drugbank_id}{}
#'   \item{right_drugbank_id}{}
#'   \item{parent_id}{drugbank id}
#' }
#' @keywords internal
drug_reactions <- function() {
    ReactionsParser$new("drug_reactions")$parse()
}

#' Drug Reactions Enzymes Parsers
#'
#' Enzymes involved in metabolizing this drug
#'
#' @return a tibble with 3 variables:
#' \describe{
#'   \item{name}{}
#'   \item{uniprot-id}{}
#'   \item{parent_id}{drugbank id}
#' }
#' @keywords internal
drug_reactions_enzymes <- function() {
  enzyme <- ReactionsEnzymesParser$new("drug_reactions_enzymes")$parse()

  if ("drugbank_id" %in% names(enzyme)) {
    enzyme <- rename(enzyme, enzyme_id = drugbank_id)
  }

  enzyme
}
