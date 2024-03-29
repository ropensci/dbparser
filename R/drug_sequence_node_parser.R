SequencesParser <- R6::R6Class(
  "SequencesParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      return(map_df(drugs, ~ private$get_sequences_df(., pb)))
    },
    get_sequences_df = function(rec, pb) {
      pb$tick()
      if (is.null(rec[["sequences"]])) {
        return()
      }
      return(map_df(
        xmlChildren(rec[["sequences"]]),
        ~ private$get_sequence_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
      ))
    },
    get_sequence_rec = function(r, drug_key) {
      tibble(
        sequence = xmlValue(r),
        format   = ifelse(is.null(xmlGetAttr(r, name = "format")),
                          NA,
                          xmlGetAttr(r, name = "format")
        ),
        drugbank_id = drug_key
      )
    }
  )
)

#' Drug Sequences parser
#'
#' The amino acid sequence; provided if the drug is a peptide.
#'
#' Describes peptide sequences of biotech drugs
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{sequence}{a textual representation of the sequence}
#'  \item{format}{Currently, only the FASTA format is used}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_sequences <- function() {
  SequencesParser$new("drug_sequences")$parse()
}
