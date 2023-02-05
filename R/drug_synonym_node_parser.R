DrugSynonymsParser <- R6::R6Class(
  "DrugSynonymsParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      map_df(drugs, ~ private$get_syn_df(.x, pb)) %>% unique()
    },
    get_syn_df = function(rec, pb) {
      pb$tick()
      if (xmlSize(rec[["synonyms"]]) < 1) return()
      syn <- as_tibble(t(xmlSApply(rec[["synonyms"]], private$syn_rec)))
      syn[["drugbank-id"]] <- xmlValue(rec[["drugbank-id"]])
      return(syn)

    },
    syn_rec = function(rec) {
      c(
        synonym = xmlValue(rec),
        language = xmlGetAttr(rec, name = "language"),
        coder = xmlGetAttr(rec,
                           name = "coder")
      )
    }
  )
)

#' Drug Synonyms parser
#'
#' Other names or identifiers that are associated with this drug.
#'
#' @return  a tibble with 3 variables:
#' \describe{
#'  \item{language}{Names of the drug in languages other than English.}
#'  \item{coder}{Organisation or source providing the synonym. For example,
#'   INN indicates the synonym is an International Nonproprietary Name,
#'   while IUPAC indicates the synonym is the nomenclature designated by the
#'   International Union of Pure and Applied Chemistry.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_syn <- function() {
    DrugSynonymsParser$new("drug_syn")$parse()
}
