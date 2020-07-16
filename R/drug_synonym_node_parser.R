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
    },
    save_db_table = function(parsed_tbl) {
      if (private$save_table) {
        save_drug_sub(
          con = private$database_connection,
          df = parsed_tbl,
          table_name = "drug_syn",
          field_types = list(synonym = "varchar(534)")
        )
      }
    }
  )
)

#' Drug Synonyms parser
#'
#' Other names or identifiers that are associated with this drug.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
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
#' @family drugs
#'
#' @inherit run_all_parsers examples
#' @export
drug_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugSynonymsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_syn",
    )$parse()
  }
