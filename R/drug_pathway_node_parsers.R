PathwayParser <- R6::R6Class(
  "PathwayParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      map_df(drugs, ~ private$get_pathways_df(.x, pb)) %>%
        unique()
    },
    get_pathways_df = function(rec, pb) {
      pb$tick()
      return(
        map_df(
          xmlChildren(rec[["pathways"]]),
          ~ private$get_pathway_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
    },
    get_pathway_rec = function(r, drug_key) {
      tibble(
        smpdb_id = xmlValue(r[["smpdb-id"]]),
        name = xmlValue(r[["name"]]),
        category = xmlValue(r[["category"]]),
        parent_key = drug_key
      )
    }
  )
)

PathwaySubNodesParser <- R6::R6Class(
  "PathwaySubNodesParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      parsed_tbl <-
        map_df(drugs, ~ private$get_pathways_sub(., pb)) %>%
        unique()
      if (nrow(parsed_tbl) > 0) {
        switch(
          private$main_node,
          "enzymes" = names(parsed_tbl) <-
            c("enzyme", "pathway_id")
        )
      }
      return(parsed_tbl)
    },
    get_pathways_sub = function(rec, pb) {
      pb$tick()
      return(map_df(
        xmlChildren(rec[[private$object_node]]),
        ~ drug_sub_df(., private$main_node, id = private$id)
      ))
    }
  )
)

#' Drug Pathway Enzymes parser
#'
#' Enzymes involved in this pathway.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return  a tibble with pathway properties
#' @family pathway
#'
#' @inherit run_all_parsers examples
#' @export
drug_pathway_enzyme <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    PathwaySubNodesParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      tibble_name = "drug_pathway_enzymes",
      object_node = "pathways",
      main_node = "enzymes",
      id = "smpdb-id"
    )$parse()
  }

#' Drug Pathway Drugs parser
#'
#' Drugs involved in this pathway.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return  a tibble with pathway drugsproperties
#' @family pathway
#'
#' @inherit run_all_parsers examples
#' @export
drug_pathway_drugs <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    PathwaySubNodesParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      tibble_name = "drug_pathway_drugs",
      object_node = "pathways",
      main_node = "drugs",
      id = "smpdb-id"
    )$parse()
  }

#' Drug Pathway parser
#'
#' Metabolic, disease, and biological pathways that the drug is involved in, as
#' identified by the Small Molecule Protein Database (SMPDB).
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{smpdb-id}{Small Molecule Pathway Database identifier for this
#'  pathway.}
#'  \item{name}{Pathway name}
#'  \item{category}{Pathway category}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family pathway
#'
#' @inherit run_all_parsers examples
#' @export
drug_pathway <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    PathwayParser$new(save_table,
                      save_csv,
                      csv_path,
                      override_csv,
                      "drug_pathway")$parse()
  }
