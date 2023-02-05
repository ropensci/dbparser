ClassificationParser <- R6::R6Class(
  "ClassificationParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      return(map_df(drugs, ~ private$drug_classifications_df(.x, pb)))
    },
    drug_classifications_df = function(rec, pb) {
      pb$tick()
      if (is.null(rec[["classification"]])) {
        return()
      }
      class_elements <- names(rec[["classification"]])
      c(
        description = xmlValue(rec[["classification"]][["description"]]),
        direct_parent = xmlValue(rec[["classification"]][["direct-parent"]]),
        kingdom = xmlValue(rec[["classification"]][["kingdom"]]),
        superclass = xmlValue(rec[["classification"]][["superclass"]]),
        class = xmlValue(rec[["classification"]][["class"]]),
        subclass = xmlValue(rec[["classification"]][["subclass"]]),
        alternative_parents = paste(
          map_chr(
            rec[["classification"]][class_elements == "alternative-parent"],
            xmlValue),
          collapse = ";"),
        substituents = paste(
          map_chr(
            rec[["classification"]][class_elements == "substituent"],
            xmlValue), collapse = ";"),
        drugbank_id = xmlValue(rec[["drugbank-id"]])

      )
    }
  )
)

#' Drug Classification parser
#'
#' A description of the hierarchical chemical classification of the drug;
#' imported from ClassyFire.
#'
#' @return  a tibble with 9 variables:
#' \describe{
#'   \item{description}{}
#'   \item{direct-parent}{}
#'   \item{kingdom}{}
#'   \item{superclass}{}
#'   \item{class}{}
#'   \item{subclass}{}
#'   \item{alternative-parent}{One or more alternative parents}
#'   \item{substituent}{One or more substituents}
#'   \item{\emph{drugbank_id}}{drugbank id}
#' }
#'@keywords internal
drug_classification <- function() {
    ClassificationParser$new("drug_classifications")$parse()
  }
