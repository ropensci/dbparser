ManufacturersParser <- R6::R6Class(
  "ManufacturersParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      return(map_df(drugs, ~ private$get_manufacturer_df(., pb)) %>%
               unique())
    },
    get_manufacturer_df = function(rec, pb) {
      pb$tick()
      if (xmlSize(rec[["manufacturers"]]) < 1)
        return()
      manufacturers <- as_tibble(t(xmlSApply(rec[["manufacturers"]],
                                             private$manufacturer_rec)))
      manufacturers[["drugbank-id"]] <- xmlValue(rec[["drugbank-id"]])
      return(manufacturers)
    },
    manufacturer_rec = function(rec) {
      c(
        manufacturer = xmlValue(rec),
        generic = xmlGetAttr(node = rec, name = "generic"),
        url = xmlGetAttr(node = rec, name = "url")
      )
    }
  )
)

#' Drug Manufacturers parser
#'
#' A list of companies that are manufacturing the commercially available forms
#' of this drug that are available in Canada and the Unites States.
#'
#'
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{generic}{A list of companies that are manufacturing the generic
#'   form of the drug.}
#'  \item{url}{A link to the companies that are manufacturing the drug.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_manufacturers <- function() {
  ManufacturersParser$new(
    "drug_manufacturers"
  )$parse()
}
