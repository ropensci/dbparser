PricesParser <- R6::R6Class(
  "PricesParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      map_df(drugs, ~ private$get_prices_df(., pb)) %>%
        unique()
    },
    get_prices_df = function(rec, pb) {
      pb$tick()
      return(map_df(
        xmlChildren(rec[["prices"]]),
        ~ private$get_price_rec(., xmlValue(rec[["drugbank-id"]]))
      ))
    },
    get_price_rec = function(r, drug_key) {
      tibble_row(
        description = xmlValue(r[["description"]]),
        currency = xmlGetAttr(r[["cost"]], name = "currency"),
        cost = xmlValue(r[["cost"]]),
        unit = xmlValue(r[["unit"]]),
        parent_key = drug_key
      )
    }
  )
)


#' Drug Prices Parsers
#'
#' Unit drug prices
#'
#' @return a tibble with 5 variables:
#' \describe{
#'   \item{description}{}
#'   \item{cost}{Drug price per unit}
#'   \item{currency}{Currency of price, example: US.}
#'   \item{unit}{}
#'   \item{parent_id}{drugbank id}
#' }
#' @family drugs
drug_prices <- function() {
  PricesParser$new("drug_prices")$parse()
}
