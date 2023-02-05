ATCParser <- R6::R6Class(
  "ATCParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      return(as_tibble(do.call(
        rbind,
        xmlApply(pkg_env$root, private$atc_recs, pb)
      )))
    },
    atc_recs = function(rec, pb) {
      pb$tick()
      if (xmlSize(rec[["atc-codes"]]) < 1) {
        return()
      }
      atcs <- xmlApply(rec[["atc-codes"]], private$atc_rec)
      atcs_tibble <-
        as_tibble(do.call(rbind, atcs))
      atcs_tibble[["drugbank-id"]] = xmlValue(rec[["drugbank-id"]])
      return(atcs_tibble)
    },
    atc_rec = function(atc) {
      c(
        atc_code = xmlGetAttr(atc, name = "code"),
        level_1 = xmlValue(atc[[1]]),
        code_1 = xmlGetAttr(atc[[1]], name = "code"),
        level_2 = xmlValue(atc[[2]]),
        code_2 = xmlGetAttr(atc[[2]], name = "code"),
        level_3 = xmlValue(atc[[3]]),
        code_3 = xmlGetAttr(atc[[3]], name = "code"),
        level_4 = xmlValue(atc[[4]]),
        code_4 = xmlGetAttr(atc[[4]], name = "code")
      )
    }
  )
)

#' Drug ATC Codes element parser
#'
#' The Anatomical Therapeutic Classification (ATC) code for the drug assigned
#' by the World Health Organization Anatomical Chemical Classification System.
#'
#' Each `atc-code`` row has one or more level. The atc-code and level>
#' have a code  the code assigned by the World Health Organization Anatomical
#' Therapeutic Chemical Classification system.
#'
#'
#' @return  a tibble with 10 variables
#' @family drugs
drug_atc_codes <- function() {
  ATCParser$new("drug_atc_codes")$parse()
}
