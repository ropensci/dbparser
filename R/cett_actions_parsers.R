CETTActionsParser <-
  R6::R6Class(
    "CETTActionsParser",
    inherit = AbstractParser,
    private = list(
      parse_record = function() {
        cett_type   <- strsplit(private$tibble_name, "_")[[1]][1]
        drugs       <-  xmlChildren(pkg_env$root)
        pb          <- progress_bar$new(total = xmlSize(drugs))
        actions_tbl <-
          map_df(drugs, ~ private$actions_rec(., cett_type, pb)) %>% unique()
        if (NROW(actions_tbl) > 0) {
          colnames(actions_tbl) <- c("action",
                                     paste0(substr(x     = cett_type,
                                                   start = 1,
                                                   stop  = nchar(cett_type)-1),
                                            "_id"))
        }

        actions_tbl
      },
      actions_rec = function(rec, cett_type, pb) {
        pb$tick()
        map_df(xmlChildren(rec[[cett_type]]),
                      ~ drug_sub_df(., "actions", id = "id"))
      }
    )
  )

#' Carriers/ Enzymes/ Targets/ Transporters Actions parsers
#'
#' Collection of related actions
#'
#' @return a tibble with 2 variables:
#' \describe{
#'   \item{action}{describe related action}
#'   \item{\emph{parent_id}}{carrier/ target/ enzyme/ transporter id}
#' }
#' @keywords internal
#' @name cett_actions_doc
NULL

#' @rdname cett_actions_doc
carriers_actions <- function() {
  CETTActionsParser$new(
    "carriers_actions"
  )$parse()
}

#' @rdname cett_actions_doc
enzymes_actions <- function() {
  CETTActionsParser$new(
    "enzymes_actions"
  )$parse()
}

#' @rdname cett_actions_doc
targets_actions <- function() {
  CETTActionsParser$new(
    "targets_actions"
  )$parse()
}

#' @rdname cett_actions_doc
transporters_actions <- function() {
  CETTActionsParser$new(
    "transporters_actions"
  )$parse()
}
