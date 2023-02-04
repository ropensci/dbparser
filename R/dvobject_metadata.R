#' init_dvobject
#'   initialize dvobject
#'
#' @keywords internal
#' @return dvobject
init_dvobject <- function() {
  dvobject        <- list()
  class(dvobject) <- "dvobject"

  attr(dvobject, "original_db_info") <- list()
  dvobject
}


#' add_drugbank_info
#'   Add passed DrugBank db metadata to passed dvobject
#'
#' @keywords internal
#' @return dvobject
add_drugbank_info <- function(dvobject) {
  db_info <- attr(dvobject, "original_db_info")

  db_info[["db_type"]]          <- "DrugBank"
  db_info[["db_version"]]       <- XML::xmlGetAttr(node = pkg_env$root,
                                                   name = "version")
  db_info[["db_exported_date"]] <- XML::xmlGetAttr(node = pkg_env$root,
                                                   name = "exported-on")

  attr(dvobject, "original_db_info") <- db_info
  dvobject
}


#' init_dvobject
#'   Returns data.frame with two columns (key, value) of dvobject attributes
#'
#' @param dvobject - dvobject list to show related metadata
#'
#' @return data.frame
#' @family utility
#' @export
show_dvobject_metadata <- function(dvobject) {
  dvobject_attributes            <- attr(dvobject, "original_db_info")
  dvobject_attributes[["class"]] <- class(dvobject)
  data.frame(Atrribute = names(dvobject_attributes), Value =  stack(dvobject_attributes)[[1]])
}
