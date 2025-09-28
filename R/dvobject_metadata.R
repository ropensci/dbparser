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


#' add_database_info
#'   Assign passed databases db metadata to passed dvobject
#'
#' @param db_type database type (defualt="DrugBank")
#' @param db_version database version as string
#' @param db_exported_date database official export date
#'
#' @keywords internal
#' @return dvobject
add_database_info <- function(dvobject,
                              db_type          = "DrugBank",
                              db_version       = NULL,
                              db_exported_date = NULL) {
  db_info <- attr(dvobject, db_type)

  db_info[["db_type"]]          <- db_type
  db_info[["db_version"]]       <- db_version
  db_info[["db_exported_date"]] <- db_exported_date
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
