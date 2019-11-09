#' Return uploaded drugbank database version
#'
#' \code{get_drugbank_version} returns uploaded drugbank database version.
#'
#' @return drugbank version
#'
#' @examples
#' \donttest{
#' get_drugbank_version()
#' }
#' @export
get_drugbank_version <- function() {
  return(pkg_env$version)
}

#' Return uploaded drugbank database exported date
#'
#' \code{get_drugbank_exported_date} returns uploaded drugbank database
#' exported date.
#'
#' @return drugbank exported date
#'
#' @examples
#' \donttest{
#' get_drugbank_exported_date()
#' }
#' @export
get_drugbank_exported_date <- function() {
  return(pkg_env$exported_date)
}


#' Return uploaded drugbank database metadata
#'
#' \code{get_drugbank_metadata} returns uploaded drugbank database version and
#' exported date.
#'
#' @return drugbank metadata
#'
#' @examples
#' \donttest{
#' get_drugbank_metadata()
#' }
#' @export
get_drugbank_metadata <- function() {
  return(list(pkg_env$version, pkg_env$exported_date))
}
