#' Extracts the all drug elements and return data as list of dataframes.
#'
#' \code{parse_drug_all} returns list of dataframes of drugs elements.
#'
#' This functions extracts all element of drug nodes in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug main node attributs data frame
#'
#' @examples
#' parse_drug_all()
#' parse_drug_all(TRUE)
#' parse_drug_all(save_table = FALSE)
#' @export
