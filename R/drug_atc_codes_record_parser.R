get_atc_codes_rec <- function(r, drug_key) {
  tibble(
    atc_code = xmlGetAttr(r, name = "code"),
    level_1 = xmlValue(r[[1]]),
    code_1 = xmlGetAttr(r[[1]], name = "code"),
    level_2 = xmlValue(r[[2]]),
    code_2 = xmlGetAttr(r[[2]], name = "code"),
    level_3 = xmlValue(r[[3]]),
    code_3 = xmlGetAttr(r[[3]], name = "code"),
    level_4 = xmlValue(r[[4]]),
    code_4 = xmlGetAttr(r[[4]], name = "code"),
    parent_key = drug_key
  )
}

get_atc_codes_df <- function(rec) {
  return (map_df(xmlChildren(rec[["atc-codes"]]),
                 ~ get_atc_codes_rec(.x,
                                     xmlValue(rec["drugbank-id"][[1]]))))
}


#' Extracts the drug atc codes element and return data as data frame.
#'
#' \code{parse_drug_atc_codes} returns data frame of drug atc codes elements.
#'
#' This functions extracts the atc codes element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug atc_codes node attributs date frame
#'
#' @examples
#' parse_drug_atc_codes()
#' parse_drug_atc_codes(FALSE)
#' parse_drug_atc_codes(save_table = FALSE)
#' @export
parse_drug_atc_codes <- function(save_table = TRUE) {
  drug_atc_codes <- map_df(pkg.env$children, ~ get_atc_codes_df(.x))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_atc_codes,
                  table_name = "drug_atc_codes")
  }
  return(drug_atc_codes)
}
