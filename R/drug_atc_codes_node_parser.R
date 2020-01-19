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
  return(map_df(
    xmlChildren(rec[["atc-codes"]]),
    ~ get_atc_codes_rec(
      .x,
      xmlValue(rec["drugbank-id"][[1]])
    )
  ))
}


#' Extracts the drug atc codes element and return data as tibble.
#'
#' \code{drug_atc_codes} returns tibble of drug atc codes elements.
#'
#' This functions extracts the atc codes element of drug node in
#'  \strong{DrugBank} xml database with the option to save it in a predefined
#'   database via \code{\link{open_db}} method.
#' It takes one single optional argument to
#' save the returned tibble in the database.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#' new parse operation
#' @return drug atc_codes node attributes date frame
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_atc_codes()
#'
#' # save in database and return parsed tibble
#' drug_atc_codes(save_table = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_atc_codes(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' current
#' #  location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_atc_codes(save_table = TRUE, save_csv = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_atc_codes(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_atc_codes(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_atc_codes <- function(save_table = FALSE, save_csv = FALSE,
                                 csv_path = ".", override_csv = FALSE) {
  check_data_and_connection(save_table)
  path <- get_dataset_full_path("drug_atc_codes", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_atc_codes <- readr::read_csv(path)
  } else {
    drug_atc_codes <- map_df(pkg_env$children, ~
    get_atc_codes_df(.x)) %>% unique()
    write_csv(drug_atc_codes, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_atc_codes,
      table_name = "drug_atc_codes"
    )
  }
  return(drug_atc_codes)
}
