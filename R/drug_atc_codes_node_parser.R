
atc_recs <- function(rec){
  if (xmlSize(rec[["atc-codes"]]) < 1) {
    return()
  }
  atcs <- xmlApply(rec[["atc-codes"]], atc_rec)
  atcs_tibble <- as_tibble(do.call(rbind, atcs))
  atcs_tibble[["drugbank-id"]] = xmlValue(rec[["drugbank-id"]])
  return(atcs_tibble)
}

atc_rec <- function(atc){
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

#' Extracts the drug atc codes element and return data as tibble.
#'
#' \code{drug_atc_codes} returns tibble of drug atc codes elements.
#'
#' This functions extracts the atc codes element of drug node in
#'  \strong{DrugBank} xml database with the option to save it in a predefined
#'   database via passed database connection.
#' It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
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
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug atc_codes node attributes date frame
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_atc_codes()
#'
#' # will throw an error, as database_connection is NULL
#' drug_atc_codes(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_atc_codes(save_table = TRUE, database_connection = sqlite_con)
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
#' drug_atc_codes(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
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
                                 csv_path = ".", override_csv = FALSE,
                           database_connection = NULL) {
  check_parameters_validation(save_table, database_connection)
  path <- get_dataset_full_path("drug_atc_codes", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_atc_codes <- readr::read_csv(path)
  } else {
    drug_atc_codes <- as_tibble(do.call(rbind,
                                        xmlApply(pkg_env$root, atc_recs)))
    write_csv(drug_atc_codes, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(
      con = database_connection,
      df = drug_atc_codes,
      table_name = "drug_atc_codes"
    )
  }
  return(drug_atc_codes)
}
