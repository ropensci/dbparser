# Extract drug classifications df
drug_classifications_df <- function(rec) {
  if (is.null(rec[["classification"]])) {
    return()
  }
  a <- xmlToList(rec[["classification"]])
  return(tibble(
    parent_key = xmlValue(rec["drugbank-id"][[1]]),
    classifications = paste(names(a), a, collapse = ";")
  ))
}

#' Extracts the drug classifications element and return data as data frame.
#'
#' \code{parse_drug_classification} returns data frame of drug classifications
#'  elements.
#'
#' This functions extracts the classifications element of drug node in \strong{DrugBank}
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed dataframe if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in
#' the new parse operation
#' @return drug classifications node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_classification()
#'
#' # save in database and return parsed dataframe
#' parse_drug_classification(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_classification(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_classification(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' #  and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_classification(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_classification(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_classification <- function(save_table = FALSE, save_csv = FALSE,
                                      csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_classifications", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_classifications <- readr::read_csv(path)
  } else {
    drug_classifications <-
      map_df(pkg_env$children, ~ drug_classifications_df(.x)) %>%
      unique()

    write_csv(drug_classifications, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_classifications,
      table_name = "drug_classifications"
    )
  }
  return(drug_classifications)
}
