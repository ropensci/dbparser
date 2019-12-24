# Extract drug manufacturers df
get_manufacturer_rec <- function(r, drug_key) {
  tibble(
    name = xmlValue(r),
    url = xmlGetAttr(r, name = "url"),
    generic = xmlGetAttr(r, name = "generic"),
    parent_key = drug_key
  )
}

get_manufactures_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["manufacturers"]]),
    ~ get_manufacturer_rec(., xmlValue(rec["drugbank-id"][[1]]))
  ))
}

#' Extracts the drug manufacturers element and return data as data frame.
#'
#' \code{drug_manufacturers} returns data frame of drug manufacturers
#' elements.
#'
#' This functions extracts the manufacturers element of drug node in
#'  \strong{DrugBank}
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed dataframe if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @return drug manufacturers node attributes date frame
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' drug_manufacturers()
#'
#' # save in database and return parsed dataframe
#' drug_manufacturers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_manufacturers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_manufacturers(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_manufacturers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' drug_manufacturers(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_manufacturers <- function(save_table = FALSE, save_csv = FALSE,
                                     csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_manufacturers", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_manufacturers <- readr::read_csv(path)
  } else {
    drug_manufacturers <-
      map_df(pkg_env$children, ~ drug_sub_df(.x, "manufacturers")) %>%
      unique()
    write_csv(drug_manufacturers, save_csv, csv_path)
  }

  if (nrow(drug_manufacturers) > 0) {
    colnames(drug_manufacturers) <- c("manufacturer", "drugbank_id")
  }


  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_manufacturers,
      table_name = "drug_manufacturers"
    )
  }
  return(drug_manufacturers)
}
