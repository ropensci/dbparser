# Extract drug classifications df
drug_classifications_df <- function(rec) {
  if (is.null(rec[["classification"]])) {
    return()
  }
  class_elements <- names(rec[["classification"]])
  c(
    description = xmlValue(rec[["classification"]][["description"]]),
    direct_parent = xmlValue(rec[["classification"]][["direct-parent"]]),
    kingdom = xmlValue(rec[["classification"]][["kingdom"]]),
    superclass = xmlValue(rec[["classification"]][["superclass"]]),
    class = xmlValue(rec[["classification"]][["class"]]),
    subclass = xmlValue(rec[["classification"]][["subclass"]]),
    alternative_parents = paste(
      map_chr(rec[["classification"]][class_elements == "alternative-parent"],
                                        xmlValue), collapse = ";"),
    substituents = paste(
      map_chr(rec[["classification"]][class_elements == "substituent"],
                                 xmlValue), collapse = ";"),
    drugbank_id = xmlValue(rec[["drugbank-id"]])

  )
}

#' Extracts the drug classifications element and return data as tibble.
#'
#' \code{drug_classification} returns tibble of drug classifications
#'  elements.
#'
#' This functions extracts the classifications element of drug node in
#'  \strong{DrugBank}
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
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
#' @param override_csv override existing csv, if any, in case it is true in
#' the new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug classifications node attributes date frame
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_classification()
#'
#' # will throw an error, as database_connection is NULL
#' drug_classification(save_table = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_classification(save_csv = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_classification(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_classification(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' #  and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_classification(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_classification(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_classification <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_classifications", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_classifications <- readr::read_csv(path)
    } else {
      drug_classifications <-
        map_df(pkg_env$children, ~ drug_classifications_df(.x))

      write_csv(drug_classifications, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_classifications,
                    table_name = "drug_classifications",
                    field_types = list(
                      description = "varchar(MAX)",
                      direct_parent = paste0("varchar(", max(nchar(drug_classifications$direct_parent), na.rm = TRUE), ")"),
                      kingdom = paste0("varchar(", max(nchar(drug_classifications$kingdom), na.rm = TRUE), ")"),
                      superclass = paste0("varchar(", max(nchar(drug_classifications$superclass), na.rm = TRUE), ")"),
                      class = paste0("varchar(", max(nchar(drug_classifications$class), na.rm = TRUE), ")"),
                      subclass = paste0("varchar(", max(nchar(drug_classifications$subclass), na.rm = TRUE), ")"),
                      drugbank_id = paste0("varchar(", max(nchar(drug_classifications$drugbank_id), na.rm = TRUE), ")"),
                      substituents = "varchar(MAX)",
                      alternative_parents = "varchar(MAX)"
                    ))
    }
    return(drug_classifications)
  }
