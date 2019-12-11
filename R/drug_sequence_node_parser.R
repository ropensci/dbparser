# Extract drug sequences df
get_sequence_rec <- function(r, drug_key) {
  tibble(
    sequence = xmlValue(r),
    format = ifelse(is.null(xmlGetAttr(r, name = "format")),
      NA,
      xmlGetAttr(r, name = "format")
    ),
    parent_key = drug_key
  )
}

get_sequences_df <- function(rec) {
  if (is.null(rec[["sequences"]])) {
    return()
  }
  return(map_df(
    xmlChildren(rec[["sequences"]]),
    ~ get_sequence_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
  ))
}


#' Extracts the drug sequences element and return data as data frame.
#'
#' \code{parse_drug_sequences} returns data frame of drug sequences elements.
#'
#' This functions extracts the sequences element of drug node in \strong{DrugBank}
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
#' @return drug sequences node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_sequences()
#'
#' # save in database and return parsed dataframe
#' parse_drug_sequences(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_sequences(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_sequences(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_sequences(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_sequences(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_sequences <- function(save_table = FALSE, save_csv = FALSE,
                                 csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_sequences", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_sequences <- readr::read_csv(path)
  } else {
    drug_sequences <-
      map_df(pkg_env$children, ~ get_sequences_df(.x)) %>%
      unique()

    write_csv(drug_sequences, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_sequences,
      table_name = "drug_sequences"
    )
  }
  return(drug_sequences)
}
