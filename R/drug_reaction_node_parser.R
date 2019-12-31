# Extract drug reactions df
get_reactions_rec <- function(r, drug_key) {
  tibble(
    sequence = xmlValue(r[["sequence"]]),
    left_drugbank_id = xmlValue(r[["left-element"]][["drugbank-id"]]),
    left_drugbank_name = xmlValue(r[["left-element"]][["name"]]),
    right_drugbank_id = xmlValue(r[["right-element"]][["drugbank-id"]]),
    right_drugbank_name = xmlValue(r[["right-element"]][["name"]]),
    parent_key = drug_key
  )
}
get_reactions_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["reactions"]]),
    ~ get_reactions_rec(., xmlValue(rec["drugbank-id"][[1]]))
  ))
}

# Extract drug reactions enzymes df
get_reactions_enzymes_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["reactions"]]),
    ~ drug_sub_df(.x, "enzymes", id = NULL)
  ))
}

#' Extracts the drug reactions element and return data as data frame.
#'
#' \code{drug_reactions} returns data frame of drug reactions elements.
#'
#' This functions extracts the groups element of drug node in \strong{DrugBank}
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
#' @return drug reactions node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' drug_reactions()
#'
#' # save in database and return parsed dataframe
#' drug_reactions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_reactions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_reactions(save_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_reactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' drug_reactions(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_reactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_reactions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_reactions <- readr::read_csv(path)
    } else {
      drug_reactions <-
        map_df(pkg_env$children, ~ get_reactions_df(.x)) %>%
        unique()

      write_csv(drug_reactions, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_reactions,
        table_name = "drug_reactions",
        foreign_key = "parent_key"
      )
    }
    return(drug_reactions)
  }

#' Extracts the drug reactions enzymes element and return data as data frame.
#'
#' \code{drug_reactions_enzymes} returns data frame of drug reactions
#' enzymes elements.
#'
#' This functions extracts the reactions enzymes element of drug node in
#'  drugbank
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
#' @return drug reactions enzymes node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' drug_reactions_enzymes()
#'
#' # save in database and return parsed dataframe
#' drug_reactions_enzymes(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_reactions_enzymes(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_reactions_enzymes(save_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' #  given location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' drug_reactions_enzymes(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist i
#' # n current location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' drug_reactions_enzymes(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_reactions_enzymes <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_reactions_enzymes", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_reactions_enzymes <- readr::read_csv(path)
    } else {
      drug_reactions_enzymes <-
        map_df(pkg_env$children, ~ get_reactions_enzymes_df(.x)) %>%
        unique()

      write_csv(drug_reactions_enzymes, save_csv, csv_path)
    }



    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_reactions_enzymes,
        table_name = "drug_reactions_enzymes",
        save_table_only = TRUE
      )
    }
    return(drug_reactions_enzymes)
  }
