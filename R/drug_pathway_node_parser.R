# Extract drug pathways df
get_pathway_rec <- function(r, drug_key) {
  tibble(
    smpdb_id = xmlValue(r[["smpdb-id"]]),
    name = xmlValue(r[["name"]]),
    category = xmlValue(r[["category"]]),
    parent_key = drug_key
  )
}

get_pathways_df <- function(rec) {
  return(map_df(xmlChildren(rec[["pathways"]]),
                ~ get_pathway_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug pathways drugs df
get_pathways_drugs_df <- function(rec) {
  return(map_df(xmlChildren(rec[["pathways"]]),
                ~ drug_sub_df(.x, "drugs", id = "smpdb-id")))
}

# Extract drug pathways enzymes df
get_pathways_enzymes_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["pathways"]]),
    ~ drug_sub_df(.x, "enzymes", id = "smpdb-id")
  ))
}

#' Extracts the drug pathway enzyme element and return data as data frame.
#'
#' \code{parse_drug_pathway_enzyme} returns data frame of drug pathway enzyme
#'  elements.
#'
#' This functions extracts the pathway enzyme element of drug node in drugbank
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
#' @return drug pathway enzyme node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_pathway_enzyme()
#'
#' # save in database and return parsed dataframe
#' parse_drug_pathway_enzyme(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway_enzyme(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway_enzyme(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway_enzyme(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_pathway_enzyme(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_pathway_enzyme <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_pathway_enzymes", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_pathway_enzymes <- readr::read_csv(path)
    } else {
      drug_pathway_enzymes <-
        map_df(pkg.env$children, ~ get_pathways_enzymes_df(.x)) %>%
        unique()
      write_csv(drug_pathway_enzymes, save_csv, csv_path)
    }


    if (nrow(drug_pathway_enzymes) > 0) {
      colnames(drug_pathway_enzymes) <- c("enzyme", "pathway_id")
    }

    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_pathway_enzymes,
        table_name = "drug_pathway_enzyme",
        save_table_only = TRUE
      )
    }
    return(drug_pathway_enzymes)
  }

#' Extracts the drug pathway drugs element and return data as data frame.
#'
#' \code{parse_drug_pathway_drugs} returns data frame of drug pathway drugs
#'  elements.
#'
#' This functions extracts the pathway drugs element of drug node in drug bank
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
#' @return drug pathway drugs node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_pathway_drugs()
#'
#' # save in database and return parsed dataframe
#' parse_drug_pathway_drugs(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway_drugs(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway_drugs(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway_drugs(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_pathway_drugs(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_pathway_drugs <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_pathway_drugs", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_pathway_drugs <- readr::read_csv(path)
    } else {
      drug_pathway_drugs <-
        map_df(pkg.env$children, ~ get_pathways_drugs_df(.x)) %>%
        unique()

      write_csv(drug_pathway_drugs, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_pathway_drugs,
        table_name = "drug_pathway_drugs",
        save_table_only = TRUE
      )
    }
    return(drug_pathway_drugs)
  }

#' Extracts the drug pathway element and return data as data frame.
#'
#' \code{parse_drug_pathway} returns data frame of drug pathway elements.
#'
#' This functions extracts the groups element of drug node in drug bank
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
#' @param override_csv override existing csv, if any, in case it is true in the
#' new parse operation
#' @return drug pathway node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_pathway()
#'
#' # save in database and return parsed dataframe
#' parse_drug_pathway(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' #  exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pathway(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_pathway(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_pathway <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_pathway", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_pathway <- readr::read_csv(path)
    } else {
      drug_pathway <-
        map_df(pkg.env$children, ~ get_pathways_df(.x)) %>%
        unique()

      write_csv(drug_pathway, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg.env$con,
                    df = drug_pathway,
                    table_name = "drug_pathway")
    }
    return(drug_pathway)
  }
