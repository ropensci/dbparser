get_carriers_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_enzymes_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_targ_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_trans_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

#' Extracts the drug books element and return data as tibble.
#'
#' \code{drug_books} returns tibble of drug books elements.
#'
#' This functions extracts the books element of drug node in drugbank
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
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug books node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_books()
#'
#' # will throw an error, as database_connection is NULL
#' drug_books(save_table = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_books(save_csv = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_books(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save in database, save parsed tibble as csv if it does not
#' # exist in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_books(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given l
#' # ocation and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_books(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_books(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_books <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_books", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_books <- readr::read_csv(path)
    } else {
      drug_books <-
        map_df(
          pkg_env$children,
          ~ drug_sub_df(.x, "general-references", seconadary_node = "textbooks")
        ) %>% unique()

      write_csv(drug_books, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_books,
                    table_name = "drug_books")
    }
    return(drug_books %>% as_tibble())
  }

#' Extracts the drug carriers textbooks element and return data as tibble.
#'
#' \code{carriers_textbooks} returns tibble of drug carriers
#'  textbooks elements.
#'
#' This functions extracts the carriers textbooks element of drug node in
#' drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#'  \code{database_connection}.
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
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug carriers textbooks node attributes date frame
#'
#' @family carriers
#'
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' carriers_textbooks()
#'
#' # will throw an error, as database_connection is NULL
#' carriers_textbooks(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' carriers_textbooks(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_textbooks(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist override it and return it.
#' carriers_textbooks(
#'   save_csv = TRUE, csv_path = TRUE, override =
#'     TRUE
#' )
#' }
#' @export
carriers_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_carriers_textbooks", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_carriers_textbooks <- readr::read_csv(path)
    } else {
      drug_carriers_textbooks <-
        map_df(pkg_env$children, ~ get_carriers_textbooks_df(.x)) %>% unique()

      write_csv(drug_carriers_textbooks, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_carriers_textbooks,
        table_name = "drug_carriers_textbooks",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_textbooks %>% as_tibble())
  }

#' Extracts the drug enzymes textbooks element and return data as tibble.
#'
#' \code{enzymes_textbooks} returns tibble of drug enzymes
#' textbooks elements.
#'
#' This functions extracts the enzymes textbooks element of drug node in
#' drugbank
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
#' @return drug enzymes textbooks node attributes date frame
#' @family enzymes
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' enzymes_textbooks()
#'
#' # will throw an error, as database_connection is NULL
#' enzymes_textbooks(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' enzymes_textbooks(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' enzymes_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' enzymes_textbooks(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' enzymes_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' enzymes_textbooks(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
enzymes_textbooks <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  check_parameters_validation(save_table, database_connection)
  path <-
    get_dataset_full_path("drug_enzymes_textbooks", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_enzymes_textbooks <- readr::read_csv(path)
  } else {
    drug_enzymes_textbooks <-
      map_df(pkg_env$children, ~ get_enzymes_textbooks_df(.x)) %>%
      unique()

    write_csv(drug_enzymes_textbooks, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(
      con = database_connection,
      df = drug_enzymes_textbooks,
      table_name = "drug_enzymes_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_enzymes_textbooks %>% as_tibble())
}

#' Extracts the drug targ textbooks element and return data as tibble.
#'
#' \code{targets_textbooks} returns tibble of drug
#'  targ textbooks elements.
#'
#' This functions extracts the targ textbooks element of drug node in
#'  drugbank
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
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug targ textbooks node attributes date frame
#' @family targets
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' targets_textbooks()
#'
#' # will throw an error, as database_connection is NULL
#' targets_textbooks(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' targets_textbooks(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' targets_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not
#' # exist in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' targets_textbooks(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' targets_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' targets_textbooks(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
targets_textbooks <- function(save_table = FALSE, save_csv = FALSE,
                              csv_path = ".", override_csv = FALSE,
                              database_connection = NULL) {
  check_parameters_validation(save_table, database_connection)
  path <-
    get_dataset_full_path("drug_targ_textbooks", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targ_textbooks <- readr::read_csv(path)
  } else {
    drug_targ_textbooks <-
      map_df(pkg_env$children, ~ get_targ_textbooks_df(.x)) %>% unique()

    write_csv(drug_targ_textbooks, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = database_connection,
      df = drug_targ_textbooks,
      table_name = "drug_targ_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_targ_textbooks %>% as_tibble())
}

#' Extracts the drug transporters textbooks element and return data as
#'  tibble.
#'
#' \code{transporters_textbooks} returns tibble of drug
#' transporters
#'  textbooks elements.
#'
#' This functions extracts the transporters textbooks element of drug node in
#' drugbank
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
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug transporters textbooks node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' transporters_textbooks()
#'
#' # will throw an error, as database_connection is NULL
#' transporters_textbooks(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' transporters_textbooks(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_textbooks(save_table = TRUE, save_csv = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' #  and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' transporters_textbooks(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
transporters_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_trans_textbooks", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_trans_textbooks <- readr::read_csv(path)
    } else {
      drug_trans_textbooks <-
        map_df(pkg_env$children, ~ get_trans_textbooks_df(.x)) %>%
        unique()

      write_csv(drug_trans_textbooks, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_trans_textbooks,
        table_name = "drug_trans_textbooks",
        save_table_only = TRUE
      )
    }
    return(drug_trans_textbooks %>% as_tibble())
  }
