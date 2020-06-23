get_trans_poly_ex_identity_df <-
  function(rec) {
    return(map_df(
      xmlChildren(rec[["transporters"]]),
      ~ get_poly_ex_identity(.x)
    ))
  }

get_trans_poly_syn_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ get_polypeptide_syn(.x)
  ))
}

get_trans_poly_pfams_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ get_polypeptide_pfams(.x)
  ))
}

get_trans_poly_go_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ get_polypeptide_go(.x)
  ))
}

#' Extracts the drug transporters polypeptides external identifiers
#'  element and return data as tibble.
#'
#' \code{transporters_polypep_ex_ident}
#' returns tibble of drug transporters polypeptides external identifiers
#' elements.
#'
#' This functions extracts the transporters polypeptides external
#'  identifiers element of drug node in \strong{DrugBank}
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
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug transporters polypeptides external identifiers
#'  node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' transporters_polypep_ex_ident()
#'
#' # will throw an error, as database_connection is NULL
#' transporters_polypep_ex_ident(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' transporters_textbooks(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypep_ex_ident(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypep_ex_ident(
#'   save_table = TRUE, save_csv = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypep_ex_ident(
#'   save_csv = TRUE, csv_path = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' transporters_polypep_ex_ident(
#'   save_csv = TRUE, csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
transporters_polypep_ex_ident <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path(
        "drug_trans_poly_ex_identity",
        csv_path
      )
    if (!override_csv & file.exists(path)) {
      drug_trans_poly_ex_identity <-
        readr::read_csv(path)
    } else {
      drug_trans_poly_ex_identity <-
        map_df(
          pkg_env$children,
          ~ get_trans_poly_ex_identity_df(.x)
        ) %>%
        unique()

      write_csv(
        drug_trans_poly_ex_identity,
        save_csv,
        csv_path
      )
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_trans_poly_ex_identity,
        table_name = "drug_trans_polys_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(drug_trans_poly_ex_identity %>% as_tibble())
  }


#' Extracts the drug transporters polypeptides syn
#' element and return data as tibble.
#'
#' \code{transporters_polypeptide_syn} returns data
#' frame of drug transporters polypeptides syn elements.
#'
#' This functions extracts the transporters polypeptides syn
#'  element of drug node in \strong{DrugBank}
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
#' @return drug transporters polypeptides syn node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' transporters_polypeptide_syn()
#'
#' # will throw an error, as database_connection is NULL
#' transporters_polypeptide_syn(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' transporters_textbooks(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_syn(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not
#' # exist in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_syn(
#'   save_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_syn(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' transporters_polypeptide_syn(
#'   save_csv = TRUE,
#'   csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
transporters_polypeptide_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_trans_poly_syn", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_trans_poly_syn <- readr::read_csv(path)
    } else {
      drug_trans_poly_syn <-
        map_df(
          pkg_env$children,
          ~ get_trans_poly_syn_df(.x)
        ) %>%
        unique()

      write_csv(
        drug_trans_poly_syn,
        save_csv,
        csv_path
      )
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_trans_poly_syn,
        table_name = "drug_trans_polys_syn",
        save_table_only = TRUE
      )
    }
    return(drug_trans_poly_syn %>% as_tibble())
  }


#' Extracts the drug transporters polypeptides pfams element
#' and return data as tibble.
#'
#' \code{transporters_polypeptide_pfams} returns tibble
#'  of drug transporters polypeptides pfams elements.
#'
#' This functions extracts the transporters polypeptides pfams
#'  element of drug node in \strong{DrugBank}
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
#' @return drug transporters polypeptides pfams node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' transporters_polypeptide_pfams()
#'
#' # will throw an error, as database_connection is NULL
#' transporters_polypeptide_pfams(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' transporters_textbooks(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not
#' # exist in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_pfams(
#'   save_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' transporters_polypeptide_pfams(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
transporters_polypeptide_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_trans_polys_pfams", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_trans_polys_pfams <- readr::read_csv(path)
    } else {
      drug_trans_polys_pfams <-
        map_df(
          pkg_env$children,
          ~ get_trans_poly_pfams_df(.x)
        ) %>%
        unique()

      write_csv(drug_trans_polys_pfams, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_trans_polys_pfams,
        table_name = "drug_trans_polys_pfams",
        save_table_only = TRUE
      )
    }
    return(drug_trans_polys_pfams %>% as_tibble())
  }


#' Extracts the drug transporters polypeptides go
#' classifiers element and return data as tibble.
#'
#' \code{transporters_polypeptide_go}
#' returns tibble of drug transporters polypeptides
#' go classifiers elements.
#'
#' This functions extracts the transporters polypeptides go
#'  classifiers element of drug node in \strong{DrugBank}
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
#' new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug transporters polypeptides go classifiers node attributes
#' date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' transporters_polypeptide_go()
#'
#' # will throw an error, as database_connection is NULL
#' transporters_polypeptide_go(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' transporters_textbooks(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_go(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' #  current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_go(
#'   save_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' transporters_polypeptide_go(
#'   save_csv = TRUE, csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
transporters_polypeptide_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path(
        "drug_trans_polys_go",
        csv_path
      )
    if (!override_csv & file.exists(path)) {
      drug_trans_polys_go <-
        readr::read_csv(path)
    } else {
      drug_trans_polys_go <-
        map_df(
          pkg_env$children,
          ~ get_trans_poly_go_df(.x)
        ) %>%
        unique()

      write_csv(
        drug_trans_polys_go,
        save_csv,
        csv_path
      )
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_trans_polys_go,
        table_name = "drug_trans_polys_go",
        save_table_only = TRUE
      )
    }
    return(drug_trans_polys_go %>% as_tibble())
  }
