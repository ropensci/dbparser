get_carriers_actions_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "actions", id = "id")
  ))
}

get_carriers_polypeptide_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]), ~ get_polypeptide_rec(.x)))
}

get_carr_poly_ex_identity_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ get_poly_ex_identity(.x)
  ))
}

get_carr_poly_syn_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]), ~ get_polypeptide_syn(.x)))
}

get_carr_poly_pfams_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]), ~ get_polypeptide_pfams(.x)))
}

get_carr_poly_go_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ get_polypeptide_go(.x)
  ))
}

#' Extracts the drug carriers actions element and return data as tibble.
#'
#' \code{carriers_actions} returns tibble of drug
#'  carriers actions elements.
#'
#' This functions extracts the carriers actions element of drug
#'  node in \strong{DrugBank}
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
#' @return drug carriers actions node attributes date frame
#'
#' @family carriers
#'
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' carriers_actions()
#'
#' # will throw an error, as database_connection is NULL
#' carriers_actions(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' carriers_actions(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_actions(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_actions(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_actions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist override it and return it.
#'
#' carriers_actions(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
carriers_actions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_carriers_actions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_carriers_actions <- readr::read_csv(path)
    } else {
      drug_carriers_actions <-
        map_df(pkg_env$children, ~ get_carriers_actions_df(.x)) %>% unique()

      write_csv(drug_carriers_actions, save_csv, csv_path)
    }


    if (nrow(drug_carriers_actions) > 0) {
      colnames(drug_carriers_actions) <- c("action", "carrier_id")
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_carriers_actions,
        table_name = "drug_carriers_actions",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_actions %>% as_tibble())
  }

#' Extracts the drug carriers polypeptides element and return data as
#' tibble.
#'
#' \code{carriers_polypeptides} returns tibble of drug carriers
#' polypeptides elements.
#'
#' This functions extracts the carriers polypeptides element of drug
#' node in \strong{DrugBank}
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
#' @param override_csv override existing csv, if any, in case it is true
#' in the new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug carriers polypeptides node attributes date frame
#'
#' @family carriers
#'
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' carriers_polypeptide()
#'
#' # will throw an error, as database_connection is NULL
#' carriers_polypeptide(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' carriers_polypeptide(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' carriers_polypeptide(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
carriers_polypeptide <- function(save_table = FALSE,
                            save_csv = FALSE,
                            csv_path = ".",
                            override_csv = FALSE,
                            database_connection = NULL) {
  check_parameters_validation(save_table, database_connection)
  path <-
    get_dataset_full_path("drug_carriers_polypeptides", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_carriers_polypeptides <- readr::read_csv(path)
  } else {
    drug_carriers_polypeptides <-
      map_df(pkg_env$children, ~ get_carriers_polypeptide_df(.x)) %>% unique()

    write_csv(drug_carriers_polypeptides, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = database_connection,
      df = drug_carriers_polypeptides,
      table_name = "drug_carriers_polypeptides",
      save_table_only = TRUE,
      field_types = list(
        general_function =
          paste("varchar(",
            max(
              nchar(drug_carriers_polypeptides$general_function),
              na.rm = TRUE
            ), ")",
            sep = ""
          ),
        specific_function =
          paste("varchar(",
            max(
              nchar(drug_carriers_polypeptides$specific_function),
              na.rm = TRUE
            ), ")",
            sep = ""
          ),
        amino_acid_sequence =
          paste("varchar(",
            max(
              nchar(drug_carriers_polypeptides$amino_acid_sequence),
              na.rm = TRUE
            ), ")",
            sep = ""
          ),
        gene_sequence = paste("varchar(max)", sep = "")
      )
    )
  }
  return(drug_carriers_polypeptides %>% as_tibble())
}

#' Extracts the drug carriers polypeptides external identifiers
#'  element and return data as tibble.
#'
#' \code{carriers_polypeptide_ext_id } returns
#'  tibble of drug carriers polypeptides external identifiers elements.
#'
#' This functions extracts the carriers polypeptides external identifiers
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
#' @param override_csv override existing csv, if any, in case it is true in
#' the new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug carriers polypeptides external identifiers
#'  node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' carriers_polypeptide_ext_id()
#'
#' # will throw an error, as database_connection is NULL
#' carriers_polypeptide_ext_id(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' carriers_polypeptide_ext_id(save_table = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide_ext_id(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide_ext_id(
#'   save_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide_ext_id(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' carriers_polypeptide_ext_id(
#'   save_csv = TRUE, csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
carriers_polypeptide_ext_id <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path(
        "carr_poly_ex_identitys",
        csv_path
      )
    if (!override_csv & file.exists(path)) {
      carr_poly_ex_identitys <-
        readr::read_csv(path)
    } else {
      carr_poly_ex_identitys <-
        map_df(
          pkg_env$children,
          ~ get_carr_poly_ex_identity_df(.x)
        ) %>%
        unique()

      write_csv(
        carr_poly_ex_identitys,
        save_csv,
        csv_path
      )
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = carr_poly_ex_identitys,
        table_name = "drug_carriers_polypeptides_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(carr_poly_ex_identitys %>% as_tibble())
  }

#' Extracts the drug carriers polypeptides syn element and return data as
#' tibble.
#'
#' \code{carriers_polypeptides_syn} returns
#'  tibble of drug carriers polypeptides syn elements.
#'
#' This functions extracts the carriers polypeptides syn
#'  element of drug node in \strong{DrugBank}
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
#' @param override_csv override existing csv, if any, in case it is true
#' in the new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug carriers polypeptides syn node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' carriers_polypeptides_syn()
#'
#' # will throw an error, as database_connection is NULL
#' carriers_polypeptides_syn(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' carriers_polypeptides_syn(save_table = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_syn(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_syn(
#'   save_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_syn(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' carriers_polypeptides_syn(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
carriers_polypeptides_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path(
      "carr_poly_syn",
      csv_path
    )
    if (!override_csv & file.exists(path)) {
      carr_poly_syn <- readr::read_csv(path)
    } else {
      carr_poly_syn <-
        map_df(
          pkg_env$children,
          ~ get_carr_poly_syn_df(.x)
        ) %>%
        unique()

      write_csv(carr_poly_syn, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = carr_poly_syn,
        table_name = "drug_carriers_polypeptides_syn",
        save_table_only = TRUE
      )
    }
    return(carr_poly_syn %>% as_tibble())
  }

#' Extracts the drug carriers polypeptides pfams element and return data as
#' tibble.
#'
#' \code{carriers_polypeptides_pfams} returns tibble
#'  of drug carriers polypeptides pfams elements.
#'
#' This functions extracts the carriers polypeptides pfams element of
#'  drug node in \strong{DrugBank}
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
#' @return drug carriers polypeptides pfams node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' carriers_polypeptides_pfams()
#'
#' # will throw an error, as database_connection is NULL
#' carriers_polypeptides_pfams(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' carriers_polypeptides_pfams(save_table = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' #  current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_pfams(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' carriers_polypeptides_pfams(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
carriers_polypeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("carr_poly_pfams", csv_path)
    if (!override_csv & file.exists(path)) {
      carr_poly_pfams <- readr::read_csv(path)
    } else {
      carr_poly_pfams <-
        map_df(
          pkg_env$children,
          ~ get_carr_poly_pfams_df(.x)
        ) %>%
        unique()

      write_csv(carr_poly_pfams, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = carr_poly_pfams,
        table_name = "carr_poly_pfams",
        save_table_only = TRUE
      )
    }
    return(carr_poly_pfams %>% as_tibble())
  }

#' Extracts the drug carriers polypeptides go classifiers
#'  element and return data as tibble.
#'
#' \code{carriers_polypeptides_go} returns
#' tibble of drug carriers polypeptides go classifiers elements.
#'
#' This functions extracts the carriers polypeptides go
#' classifiers element of drug node in \strong{DrugBank}
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
#' @return drug carriers polypeptides go classifiers node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' carriers_polypeptides_go()
#'
#' # will throw an error, as database_connection is NULL
#' carriers_polypeptides_go(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' carriers_polypeptides_go(save_table = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_go(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_go(
#'   save_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptides_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' carriers_polypeptides_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
carriers_polypeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path(
        "carr_poly_go",
        csv_path
      )
    if (!override_csv & file.exists(path)) {
      carr_poly_go <- readr::read_csv(path)
    } else {
      carr_poly_go <-
        map_df(
          pkg_env$children,
          ~ get_carr_poly_go_df(.x)
        ) %>% unique()

      write_csv(
        carr_poly_go,
        save_csv,
        csv_path
      )
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = carr_poly_go,
        table_name = "carr_poly_go",
        save_table_only = TRUE
      )
    }
    return(carr_poly_go %>% as_tibble())
  }
