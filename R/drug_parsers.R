#' Extracts the main drug elements and return data as tibble.
#'
#' \code{drug} returns tibble of drugs main elements.
#'
#' This functions extracts the main element of drug node in drugbank
#' xml database with the option to save it in a user defined database.
#' It takes two optional arguments to save the returned tibble in the database
#' \code{save_table} and \code{database_connection}.
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
#' @return drug main node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug()
#'
#' # will throw an error, as database_connection is NULL
#' drug(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug(save_table = TRUE, save_csv = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' #  and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drugs", csv_path)
    if (!override_csv & file.exists(path)) {
      drugs <- readr::read_csv(path)
    } else {
      drugs <- xmlSApply(xmlRoot(pkg_env$root), drug_row)
      drugs <- as_tibble(t(drugs))
      write_csv(drugs, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drugs,
        table_name = "drug",
        primary_key = "primary_key",
        foreign_key = NULL,
        field_types = list(
          primary_key = paste0("varchar(", max(nchar(
            drugs$primary_key
          )), ")"),
          other_keys = paste0("varchar(",
                              max(nchar(
                                drugs$other_keys
                              ), na.rm = TRUE), ")"),
          type = paste0("varchar(", max(nchar(drugs$type), na.rm = TRUE), ")"),
          name = paste0("varchar(", max(nchar(drugs$name), na.rm = TRUE), ")"),
          description = paste0("varchar(", max(nchar(
            drugs$description
          ),
          na.rm = TRUE) + 10, ")"),
          cas_number = paste0("varchar(", max(nchar(
            drugs$cas_number
          ),
          na.rm = TRUE), ")"),
          unii = paste0("varchar(", max(nchar(drugs$unii), na.rm = TRUE), ")"),
          state = paste0("varchar(", max(nchar(drugs$state), na.rm = TRUE),
                         ")"),
          mechanism_of_action = "varchar(MAX)",
          pharmacodynamics = "varchar(MAX)",
          indication = paste0("varchar(", max(nchar(
            drugs$indication
          ), na.rm = TRUE) + 10, ")"),
          absorption = paste0("varchar(", max(nchar(
            drugs$absorption
          ), na.rm = TRUE) + 10, ")"),
          route_of_elimination = paste0("varchar(", max(
            nchar(drugs$route_of_elimination),
            na.rm = TRUE
          ) + 10, ")"),
          metabolism = paste0("varchar(", max(nchar(
            drugs$metabolism
          ), na.rm = TRUE) + 10, ")"),
          international_brands = paste0("varchar(", max(
            nchar(drugs$international_brands),
            na.rm = TRUE
          ) + 10, ")"),
          fda_label = paste0("varchar(", max(nchar(
            drugs$fda_label
          ), na.rm = TRUE), ")"),
          msds = paste0("varchar(", max(nchar(drugs$msds), na.rm = TRUE), ")"),
          protein_binding = paste0("varchar(", max(
            nchar(drugs$protein_binding),
            na.rm = TRUE
          ) + 10, ")"),
          synthesis_reference = paste0("varchar(", max(
            nchar(drugs$synthesis_reference),
            na.rm = TRUE
          ) + 10, ")"),
          clearance = paste0("varchar(", max(nchar(
            drugs$clearance
          ), na.rm = TRUE) + 10, ")"),
          half_life = paste0("varchar(", max(nchar(
            drugs$half_life
          ), na.rm = TRUE) + 10, ")"),
          volume_of_distribution = paste0("varchar(", max(
            nchar(drugs$volume_of_distribution),
            na.rm = TRUE
          ) + 10, ")"),
          toxicity = "varchar(MAX)"
        )
      )
    }

    return(drugs %>% as_tibble())
  }

#' Extracts the drug groups element and return data as tibble.
#'
#' \code{drug_groups} returns tibble of drug groups elements.
#'
#' This functions extracts the groups element of drug node in drugbank
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
#' @return drug groups node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_groups()
#'
#' # will throw an error, as database_connection is NULL
#' drug_groups(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_groups(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_groups(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_groups(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_groups(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_groups(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_groups <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_groups", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_groups <- readr::read_csv(path)
    } else {
      drug_groups <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "groups")) %>% unique()
      names(drug_groups) <- c("group", "drugbank-id")
      write_csv(drug_groups, save_csv, csv_path)
    }


    if (nrow(drug_groups) > 0) {
      colnames(drug_groups) <- c("group", "drugbank_id")
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_groups,
                    table_name = "drug_groups")
    }
    return(drug_groups %>% as_tibble())
  }

#' Extracts the drug articles element and return data as tibble.
#'
#' \code{drug_articles} returns tibble of drug articles elements.
#'
#' This functions extracts the articles element of drug node in drugbank
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
#' @return drug articles node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_articles()
#'
#' # will throw an error, as database_connection is NULL
#' drug_articles(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_articles(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_articles(save_csv = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_articles(save_table = TRUE, database_connection = sqlite_con)
#' # save in database, save parsed tibble as csv if it
#' # does not exist in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_articles(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_articles(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_articles(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_articles", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_articles <- readr::read_csv(path)
    } else {
      drug_articles <-
        map_df(
          pkg_env$children,
          ~ drug_sub_df(.x, "general-references",
                        seconadary_node = "articles")
        ) %>% unique()

      write_csv(drug_articles, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_articles,
                    table_name = "drug_articles")
    }
    return(drug_articles %>% as_tibble())
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

#' Extracts the drug links element and return data as tibble.
#'
#' \code{drug_links} returns tibble of drug links elements.
#'
#' This functions extracts the links element of drug node in drugbank
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
#' @return drug links node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_links()
#'
#' # will throw an error, as database_connection is NULL
#' drug_links(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_links(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_links(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not
#' # exist in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_links(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_links <- readr::read_csv(path)
    } else {
      drug_links <-
        map_df(
          pkg_env$children,
          ~ drug_sub_df(.x, "general-references", seconadary_node = "links")
        ) %>% unique()

      write_csv(drug_links, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_links,
                    table_name = "drug_links")
    }
    return(drug_links %>% as_tibble())
  }


#' Extracts the drug syn element and return data as tibble.
#'
#' \code{drug_syn} returns tibble of drug syn elements.
#'
#' This functions extracts the syn element of drug node in drugbank
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
#' @return drug syn node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_syn()
#'
#' # will throw an error, as database_connection is NULL
#' drug_syn(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_syn(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_syn(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_syn(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_syn(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_syn(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_syn", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_syn <- readr::read_csv(path)
    } else {
      drug_syn <-
        map_df(pkg_env$children, ~ get_syn_df(.x)) %>% unique()
      write_csv(drug_syn, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_syn,
        table_name = "drug_syn",
        field_types = list(synonym = "varchar(534)")
      )
    }
    return(drug_syn %>% as_tibble())
  }

#' Extracts the drug products element and return data as tibble.
#'
#' \code{drug_products} returns tibble of drug products elements.
#'
#' This functions extracts the products element of drug node in drugbank
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
#' @return drug products node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_products()
#'
#' # will throw an error, as database_connection is NULL
#' drug_products(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_products(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_products(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_products(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_products(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_products(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_products <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_products", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_products <- readr::read_csv(path)
    } else {
      drug_products <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "products")) %>% unique()
      write_csv(drug_products, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_products,
                    table_name = "drug_products")
    }
    return(drug_products %>% as_tibble())
  }

#' Extracts the drug calculated properties element and return data as tibble.
#'
#' \code{drug_calc_prop} returns tibble of drug calculated
#' properties elements.
#'
#' This functions extracts the calculated properties element of drug node in
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
#' @return drug calculated properties node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_calc_prop()
#'
#' # will throw an error, as database_connection is NULL
#' drug_calc_prop(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_calc_prop(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_calc_prop(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_calc_prop(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_calc_prop(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_calc_prop(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_calc_prop <- function(save_table = FALSE,
                           save_csv = FALSE,
                           csv_path = ".",
                           override_csv = FALSE,
                           database_connection = NULL) {
  check_parameters_validation(save_table, database_connection)
  path <-
    get_dataset_full_path("drug_calculated_properties", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_calculated_properties <- readr::read_csv(path)
  } else {
    drug_calculated_properties <-
      map_df(pkg_env$children,
             ~ drug_sub_df(.x, "calculated-properties")) %>% unique()

    write_csv(drug_calculated_properties, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(con = database_connection,
                  df = drug_calculated_properties,
                  table_name = "drug_calculated_properties")
  }
  return(drug_calculated_properties %>% as_tibble())
}

#' Extracts the drug international brands and return data as tibble.
#'
#' \code{drug_intern_brand} returns tibble of drug products
#' elements.
#'
#' This functions extracts the international brands element of drug node in
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
#'  the new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug international brands node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_intern_brand()
#'
#' # will throw an error, as database_connection is NULL
#' drug_intern_brand(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_intern_brand(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_intern_brand(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' #  current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_intern_brand(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_intern_brand(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_intern_brand(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_intern_brand <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_international_brands", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_international_brands <- readr::read_csv(path)
    } else {
      drug_international_brands <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "international-brands")) %>%
        unique()

      write_csv(drug_international_brands, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_international_brands,
                    table_name = "international_brands")
    }
    return(drug_international_brands %>% as_tibble())
  }

#' Extracts the drug salts and return data as tibble.
#'
#' \code{drug_salts} returns tibble of drug products elements.
#'
#' This functions extracts the salts element of drug node in drugbank
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
#' @param override_csv override existing csv, if any, in case it is true in t
#' he new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug salts node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_salts()
#'
#' # will throw an error, as database_connection is NULL
#' drug_salts(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_salts(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_salts(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_salts(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_salts(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_salts(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_salts <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_salts", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_salts <- readr::read_csv(path)
    } else {
      drug_salts <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "salts")) %>%
        unique()

      write_csv(drug_salts, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_salts,
                    table_name = "salts")
    }
    return(drug_salts %>% as_tibble())
  }

#' Extracts the drug mixtures element and return data as tibble.
#'
#' \code{drug_mixtures} returns tibble of drug mixtures elements.
#'
#' This functions extracts the mixtures element of drug node in drugbank
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
#' @return drug mixtures node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_mixtures()
#'
#' # will throw an error, as database_connection is NULL
#' drug_mixtures(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_mixtures(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_mixtures(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_mixtures(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_mixtures(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_mixtures(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_mixtures <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_mixtures", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_mixtures <- readr::read_csv(path)
    } else {
      drug_mixtures <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "mixtures")) %>% unique()

      write_csv(drug_mixtures, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_mixtures,
                    table_name = "drug_mixtures")
    }
    return(drug_mixtures %>% as_tibble())
  }

#' Extracts the drug packagers element and return data as tibble.
#'
#' \code{drug_packagers} returns tibble of drug packagers elements.
#'
#' This functions extracts the packagers element of drug node in drugbank
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
#' @return drug packagers node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_packagers()
#'
#' # will throw an error, as database_connection is NULL
#' drug_packagers(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_packagers(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_packagers(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not
#' # exist in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_packagers(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_packagers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_packagers(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_packagers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_packagers", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_packagers <- readr::read_csv(path)
    } else {
      drug_packagers <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "packagers")) %>% unique()
      write_csv(drug_packagers, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_packagers,
                    table_name = "drug_packagers")
    }
    return(drug_packagers %>% as_tibble())
  }


#' Extracts the drug categories element and return data as tibble.
#'
#' \code{drug_categories} returns tibble of drug categories elements.
#'
#' This functions extracts the categories element of drug node in drugbank
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
#' @return drug categories node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_categories()
#'
#' # will throw an error, as database_connection is NULL
#' drug_categories(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_categories(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_categories(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' #  in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_categories(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_categories(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_categories(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_categories <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_categories", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_categories <- readr::read_csv(path)
    } else {
      drug_categories <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "categories")) %>% unique()
      write_csv(drug_categories, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_categories,
                    table_name = "drug_categories")
    }
    return(drug_categories %>% as_tibble())
  }

#' Extracts the drug affected organisms element and return data as tibble.
#'
#' \code{drug_affected_organisms} returns tibble of drug affected
#' organisms elements.
#'
#' This functions extracts the affected organisms element of drug node in
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
#' @return drug affected organisms node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_affected_organisms()
#'
#' # will throw an error, as database_connection is NULL
#' drug_affected_organisms(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_affected_organisms(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_affected_organisms(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_affected_organisms(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_affected_organisms(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_affected_organisms(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_affected_organisms <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_affected_organisms", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_affected_organisms <- readr::read_csv(path)
    } else {
      drug_affected_organisms <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "affected-organisms")) %>%
        unique()
      write_csv(drug_affected_organisms, save_csv, csv_path)
    }


    if (nrow(drug_affected_organisms) > 0) {
      colnames(drug_affected_organisms) <-
        c("affected_organism", "drugbank_id")
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_affected_organisms,
                    table_name = "drug_affected_organisms")
    }
    return(drug_affected_organisms %>% as_tibble())
  }

#' Extracts the drug dosages element and return data as tibble.
#'
#' \code{drug_dosages} returns tibble of drug dosages elements.
#'
#' This functions extracts the dosages element of drug node in drugbank
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
#' @return drug dosages node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_dosages()
#'
#' # will throw an error, as database_connection is NULL
#' drug_dosages(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_dosages(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_dosages(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_dosages(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' #  and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_dosages(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_dosages(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_dosages <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_dosages", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_dosages <- readr::read_csv(path)
    } else {
      drug_dosages <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "dosages")) %>% unique()

      write_csv(drug_dosages, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_dosages,
                    table_name = "drug_dosages")
    }
    return(drug_dosages %>% as_tibble())
  }


#' Extracts the drug ahfs codes element and return data as tibble.
#'
#' \code{drug_ahfs_codes} returns tibble of drug ahfs codes elements.
#'
#' This functions extracts the ahfs codes element of drug node in drugbank
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
#' @return drug ahfs codes node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_ahfs_codes()
#'
#' # will throw an error, as database_connection is NULL
#' drug_ahfs_codes(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_ahfs_codes(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ahfs_codes(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ahfs_codes(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ahfs_codes(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_ahfs_codes(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_ahfs_codes <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_ahfs_codes", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_ahfs_codes <- readr::read_csv(path)
    } else {
      drug_ahfs_codes <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "ahfs-codes")) %>% unique()
      write_csv(drug_ahfs_codes, save_csv, csv_path)
    }

    if (nrow(drug_ahfs_codes) > 0) {
      colnames(drug_ahfs_codes) <- c("ahfs_code", "drugbank_id")
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_ahfs_codes,
                    table_name = "drug_ahfs_codes")
    }
    return(drug_ahfs_codes %>% as_tibble())
  }

#' Extracts the drug pdb entries element and return data as tibble.
#'
#' \code{drug_pdb_entries} returns tibble of drug pdb entries elements.
#'
#' This functions extracts the pdb entries element of drug node in drugbank
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
#' @return drug pdb entries node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_pdb_entries()
#'
#' # will throw an error, as database_connection is NULL
#' drug_pdb_entries(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_pdb_entries(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pdb_entries(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pdb_entries(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pdb_entries(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_pdb_entries(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_pdb_entries <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_pdb_entries", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_pdb_entries <- readr::read_csv(path)
    } else {
      drug_pdb_entries <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "pdb-entries")) %>% unique()
      write_csv(drug_pdb_entries, save_csv, csv_path)
    }

    if (nrow(drug_pdb_entries) > 0) {
      colnames(drug_pdb_entries) <- c("pdb_entry", "drugbank_id")
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_pdb_entries,
                    table_name = "drug_pdb_entries")
    }
    return(drug_pdb_entries %>% as_tibble())
  }

#' Extracts the drug patents element and return data as tibble.
#'
#' \code{drug_patents} returns tibble of drug patents elements.
#'
#' This functions extracts the patents element of drug node in drugbank
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
#' @return drug patents node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_patents()
#'
#' # will throw an error, as database_connection is NULL
#' drug_patents(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_patents(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_patents(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # 'in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_patents(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_patents(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_patents(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_patents <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_patents", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_patents <- readr::read_csv(path)
    } else {
      drug_patents <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "patents")) %>% unique()

      write_csv(drug_patents, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_patents,
                    table_name = "drug_patents")
    }
    return(drug_patents %>% as_tibble())
  }

#' Extracts the drug food interactions element and return data as tibble.
#'
#' \code{drug_food_interactions} returns tibble of drug food
#' interactions elements.
#'
#' This functions extracts the food interactions element of drug node in
#' drugbank xml database with the option to save it in a predefined database via
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
#' @return drug food interactions node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_food_interactions()
#'
#' # will throw an error, as database_connection is NULL
#' drug_food_interactions(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_food_interactions(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_food_interactions(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_food_interactions(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_food_interactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' #  and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_food_interactions(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_food_interactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_food_interactions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_food_interactions <- readr::read_csv(path)
    } else {
      drug_food_interactions <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "food-interactions")) %>%
        unique()
      write_csv(drug_food_interactions, save_csv, csv_path)
    }



    if (nrow(drug_food_interactions) > 0) {
      colnames(drug_food_interactions) <-
        c("food_interaction", "drugbank_id")
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_food_interactions,
                    table_name = "drug_food_interactions")
    }
    return(drug_food_interactions %>% as_tibble())
  }

#' Extracts the drug interactions element and return data as tibble.
#'
#' \code{drug_interactions} returns tibble of drug interactions elements.
#'
#' This functions extracts the interactions element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table}
#' and \code{database_connection}.
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
#' @return drug interactions node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_interactions()
#'
#' # will throw an error, as database_connection is NULL
#' drug_interactions(save_table = TRUE)
#'
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_interactions(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_interactions(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_interactions(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_interactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_interactions(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_interactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_drug_interactions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_drug_interactions <- readr::read_csv(path)
    } else {
      drug_drug_interactions <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "drug-interactions")) %>%
        unique()

      write_csv(drug_drug_interactions, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_drug_interactions,
                    table_name = "drug_drug_interactions")
    }
    return(drug_drug_interactions %>% as_tibble())
  }

#' Extracts the drug experimental properties element and return data as tibble.
#'
#' \code{drug_exp_prop} returns tibble of drug
#'  experimental
#'  properties elements.
#'
#' This functions extracts the experimental properties element of drug node in
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
#' @return drug experimental properties node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_exp_prop()
#'
#' # will throw an error, as database_connection is NULL
#' drug_exp_prop(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_exp_prop(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_exp_prop(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_exp_prop(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_exp_prop(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_exp_prop(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_exp_prop <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_experimental_properties", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_experimental_properties <- readr::read_csv(path)
    } else {
      drug_experimental_properties <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "experimental-properties")) %>% unique()
      write_csv(drug_experimental_properties, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_experimental_properties,
                    table_name = "drug_experimental_properties")
    }
    return(drug_experimental_properties %>% as_tibble())
  }

#' Extracts the drug external identifiers element and return data as tibble.
#'
#' \code{drug_ex_identity} returns tibble of external
#' identifiers groups elements.
#'
#' This functions extracts the external identifiers element of drug node in
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
#' @return drug external identifiers node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_ex_identity()
#'
#' # will throw an error, as database_connection is NULL
#' drug_ex_identity(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_ex_identity(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_ex_identity(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_ex_identity <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_external_identifiers", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_external_identifiers <- readr::read_csv(path)
    } else {
      drug_external_identifiers <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "external-identifiers"))

      write_csv(drug_external_identifiers, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_external_identifiers,
                    table_name = "drug_external_identifiers")
    }
    return(drug_external_identifiers %>% as_tibble())
  }

#' Extracts the drug external links element and return data as tibble.
#'
#' \code{drug_external_links} returns tibble of drug external links
#' elements.
#'
#' This functions extracts the external links element of drug node in drugbank
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
#' @return drug external links node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_external_links()
#'
#' # will throw an error, as database_connection is NULL
#' drug_external_links(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_external_links(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_external_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_external_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_external_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_external_links <- readr::read_csv(path)
    } else {
      drug_external_links <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "external-links")) %>%
        unique()

      write_csv(drug_external_links, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_external_links,
                    table_name = "drug_external_links")
    }
    return(drug_external_links %>% as_tibble())
  }

#' Extracts the drug snp effects element and return data as tibble.
#'
#' \code{drug_snp_effects} returns tibble of snp effects groups elements.
#'
#' This functions extracts the snp effects element of drug node in drugbank
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
#' @return drug snp effects node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_snp_effects()
#'
#' # will throw an error, as database_connection is NULL
#' drug_snp_effects(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_snp_effects(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_snp_effects(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_snp_effects <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_snp_effects", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_snp_effects <- readr::read_csv(path)
    } else {
      drug_snp_effects <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "snp-effects")) %>% unique()

      write_csv(drug_snp_effects, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_snp_effects,
                    table_name = "drug_snp_effects")
    }
    return(drug_snp_effects %>% as_tibble())
  }

#' Extracts the drug snp adverse drug reactions element and return data as
#' tibble.
#'
#' \code{drug_snp_adverse_reactions } returns tibble of drug
#'  snp adverse drug reactions elements.
#'
#' This functions extracts the groups element of drug node in drugbank
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
#' @return drug snp adverse drug reactions node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_snp_adverse_reactions()
#'
#' # will throw an error, as database_connection is NULL
#' drug_snp_adverse_reactions(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_snp_adverse_reactions(save_table = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_snp_adverse_reactions(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_snp_adverse_reactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("snp_adverse_reactions", csv_path)
    if (!override_csv & file.exists(path)) {
      snp_adverse_reactions <- readr::read_csv(path)
    } else {
      snp_adverse_reactions <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "snp-adverse-drug-reactions")) %>% unique()

      write_csv(snp_adverse_reactions, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = snp_adverse_reactions,
                    table_name = "snp_adverse_reactions")
    }
    return(snp_adverse_reactions %>% as_tibble())
  }
