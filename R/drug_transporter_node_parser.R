get_transporters_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~ get_organizm_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

get_transporters_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]), ~ drug_sub_df(.x,
                                                                  "actions",
                                                                  id = "id")))
}

get_transporters_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_transporters_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_transporters_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}

get_transporters_polypeptide_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]), ~ get_polypeptide_rec(.x)))
}

get_transporters_polypeptide_external_identifiers_df <-
  function(rec) {
    return(map_df(
      xmlChildren(rec[["transporters"]]),
      ~ get_polypeptide_external_identifiers(.x)
    ))
  }

get_transporters_polypeptide_synonyms_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~ get_polypeptide_synonyms(.x)))
}

get_transporters_polypeptide_pfams_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~ get_polypeptide_pfams(.x)))
}

get_transporters_polypeptide_go_classifiers_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~ get_polypeptide_go_classifiers(.x)))
}

#' Extracts the drug transporters actions element and return data as data frame.
#'
#' \code{parse_drug_transporters actions} returns data frame of drug
#'  transporters
#' actions elements.
#'
#' This functions extracts the transporters actions element of drug node in
#' drugbank
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
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in
#'  the new parse operation
#' @return drug transporters actions node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_transporters_actions()
#'
#' # save in database and return parsed dataframe
#' parse_drug_transporters_actions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_actions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_actions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_actions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' #  and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_transporters_actions(save_csv = TRUE, csv_path = TRUE,
#'  override = TRUE)
#' }
#' @export
parse_drug_transporters_actions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporters_actions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters_actions <- readr::read_csv(path)
    } else {
      drug_transporters_actions <-
        map_df(pkg_env$children, ~ get_transporters_actions_df(.x)) %>%
        unique()

      write_csv(drug_transporters_actions, save_csv, csv_path)
    }

    if (nrow(drug_transporters_actions) > 0) {
      colnames(drug_transporters_actions) <- c("action", "transporter_id")
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters_actions,
        table_name = "drug_transporters_actions",
        save_table_only = TRUE
      )
    }
    return(drug_transporters_actions)
  }


#' Extracts the drug transporters articles element and return data as
#' data frame.
#'
#' \code{parse_drug_transporters articles} returns data frame of drug
#' transporters articles elements.
#'
#' This functions extracts the transporters articles element of drug node in
#'  drugbank
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
#'  new parse operation
#' @return drug transporters articles node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_transporters_articles()
#'
#' # save in database and return parsed dataframe
#' parse_drug_transporters_articles(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_articles(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_articles(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_articles(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_transporters_articles(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_transporters_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporters_articles", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters_articles <- readr::read_csv(path)
    } else {
      drug_transporters_articles <-
        map_df(pkg_env$children, ~ get_transporters_articles_df(.x)) %>%
        unique()

      write_csv(drug_transporters_articles, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters_articles,
        table_name = "drug_transporters_articles",
        save_table_only = TRUE
      )
    }
    return(drug_transporters_articles)
  }


#' Extracts the drug transporters textbooks element and return data as
#'  data frame.
#'
#' \code{parse_drug_transporters_textbooks} returns data frame of drug
#' transporters
#'  textbooks elements.
#'
#' This functions extracts the transporters textbooks element of drug node in
#' drugbank
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
#'  new parse operation
#' @return drug transporters textbooks node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_transporters_textbooks()
#'
#' # save in database and return parsed dataframe
#' parse_drug_transporters_textbooks(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_textbooks(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' #  and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_transporters_textbooks(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_transporters_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporters_textbooks", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters_textbooks <- readr::read_csv(path)
    } else {
      drug_transporters_textbooks <-
        map_df(pkg_env$children, ~ get_transporters_textbooks_df(.x)) %>%
        unique()

      write_csv(drug_transporters_textbooks, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters_textbooks,
        table_name = "drug_transporters_textbooks",
        save_table_only = TRUE
      )
    }
    return(drug_transporters_textbooks)
  }


#' Extracts the drug transporters links element and return data as data frame.
#'
#' \code{parse_drug_transporters_links} returns data frame of drug
#' transporters_links elements.
#'
#' This functions extracts the transporters links element of drug node in
#' drugbank
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
#'  new parse operation
#' @return drug transporters links node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_transporters_links()
#'
#' # save in database and return parsed dataframe
#' parse_drug_transporters_links(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_links(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_links(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_transporters_links(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_transporters_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporters_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters_links <- readr::read_csv(path)
    } else {
      drug_transporters_links <-
        map_df(pkg_env$children, ~ get_transporters_links_df(.x)) %>%
        unique()

      write_csv(drug_transporters_links, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters_links,
        table_name = "drug_transporters_links",
        save_table_only = TRUE
      )
    }
    return(drug_transporters_links)
  }


#' Extracts the drug transporters polypeptides element and return data
#'  as data frame.
#'
#' \code{parse_drug_transporters_polypeptides} returns data frame of
#' transporters polypeptides groups elements.
#'
#' This functions extracts the transporters polypeptides element of drug
#' node in drug bank
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
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @return drug transporters polypeptides node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_transporters_polypeptides()
#'
#' # save in database and return parsed dataframe
#' parse_drug_transporters_polypeptides(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_polypeptides(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_polypeptides(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_polypeptides(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_transporters_polypeptides(save_csv = TRUE, csv_path = TRUE,
#'  override = TRUE)
#' }
#' @export
parse_drug_transporters_polypeptides <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporters_polypeptides", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters_polypeptides <- readr::read_csv(path)
    } else {
      drug_transporters_polypeptides <-
        map_df(pkg_env$children, ~ get_transporters_polypeptide_df(.x)) %>%
        unique()

      write_csv(drug_transporters_polypeptides, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters_polypeptides,
        table_name = "drug_transporters_polypeptides",
        save_table_only = TRUE,
        field_types = list(
          general_function =
            paste("varchar(",
                  max(
                    nchar(drug_transporters_polypeptides$general_function),
                    na.rm = TRUE
                  ), ")", sep = ""),
          specific_function =
            paste("varchar(",
                  max(
                    nchar(drug_transporters_polypeptides$specific_function),
                    na.rm = TRUE
                  ), ")", sep = ""),
          amino_acid_sequence =
            paste("varchar(",
                  max(
                    nchar(drug_transporters_polypeptides$amino_acid_sequence),
                    na.rm = TRUE
                  ), ")", sep = ""),
          gene_sequence = paste("varchar(max)", sep = "")
        )
      )
    }
    return(drug_transporters_polypeptides)
  }


#' Extracts the drug transporters polypeptides external identifiers
#'  element and return data as data frame.
#'
#' \code{parse_drug_transporters_polypeptides_external_identifiers}
#' returns data frame of drug transporters polypeptides external identifiers
#' elements.
#'
#' This functions extracts the transporters polypeptides external
#'  identifiers element of drug node in drug bank
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
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @return drug transporters polypeptides external identifiers
#'  node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_transporters_polypeptides_external_identifiers()
#'
#' # save in database and return parsed dataframe
#' parse_drug_transporters_polypeptides_external_identifiers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_polypeptides_external_identifiers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_polypeptides_external_identifiers(
#' save_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters_polypeptides_external_identifiers(
#' save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_transporters_polypeptides_external_identifiers(
#' save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_transporters_polypeptides_external_identifiers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path(
        "drug_transporters_polypeptide_external_identifiers",
        csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters_polypeptide_external_identifiers <-
        readr::read_csv(path)
    } else {
      drug_transporters_polypeptide_external_identifiers <-
        map_df(pkg_env$children,
               ~ get_transporters_polypeptide_external_identifiers_df(.x)) %>%
        unique()

      write_csv(drug_transporters_polypeptide_external_identifiers,
                save_csv,
                csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters_polypeptide_external_identifiers,
        table_name = "drug_transporters_polypeptides_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(drug_transporters_polypeptide_external_identifiers)
  }


#' Extracts the drug transporters polypeptides synonyms
#' element and return data as data frame.
#'
#' \code{parse_trans_poly_syn} returns data
#' frame of drug transporters polypeptides synonyms elements.
#'
#' This functions extracts the transporters polypeptides synonyms
#'  element of drug node in drug bank
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
#'  new parse operation
#' @return drug transporters polypeptides synonyms node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_trans_poly_syn()
#'
#' # save in database and return parsed dataframe
#' parse_trans_poly_syn(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_syn(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_syn(ssave_table = TRUE,
#' save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_syn(save_csv = TRUE,
#'  csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_trans_poly_syn(save_csv = TRUE,
#'  csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_trans_poly_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporter_polypeptide_synonyms", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporter_polypeptide_synonyms <- readr::read_csv(path)
    } else {
      drug_transporter_polypeptide_synonyms <-
        map_df(pkg_env$children,
               ~ get_transporters_polypeptide_synonyms_df(.x)) %>%
        unique()

      write_csv(drug_transporter_polypeptide_synonyms,
                save_csv,
                csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporter_polypeptide_synonyms,
        table_name = "drug_transporters_polypeptides_synonyms",
        save_table_only = TRUE
      )
    }
    return(drug_transporter_polypeptide_synonyms)
  }


#' Extracts the drug transporters polypeptides pfams element
#' and return data as data frame.
#'
#' \code{parse_trans_poly_pfams} returns data frame
#'  of drug transporters polypeptides pfams elements.
#'
#' This functions extracts the transporters polypeptides pfams
#'  element of drug node in drug bank
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
#'  new parse operation
#' @return drug transporters polypeptides pfams node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_trans_poly_pfams()
#'
#' # save in database and return parsed dataframe
#' parse_trans_poly_pfams(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_pfams(ssave_table = TRUE,
#' save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_trans_poly_pfams(save_csv = TRUE, csv_path = TRUE,
#'  override = TRUE)
#' }
#' @export
parse_trans_poly_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporters_polypeptides_pfams", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters_polypeptides_pfams <- readr::read_csv(path)
    } else {
      drug_transporters_polypeptides_pfams <-
        map_df(pkg_env$children,
               ~ get_transporters_polypeptide_pfams_df(.x)) %>%
        unique()

      write_csv(drug_transporters_polypeptides_pfams, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters_polypeptides_pfams,
        table_name = "drug_transporters_polypeptides_pfams",
        save_table_only = TRUE
      )
    }
    return(drug_transporters_polypeptides_pfams)
  }


#' Extracts the drug transporters polypeptides go
#' classifiers element and return data as data frame.
#'
#' \code{parse_trans_poly_go}
#' returns data frame of drug transporters polypeptides
#' go classifiers elements.
#'
#' This functions extracts the transporters polypeptides go
#'  classifiers element of drug node in drug bank
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
#' @return drug transporters polypeptides go classifiers node attributes
#' date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_trans_poly_go()
#'
#' # save in database and return parsed dataframe
#' parse_trans_poly_go(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_go(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_go(ssave_table = TRUE,
#' save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_trans_poly_go(save_csv = TRUE,
#' csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_trans_poly_go(
#' save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_trans_poly_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporters_polypeptides_go_classifiers",
                            csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters_polypeptides_go_classifiers <-
        readr::read_csv(path)
    } else {
      drug_transporters_polypeptides_go_classifiers <-
        map_df(pkg_env$children,
               ~ get_transporters_polypeptide_go_classifiers_df(.x)) %>%
        unique()

      write_csv(drug_transporters_polypeptides_go_classifiers,
                save_csv,
                csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters_polypeptides_go_classifiers,
        table_name = "drug_transporters_polypeptides_go_classifiers",
        save_table_only = TRUE
      )
    }
    return(drug_transporters_polypeptides_go_classifiers)
  }




#' Extracts the drug transporters element and return data as data frame.
#'
#' \code{parse_drug_transporters} returns data frame of drug transporters
#' elements.
#'
#' This functions extracts the transporters element of drug node in drug bank
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
#'  new parse operation
#' @return drug transporters node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_transporters()
#'
#' # save in database and return parsed dataframe
#' parse_drug_transporters(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_transporters(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_transporters(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_transporters <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_transporters", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_transporters <- readr::read_csv(path)
    } else {
      drug_transporters <-
        map_df(pkg_env$children, ~ get_transporters_df(.x)) %>%
        unique()

      write_csv(drug_transporters, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_transporters,
        table_name = "drug_transporters",
        foreign_key = "parent_key"
      )
    }
    return(drug_transporters)
  }
