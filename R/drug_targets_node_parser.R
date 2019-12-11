get_targ_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ get_organizm_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
  ))
}

get_targ_actions_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "actions", id = "id")
  ))
}

get_targ_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_targ_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_targ_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}

get_targ_poly_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]), ~ get_polypeptide_rec(.x)))
}

get_targ_poly_ex_identity_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ get_poly_ex_identity(.x)
  ))
}

get_targ_poly_syn_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]), ~ get_polypeptide_syn(.x)))
}

get_targ_poly_pfams_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]), ~ get_polypeptide_pfams(.x)))
}

get_targ_poly_go_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ get_polypeptide_go(.x)
  ))
}

#' Extracts the drug targ polypeptides external identifiers
#'  element and return data as data frame.
#'
#' \code{parse_targ_poly_ext_identity}
#'  returns data frame of drug targ polypeptides external identifiers
#'  elements.
#'
#' This functions extracts the targ polypeptides external identifiers
#'  element of drug node in \strong{DrugBank}
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
#' @return drug targ polypeptides external identifiers node attributes
#' date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_targ_poly_ext_identity()
#'
#' # save in database and return parsed dataframe
#' parse_targ_poly_ext_identity(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_ext_identity(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_ext_identity(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_ext_identity(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_targ_poly_ext_identity(
#'   save_csv = TRUE, csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
parse_targ_poly_ext_identity <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path(
        "drug_targ_poly_ex_identity",
        csv_path
      )
    if (!override_csv & file.exists(path)) {
      drug_targ_poly_ex_identity <- readr::read_csv(path)
    } else {
      drug_targ_poly_ex_identity <-
        map_df(
          pkg_env$children,
          ~ get_targ_poly_ex_identity_df(.x)
        ) %>%
        unique()

      write_csv(
        drug_targ_poly_ex_identity, save_csv,
        csv_path
      )
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_targ_poly_ex_identity,
        table_name = "drug_targ_polys_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(drug_targ_poly_ex_identity)
  }


#' Extracts the drug targ polypeptides syn element
#' and return data as data frame.
#'
#' \code{parse_targ_poly_syn} returns data
#'  frame of drug targ polypeptides syn elements.
#'
#' This functions extracts the targ polypeptides syn element of
#' drug node in \strong{DrugBank}
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
#' @return drug targ polypeptides syn node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_targ_poly_syn()
#'
#' # save in database and return parsed dataframe
#' parse_targ_poly_syn(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_syn(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_syn(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_syn(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_targ_poly_syn(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_targ_poly_syn <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_targ_poly_syn", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_targ_poly_syn <- readr::read_csv(path)
    } else {
      drug_targ_poly_syn <-
        map_df(
          pkg_env$children,
          ~ get_targ_poly_syn_df(.x)
        ) %>%
        unique()

      write_csv(drug_targ_poly_syn, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_targ_poly_syn,
        table_name = "drug_targ_polys_syn",
        save_table_only = TRUE
      )
    }
    return(drug_targ_poly_syn)
  }


#' Extracts the drug targ polypeptides pfams
#'  element and return data as data frame.
#'
#' \code{parse_drug_targ_polys_pfams} returns data frame of
#'  drug targ polypeptides pfams elements.
#'
#' This functions extracts the targ polypeptides pfams element of drug node
#' in \strong{DrugBank}
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
#' @return drug targ polypeptides pfams node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_targ_polys_pfams()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targ_polys_pfams(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_polys_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_polys_pfams(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' #  return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_polys_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targ_polys_pfams(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_drug_targ_polys_pfams <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_targ_poly_pfams", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_targ_poly_pfams <- readr::read_csv(path)
    } else {
      drug_targ_poly_pfams <-
        map_df(
          pkg_env$children,
          ~ get_targ_poly_pfams_df(.x)
        ) %>%
        unique()

      write_csv(drug_targ_poly_pfams, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_targ_poly_pfams,
        table_name = "drug_targ_polys_pfams",
        save_table_only = TRUE
      )
    }
    return(drug_targ_poly_pfams)
  }


#' Extracts the drug targ polypeptides go classifiers
#'  element and return data as data frame.
#'
#' \code{parse_targ_poly_go}
#'  returns data frame of drug targ polypeptides go classifiers elements.
#'
#' This functions extracts the targ polypeptides go classifiers
#'  element of drug node in \strong{DrugBank}
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
#' @return drug targ polypeptides go classifiers node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_targ_poly_go()
#'
#' # save in database and return parsed dataframe
#' parse_targ_poly_go(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_go(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_go(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_targ_poly_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_targ_poly_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
parse_targ_poly_go <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path(
        "drug_targ_polys_go",
        csv_path
      )
    if (!override_csv & file.exists(path)) {
      drug_targ_polys_go <- readr::read_csv(path)
    } else {
      drug_targ_polys_go <-
        map_df(
          pkg_env$children,
          ~ get_targ_poly_go_df(.x)
        ) %>%
        unique()

      write_csv(drug_targ_polys_go, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_targ_polys_go,
        table_name = "drug_targ_polys_go",
        save_table_only = TRUE
      )
    }
    return(drug_targ_polys_go)
  }

#' Extracts the drug targ actions element and return data as data frame.
#'
#' \code{parse_drug_targ_actions} returns data frame of drug targ
#' actions elements.
#'
#' This functions extracts the targ actions element of drug node in drugbank
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
#' @return drug targ actions node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_targ_actions()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targ_actions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_actions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_actions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_actions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targ_actions(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_targ_actions <- function(save_table = FALSE, save_csv = FALSE,
                                    csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targ_actions", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targ_actions <- readr::read_csv(path)
  } else {
    drug_targ_actions <-
      map_df(pkg_env$children, ~ get_targ_actions_df(.x)) %>%
      unique()
    write_csv(drug_targ_actions, save_csv, csv_path)
  }


  if (nrow(drug_targ_actions) > 0) {
    colnames(drug_targ_actions) <- c("action", "target_id")
  }


  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_targ_actions,
      table_name = "drug_targ_actions",
      save_table_only = TRUE
    )
  }
  return(drug_targ_actions)
}

#' Extracts the drug targ articles element and return data as data frame.
#'
#' \code{parse_drug_targ_articles} returns data frame of drug targ
#' articles elements.
#'
#' This functions extracts the targ articles element of drug node in drugbank
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
#' @return drug targ articles node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_targ_articles()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targ_articles(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_articles(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_articles(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_articles(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targ_articles(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_drug_targ_articles <- function(save_table = FALSE, save_csv = FALSE,
                                     csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targ_articles", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targ_articles <- readr::read_csv(path)
  } else {
    drug_targ_articles <-
      map_df(pkg_env$children, ~ get_targ_articles_df(.x)) %>% unique()

    write_csv(drug_targ_articles, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_targ_articles,
      table_name = "drug_targ_articles",
      save_table_only = TRUE
    )
  }
  return(drug_targ_articles)
}

#' Extracts the drug targ textbooks element and return data as data frame.
#'
#' \code{parse_drug_targ_textbooks} returns data frame of drug
#'  targ textbooks elements.
#'
#' This functions extracts the targ textbooks element of drug node in
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
#' @return drug targ textbooks node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_targ_textbooks()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targ_textbooks(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_textbooks(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targ_textbooks(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_drug_targ_textbooks <- function(save_table = FALSE, save_csv = FALSE,
                                      csv_path = ".", override_csv = FALSE) {
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
      con = pkg_env$con,
      df = drug_targ_textbooks,
      table_name = "drug_targ_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_targ_textbooks)
}

#' Extracts the drug targ links element and return data as data frame.
#'
#' \code{parse_drug_targ_links} returns data frame of drug targ links
#'  elements.
#'
#' This functions extracts the targ links element of drug node in \strong{DrugBank}
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
#' @return drug targ_links node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_targ_links()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targ_links(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_links(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_links(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targ_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_targ_links <- function(save_table = FALSE, save_csv = FALSE,
                                  csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targ_links", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targ_links <- readr::read_csv(path)
  } else {
    drug_targ_links <- map_df(pkg_env$children, ~
    get_targ_links_df(.x)) %>% unique()

    write_csv(drug_targ_links, save_csv, csv_path)
  }



  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_targ_links,
      table_name = "drug_targ_links",
      save_table_only = TRUE,
      field_types = list(
        title = paste("varchar(",
          max(nchar(
            drug_targ_links$title
          ), na.rm = TRUE) + 100, ")",
          sep = ""
        ),
        url = paste("varchar(", max(nchar(
          drug_targ_links$url
        )) + 100, ")", sep = "")
      )
    )
  }
  return(drug_targ_links)
}

#' Extracts the drug targ polypeptides element and return data as data frame.
#'
#' \code{parse_drug_targ_polys} returns data frame of drug targ
#'  polypeptides elements.
#'
#' This functions extracts the targ polypeptides element of drug node
#' in \strong{DrugBank}
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
#' @return drug targ polypeptides node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_targ_polys()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targ_polys(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_polys(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_polys(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ_polys(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targ_polys(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_drug_targ_polys <- function(save_table = FALSE,
                                  save_csv = FALSE, csv_path = ".",
                                  override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targ_polys", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targ_polys <- readr::read_csv(path)
  } else {
    drug_targ_polys <-
      map_df(pkg_env$children, ~ get_targ_poly_df(.x)) %>%
      unique()

    write_csv(drug_targ_polys, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_targ_polys,
      table_name = "drug_targ_polys",
      save_table_only = TRUE,
      field_types = list(
        general_function =
          paste("varchar(",
            max(nchar(drug_targ_polys$general_function),
              na.rm = TRUE
            ), ")",
            sep = ""
          ),
        specific_function = paste("varchar(max)", sep = ""),
        amino_acid_sequence = paste("varchar(max)", sep = ""),
        gene_sequence = paste("varchar(max)", sep = "")
      )
    )
  }
  return(drug_targ_polys)
}


#' Extracts the drug targ element and return data as data frame.
#'
#' \code{parse_drug_targ} returns data frame of drug targ elements.
#'
#' This functions extracts the target element of drug node in \strong{DrugBank}
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
#' @return drug target node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_targ()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targ(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targ(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' #  location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targ(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_targ <- function(save_table = FALSE, save_csv = FALSE,
                            csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targ", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targ <- readr::read_csv(path)
  } else {
    drug_targ <-
      map_df(pkg_env$children, ~ get_targ_df(.x)) %>%
      unique()

    write_csv(drug_targ, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_targ,
      table_name = "drug_targ",
      foreign_key = "parent_key"
    )
  }
  return(drug_targ)
}
