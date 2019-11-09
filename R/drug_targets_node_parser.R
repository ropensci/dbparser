get_targets_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]),
                ~ get_organizm_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

get_targets_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]),
                ~ drug_sub_df(.x, "actions", id = "id")))
}

get_targets_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_targets_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_targets_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}

get_targets_polypeptide_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]), ~ get_polypeptide_rec(.x)))
}

get_targets_polypeptide_external_identifiers_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["targets"]]),
    ~ get_polypeptide_external_identifiers(.x)
  ))
}

get_targets_polypeptide_synonyms_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]), ~ get_polypeptide_synonyms(.x)))
}

get_targets_polypeptide_pfams_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]), ~ get_polypeptide_pfams(.x)))
}

get_targets_polypeptide_go_classifiers_df <- function(rec) {
  return(map_df(xmlChildren(rec[["targets"]]),
                ~ get_polypeptide_go_classifiers(.x)))
}

#' Extracts the drug targets polypeptides external identifiers
#'  element and return data as data frame.
#'
#' \code{parse_drug_targets_polypeptides_external_identifiers}
#'  returns data frame of drug targets polypeptides external identifiers
#'  elements.
#'
#' This functions extracts the targets polypeptides external identifiers
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
#' @return drug targets polypeptides external identifiers node attributes
#' date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_polypeptides_external_identifiers()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_polypeptides_external_identifiers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_external_identifiers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_external_identifiers(ssave_table = TRUE,
#' save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_external_identifiers(save_csv = TRUE,
#'  csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_polypeptides_external_identifiers(
#' save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_targets_polypeptides_external_identifiers <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_targets_polypeptide_external_identifiers",
                            csv_path)
    if (!override_csv & file.exists(path)) {
      drug_targets_polypeptide_external_identifiers <- readr::read_csv(path)
    } else {
      drug_targets_polypeptide_external_identifiers <-
        map_df(pkg.env$children,
               ~ get_targets_polypeptide_external_identifiers_df(.x)) %>%
        unique()

      write_csv(drug_targets_polypeptide_external_identifiers, save_csv,
                csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_targets_polypeptide_external_identifiers,
        table_name = "drug_targets_polypeptides_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(drug_targets_polypeptide_external_identifiers)
  }


#' Extracts the drug targets polypeptides synonyms element
#' and return data as data frame.
#'
#' \code{parse_drug_targets_polypeptides_synonyms} returns data
#'  frame of drug targets polypeptides synonyms elements.
#'
#' This functions extracts the targets polypeptides synonyms element of
#' drug node in drug bank
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
#' @return drug targets polypeptides synonyms node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_polypeptides_synonyms()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_polypeptides_synonyms(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_synonyms(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_synonyms(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_synonyms(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_polypeptides_synonyms(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_targets_polypeptides_synonyms <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_targets_polypeptide_synonyms", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_targets_polypeptide_synonyms <- readr::read_csv(path)
    } else {
      drug_targets_polypeptide_synonyms <-
        map_df(pkg.env$children,
               ~ get_targets_polypeptide_synonyms_df(.x)) %>%
        unique()

      write_csv(drug_targets_polypeptide_synonyms, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_targets_polypeptide_synonyms,
        table_name = "drug_targets_polypeptides_synonyms",
        save_table_only = TRUE
      )
    }
    return(drug_targets_polypeptide_synonyms)
  }


#' Extracts the drug targets polypeptides pfams
#'  element and return data as data frame.
#'
#' \code{parse_drug_targets_polypeptides_pfams} returns data frame of
#'  drug targets polypeptides pfams elements.
#'
#' This functions extracts the targets polypeptides pfams element of drug node
#' in drug bank
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
#' @return drug targets polypeptides pfams node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_polypeptides_pfams()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_polypeptides_pfams(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#'  current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_pfams(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' #  return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_polypeptides_pfams(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_targets_polypeptides_pfams <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_targets_polypeptide_pfams", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_targets_polypeptide_pfams <- readr::read_csv(path)
    } else {
      drug_targets_polypeptide_pfams <-
        map_df(pkg.env$children,
               ~ get_targets_polypeptide_pfams_df(.x)) %>%
        unique()

      write_csv(drug_targets_polypeptide_pfams, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_targets_polypeptide_pfams,
        table_name = "drug_targets_polypeptides_pfams",
        save_table_only = TRUE
      )
    }
    return(drug_targets_polypeptide_pfams)
  }


#' Extracts the drug targets polypeptides go classifiers
#'  element and return data as data frame.
#'
#' \code{parse_drug_targets_polypeptides_go_classifiers}
#'  returns data frame of drug targets polypeptides go classifiers elements.
#'
#' This functions extracts the targets polypeptides go classifiers
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
#' @return drug targets polypeptides go classifiers node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_polypeptides_go_classifiers()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_polypeptides_go_classifiers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_go_classifiers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_go_classifiers(ssave_table = TRUE,
#'  save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides_go_classifiers(save_csv = TRUE,
#' csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_polypeptides_go_classifiers(save_csv = TRUE,
#' csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_targets_polypeptides_go_classifiers <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_targets_polypeptides_go_classifiers",
                            csv_path)
    if (!override_csv & file.exists(path)) {
      drug_targets_polypeptides_go_classifiers <- readr::read_csv(path)
    } else {
      drug_targets_polypeptides_go_classifiers <-
        map_df(pkg.env$children,
               ~ get_targets_polypeptide_go_classifiers_df(.x)) %>%
        unique()

      write_csv(drug_targets_polypeptides_go_classifiers, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_targets_polypeptides_go_classifiers,
        table_name = "drug_targets_polypeptides_go_classifiers",
        save_table_only = TRUE
      )
    }
    return(drug_targets_polypeptides_go_classifiers)
  }

#' Extracts the drug targets actions element and return data as data frame.
#'
#' \code{parse_drug_targets_actions} returns data frame of drug targets
#' actions elements.
#'
#' This functions extracts the targets actions element of drug node in drugbank
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
#' @return drug targets actions node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_actions()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_actions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_actions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_actions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_actions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_actions(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_targets_actions <- function(save_table = FALSE, save_csv = FALSE,
                                       csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targets_actions", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targets_actions <- readr::read_csv(path)
  } else {
    drug_targets_actions <-
      map_df(pkg.env$children, ~ get_targets_actions_df(.x)) %>%
      unique()
    write_csv(drug_targets_actions, save_csv, csv_path)
  }


  if (nrow(drug_targets_actions) > 0) {
    colnames(drug_targets_actions) <- c("action", "target_id")
  }


  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets_actions,
      table_name = "drug_targets_actions",
      save_table_only = TRUE
    )
  }
  return(drug_targets_actions)
}

#' Extracts the drug targets articles element and return data as data frame.
#'
#' \code{parse_drug_targets_articles} returns data frame of drug targets
#' articles elements.
#'
#' This functions extracts the targets articles element of drug node in drugbank
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
#' @return drug targets articles node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_articles()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_articles(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_articles(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_articles(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_articles(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_articles(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_targets_articles <- function(save_table = FALSE, save_csv = FALSE,
                                        csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targets_articles", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targets_articles <- readr::read_csv(path)
  } else {
    drug_targets_articles <-
      map_df(pkg.env$children, ~ get_targets_articles_df(.x)) %>% unique()

    write_csv(drug_targets_articles, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets_articles,
      table_name = "drug_targets_articles",
      save_table_only = TRUE
    )
  }
  return(drug_targets_articles)
}

#' Extracts the drug targets textbooks element and return data as data frame.
#'
#' \code{parse_drug_targets_textbooks} returns data frame of drug
#'  targets textbooks elements.
#'
#' This functions extracts the targets textbooks element of drug node in
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
#' @return drug targets textbooks node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_textbooks()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_textbooks(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_textbooks(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_textbooks(save_csv = TRUE, csv_path = TRUE,
#'  override = TRUE)
#' }
#' @export
parse_drug_targets_textbooks <- function(save_table = FALSE, save_csv = FALSE,
                                         csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targets_textbooks", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targets_textbooks <- readr::read_csv(path)
  } else {
    drug_targets_textbooks <-
      map_df(pkg.env$children, ~ get_targets_textbooks_df(.x)) %>% unique()

    write_csv(drug_targets_textbooks, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets_textbooks,
      table_name = "drug_targets_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_targets_textbooks)
}

#' Extracts the drug targets links element and return data as data frame.
#'
#' \code{parse_drug_targets_links} returns data frame of drug targets links
#'  elements.
#'
#' This functions extracts the targets links element of drug node in drug bank
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
#' @return drug targets_links node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_links()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_links(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_links(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_links(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_targets_links <- function(save_table = FALSE, save_csv = FALSE,
                                     csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targets_links", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targets_links <- readr::read_csv(path)
  } else {
    drug_targets_links <- map_df(pkg.env$children, ~
                                   get_targets_links_df(.x)) %>% unique()

    write_csv(drug_targets_links, save_csv, csv_path)
  }



  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets_links,
      table_name = "drug_targets_links",
      save_table_only = TRUE,
      field.types = list(
        title = paste("varchar(",
                      max(nchar(
                        drug_targets_links$title
                      ), na.rm = TRUE) + 100, ")", sep = ""),
        url = paste("varchar(", max(nchar(
          drug_targets_links$url
        )) + 100, ")", sep = "")
      )
    )
  }
  return(drug_targets_links)
}

#' Extracts the drug targets polypeptides element and return data as data frame.
#'
#' \code{parse_drug_targets_polypeptides} returns data frame of drug targets
#'  polypeptides elements.
#'
#' This functions extracts the targets polypeptides element of drug node
#' in drug bank
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
#' @return drug targets polypeptides node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets_polypeptides()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets_polypeptides(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets_polypeptides(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets_polypeptides(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_targets_polypeptides <- function(save_table = FALSE,
                                            save_csv = FALSE, csv_path = ".",
                                            override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targets_polypeptides", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targets_polypeptides <- readr::read_csv(path)
  } else {
    drug_targets_polypeptides <-
      map_df(pkg.env$children, ~ get_targets_polypeptide_df(.x)) %>%
      unique()

    write_csv(drug_targets_polypeptides, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets_polypeptides,
      table_name = "drug_targets_polypeptides",
      save_table_only = TRUE,
      field.types = list(
        general_function =
          paste("varchar(",
                max(nchar(drug_targets_polypeptides$general_function),
                    na.rm = TRUE), ")", sep = ""),
        specific_function = paste("varchar(max)", sep = ""),
        amino_acid_sequence = paste("varchar(max)", sep = ""),
        gene_sequence = paste("varchar(max)", sep = "")
      )
    )
  }
  return(drug_targets_polypeptides)
}


#' Extracts the drug targets element and return data as data frame.
#'
#' \code{parse_drug_targets} returns data frame of drug targets elements.
#'
#' This functions extracts the target element of drug node in drug bank
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
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_targets()
#'
#' # save in database and return parsed dataframe
#' parse_drug_targets(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_targets(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' #  location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_targets(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_targets <- function(save_table = FALSE, save_csv = FALSE,
                               csv_path = ".", override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_targets", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targets <- readr::read_csv(path)
  } else {
    drug_targets <-
      map_df(pkg.env$children, ~ get_targets_df(.x)) %>%
      unique()

    write_csv(drug_targets, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets,
      table_name = "drug_targets",
      foreign_key = "parent_key"
    )
  }
  return(drug_targets)
}
