get_trans_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ get_organizm_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
  ))
}

get_trans_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]), ~ drug_sub_df(.x,
    "actions",
    id = "id"
  )))
}

get_trans_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_trans_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_trans_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["transporters"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}

get_trans_poly_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]), ~ get_polypeptide_rec(.x)))
}

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

#' Extracts the drug transporters actions element and return data as data frame.
#'
#' \code{transporters actions} returns data frame of drug
#'  transporters
#' actions elements.
#'
#' This functions extracts the transporters actions element of drug node in
#' drugbank
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
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in
#'  the new parse operation
#' @return drug transporters actions node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_actions()
#'
#' # save in database and return parsed dataframe
#' transporters_actions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_actions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_actions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_actions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' #  and return parsed dataframe.
#' # If the csv exist override it and return it.
#' transporters_actions(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
transporters_actions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_trans_actions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_trans_actions <- readr::read_csv(path)
    } else {
      drug_trans_actions <-
        map_df(pkg_env$children, ~ get_trans_actions_df(.x)) %>%
        unique()

      write_csv(drug_trans_actions, save_csv, csv_path)
    }

    if (nrow(drug_trans_actions) > 0) {
      colnames(drug_trans_actions) <- c("action", "transporter_id")
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_trans_actions,
        table_name = "drug_trans_actions",
        save_table_only = TRUE
      )
    }
    return(drug_trans_actions)
  }


#' Extracts the drug transporters articles element and return data as
#' data frame.
#'
#' \code{transporters articles} returns data frame of drug
#' transporters articles elements.
#'
#' This functions extracts the transporters articles element of drug node in
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
#' @return drug transporters articles node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_articles()
#'
#' # save in database and return parsed dataframe
#' transporters_articles(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_articles(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_articles(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_articles(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' transporters_articles(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
transporters_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_trans_articles", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_trans_articles <- readr::read_csv(path)
    } else {
      drug_trans_articles <-
        map_df(pkg_env$children, ~ get_trans_articles_df(.x)) %>%
        unique()

      write_csv(drug_trans_articles, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_trans_articles,
        table_name = "drug_trans_articles",
        save_table_only = TRUE
      )
    }
    return(drug_trans_articles)
  }


#' Extracts the drug transporters textbooks element and return data as
#'  data frame.
#'
#' \code{transporters_textbooks} returns data frame of drug
#' transporters
#'  textbooks elements.
#'
#' This functions extracts the transporters textbooks element of drug node in
#' drugbank
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
#' @return drug transporters textbooks node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_textbooks()
#'
#' # save in database and return parsed dataframe
#' transporters_textbooks(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_textbooks(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' #  and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
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
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = drug_trans_textbooks,
        table_name = "drug_trans_textbooks",
        save_table_only = TRUE
      )
    }
    return(drug_trans_textbooks)
  }


#' Extracts the drug transporters links element and return data as data frame.
#'
#' \code{transporters_links} returns data frame of drug
#' transporters_links elements.
#'
#' This functions extracts the transporters links element of drug node in
#' drugbank
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
#' @return drug transporters links node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_links()
#'
#' # save in database and return parsed dataframe
#' transporters_links(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_links(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_links(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' transporters_links(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
transporters_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_trans_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_trans_links <- readr::read_csv(path)
    } else {
      drug_trans_links <-
        map_df(pkg_env$children, ~ get_trans_links_df(.x)) %>%
        unique()

      write_csv(drug_trans_links, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_trans_links,
        table_name = "drug_trans_links",
        save_table_only = TRUE
      )
    }
    return(drug_trans_links)
  }


#' Extracts the drug transporters polypeptides element and return data
#'  as data frame.
#'
#' \code{transporters_polypeptide} returns data frame of
#' transporters polypeptides groups elements.
#'
#' This functions extracts the transporters polypeptides element of drug
#' node in \strong{DrugBank}
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
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @return drug transporters polypeptides node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_polypeptide()
#'
#' # save in database and return parsed dataframe
#' transporters_polypeptide(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' transporters_polypeptide(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
transporters_polypeptide <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_trans_polys", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_trans_polys <- readr::read_csv(path)
    } else {
      drug_trans_polys <-
        map_df(pkg_env$children, ~ get_trans_poly_df(.x)) %>%
        unique()

      write_csv(drug_trans_polys, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_trans_polys,
        table_name = "drug_trans_polys",
        save_table_only = TRUE,
        field_types = list(
          general_function =
            paste("varchar(",
              max(
                nchar(drug_trans_polys$general_function),
                na.rm = TRUE
              ), ")",
              sep = ""
            ),
          specific_function =
            paste("varchar(",
              max(
                nchar(drug_trans_polys$specific_function),
                na.rm = TRUE
              ), ")",
              sep = ""
            ),
          amino_acid_sequence =
            paste("varchar(",
              max(
                nchar(drug_trans_polys$amino_acid_sequence),
                na.rm = TRUE
              ), ")",
              sep = ""
            ),
          gene_sequence = paste("varchar(max)", sep = "")
        )
      )
    }
    return(drug_trans_polys)
  }


#' Extracts the drug transporters polypeptides external identifiers
#'  element and return data as data frame.
#'
#' \code{transporters_polypep_ex_ident}
#' returns data frame of drug transporters polypeptides external identifiers
#' elements.
#'
#' This functions extracts the transporters polypeptides external
#'  identifiers element of drug node in \strong{DrugBank}
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
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @return drug transporters polypeptides external identifiers
#'  node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_polypep_ex_ident()
#'
#' # save in database and return parsed dataframe
#' transporters_polypep_ex_ident(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypep_ex_ident(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypep_ex_ident(
#'   save_table = TRUE, save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypep_ex_ident(
#'   save_csv = TRUE, csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
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
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = drug_trans_poly_ex_identity,
        table_name = "drug_trans_polys_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(drug_trans_poly_ex_identity)
  }


#' Extracts the drug transporters polypeptides syn
#' element and return data as data frame.
#'
#' \code{transporters_polypeptide_syn} returns data
#' frame of drug transporters polypeptides syn elements.
#'
#' This functions extracts the transporters polypeptides syn
#'  element of drug node in \strong{DrugBank}
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
#' @return drug transporters polypeptides syn node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_polypeptide_syn()
#'
#' # save in database and return parsed dataframe
#' transporters_polypeptide_syn(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_syn(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_syn(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_syn(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
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
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = drug_trans_poly_syn,
        table_name = "drug_trans_polys_syn",
        save_table_only = TRUE
      )
    }
    return(drug_trans_poly_syn)
  }


#' Extracts the drug transporters polypeptides pfams element
#' and return data as data frame.
#'
#' \code{transporters_polypeptide_pfams} returns data frame
#'  of drug transporters polypeptides pfams elements.
#'
#' This functions extracts the transporters polypeptides pfams
#'  element of drug node in \strong{DrugBank}
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
#' @return drug transporters polypeptides pfams node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_polypeptide_pfams()
#'
#' # save in database and return parsed dataframe
#' transporters_polypeptide_pfams(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_pfams(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
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
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = drug_trans_polys_pfams,
        table_name = "drug_trans_polys_pfams",
        save_table_only = TRUE
      )
    }
    return(drug_trans_polys_pfams)
  }


#' Extracts the drug transporters polypeptides go
#' classifiers element and return data as data frame.
#'
#' \code{transporters_polypeptide_go}
#' returns data frame of drug transporters polypeptides
#' go classifiers elements.
#'
#' This functions extracts the transporters polypeptides go
#'  classifiers element of drug node in \strong{DrugBank}
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
#' new parse operation
#' @return drug transporters polypeptides go classifiers node attributes
#' date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters_polypeptide_go()
#'
#' # save in database and return parsed dataframe
#' transporters_polypeptide_go(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_go(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_go(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters_polypeptide_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
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
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = drug_trans_polys_go,
        table_name = "drug_trans_polys_go",
        save_table_only = TRUE
      )
    }
    return(drug_trans_polys_go)
  }




#' Extracts the drug transporters element and return data as data frame.
#'
#' \code{transporters} returns data frame of drug transporters
#' elements.
#'
#' This functions extracts the transporters element of drug node in
#'  \strong{DrugBank}
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
#' @return drug transporters node attributes date frame
#' @family transporters
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' transporters()
#'
#' # save in database and return parsed dataframe
#' transporters(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' transporters(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' transporters(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
transporters <-
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
        map_df(pkg_env$children, ~ get_trans_df(.x)) %>%
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
