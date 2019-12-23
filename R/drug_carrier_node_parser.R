get_carriers_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ get_organizm_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
  ))
}

get_carriers_actions_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "actions", id = "id")
  ))
}

get_carriers_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

get_carriers_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

get_carriers_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
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

#' Extracts the drug carriers actions element and return data as data frame.
#'
#' \code{carriers_actions} returns data frame of drug
#'  carriers actions elements.
#'
#' This functions extracts the carriers actions element of drug
#'  node in \strong{DrugBank}
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
#' @param override_csv override existing csv, if any, in case it is true in
#' the new parse operation
#' @return drug carriers actions node attributes date frame
#'
#' @family carriers
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_actions()
#'
#' # save in database and return parsed dataframe
#' carriers_actions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_actions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_actions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_actions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location and
#' # return parsed dataframe.
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
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = drug_carriers_actions,
        table_name = "drug_carriers_actions",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_actions)
  }

#' Extracts the drug carriers articles element and return
#'  data as data frame.
#'
#' \code{carriers_articles} returns data frame of
#' drug carriers articles elements.
#'
#' This functions extracts the carriers articles element of drug node in
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
#' @return drug carriers_articles node attributes date frame
#'
#' @family carriers
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_articles()
#'
#' # save in database and return parsed dataframe
#' carriers_articles(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_articles(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_articles(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_articles(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' carriers_articles(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
carriers_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_carriers_articles", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_carriers_articles <- readr::read_csv(path)
    } else {
      drug_carriers_articles <-
        map_df(pkg_env$children, ~ get_carriers_articles_df(.x)) %>% unique()

      write_csv(drug_carriers_articles, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_carriers_articles,
        table_name = "drug_carriers_articles",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_articles)
  }


#' Extracts the drug carriers textbooks element and return data as data frame.
#'
#' \code{carriers_textbooks} returns data frame of drug carriers
#'  textbooks elements.
#'
#' This functions extracts the carriers textbooks element of drug node in
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
#' @return drug carriers textbooks node attributes date frame
#'
#' @family carriers
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_textbooks()
#'
#' # save in database and return parsed dataframe
#' carriers_textbooks(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_textbooks(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location and
#' # return parsed dataframe.
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
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = drug_carriers_textbooks,
        table_name = "drug_carriers_textbooks",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_textbooks)
  }

#' Extracts the drug carriers links element and return data as data frame.
#'
#' \code{parse_drug_groups} returns data frame of drug carriers links elements.
#'
#' This functions extracts the carriers links element of drug node in \strong{DrugBank}
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
#' @param override_csv override existing csv, if any, in case it is true
#' in the new parse operation
#' @return drug carriers_links node attributes date frame
#'
#' @family carriers
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_links()
#'
#' # save in database and return parsed dataframe
#' carriers_links(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_links(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_links(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location and
#' # return parsed dataframe.
#' # If the csv exist override it and return it.
#' carriers_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
carriers_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_carriers_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_carriers_links <- readr::read_csv(path)
    } else {
      drug_carriers_links <-
        map_df(pkg_env$children, ~ get_carriers_links_df(.x)) %>% unique()

      write_csv(drug_carriers_links, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_carriers_links,
        table_name = "drug_carriers_links",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_links)
  }

#' Extracts the drug carriers polypeptides element and return data as
#' data frame.
#'
#' \code{carriers_polypeptides} returns data frame of drug carriers
#' polypeptides elements.
#'
#' This functions extracts the carriers polypeptides element of drug
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
#' @param override_csv override existing csv, if any, in case it is true
#' in the new parse operation
#' @return drug carriers polypeptides node attributes date frame
#'
#' @family carriers
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_polypeptide()
#'
#' # save in database and return parsed dataframe
#' carriers_polypeptide(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
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
                            override_csv = FALSE) {
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
      con = pkg_env$con,
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
  return(drug_carriers_polypeptides)
}

#' Extracts the drug carriers polypeptides external identifiers
#'  element and return data as data frame.
#'
#' \code{carriers_polypeptide_ext_identity } returns
#'  data frame of drug carriers polypeptides external identifiers elements.
#'
#' This functions extracts the carriers polypeptides external identifiers
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
#' @param override_csv override existing csv, if any, in case it is true in
#' the new parse operation
#' @return drug carriers polypeptides external identifiers
#'  node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_polypeptide_ext_identity()
#'
#' # save in database and return parsed dataframe
#' carriers_polypeptide_ext_identity(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide_ext_identity(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide_ext_identity(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptide_ext_identity(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' carriers_polypeptide_ext_identity(
#'   save_csv = TRUE, csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
carriers_polypeptide_ext_identity <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = carr_poly_ex_identitys,
        table_name = "drug_carriers_polypeptides_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(carr_poly_ex_identitys)
  }

#' Extracts the drug carriers polypeptides syn element and return data as
#' data frame.
#'
#' \code{carriers_polypeptidepeptides_syn} returns
#'  data frame of drug carriers polypeptides syn elements.
#'
#' This functions extracts the carriers polypeptides syn
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
#' @param override_csv override existing csv, if any, in case it is true
#' in the new parse operation
#' @return drug carriers polypeptides syn node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_polypeptidepeptides_syn()
#'
#' # save in database and return parsed dataframe
#' carriers_polypeptidepeptides_syn(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_syn(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_syn(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_syn(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' carriers_polypeptidepeptides_syn(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
carriers_polypeptidepeptides_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = carr_poly_syn,
        table_name = "drug_carriers_polypeptides_syn",
        save_table_only = TRUE
      )
    }
    return(carr_poly_syn)
  }

#' Extracts the drug carriers polypeptides pfams element and return data as
#' data frame.
#'
#' \code{carriers_polypeptidepeptides_pfams} returns data frame
#'  of drug carriers polypeptides pfams elements.
#'
#' This functions extracts the carriers polypeptides pfams element of
#'  drug node in \strong{DrugBank}
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
#' @param override_csv override existing csv, if any, in case it is true in
#' the new parse operation
#' @return drug carriers polypeptides pfams node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_polypeptidepeptides_pfams()
#'
#' # save in database and return parsed dataframe
#' carriers_polypeptidepeptides_pfams(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_pfams(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' carriers_polypeptidepeptides_pfams(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
carriers_polypeptidepeptides_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = carr_poly_pfams,
        table_name = "carr_poly_pfams",
        save_table_only = TRUE
      )
    }
    return(carr_poly_pfams)
  }

#' Extracts the drug carriers polypeptides go classifiers
#'  element and return data as data frame.
#'
#' \code{carriers_polypeptidepeptides_go} returns
#' data frame of drug carriers polypeptides go classifiers elements.
#'
#' This functions extracts the carriers polypeptides go
#' classifiers element of drug node in \strong{DrugBank}
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
#' @param override_csv override existing csv, if any, in case it is true in
#' the new parse operation
#' @return drug carriers polypeptides go classifiers node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers_polypeptidepeptides_go()
#'
#' # save in database and return parsed dataframe
#' carriers_polypeptidepeptides_go(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_go(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_go(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers_polypeptidepeptides_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' carriers_polypeptidepeptides_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
carriers_polypeptidepeptides_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
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
        con = pkg_env$con,
        df = carr_poly_go,
        table_name = "carr_poly_go",
        save_table_only = TRUE
      )
    }
    return(carr_poly_go)
  }

#' Extracts the drug carriers element and return data as data frame.
#'
#' \code{carriers} returns data frame of drug carriers elements.
#'
#' This functions extracts the carriers element of drug node in \strong{DrugBank}
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
#' @param override_csv override existing csv, if any, in case it is true
#' in the new parse operation
#' @return drug carriers node attributes date frame
#' @family carriers
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' carriers()
#'
#' # save in database and return parsed dataframe
#' carriers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' carriers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' carriers(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
carriers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_carriers", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_carriers <- readr::read_csv(path)
    } else {
      drug_carriers <-
        map_df(pkg_env$children, ~ get_carriers_df(.x)) %>%
        unique()
      write_csv(drug_carriers, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_carriers,
        table_name = "drug_carriers",
        foreign_key = "parent_key"
      )
    }
    return(drug_carriers)
  }
