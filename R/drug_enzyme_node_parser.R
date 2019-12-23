get_enzyme_rec <- function(r, drug_key) {
  tibble(
    id = xmlValue(r[["id"]]),
    name = xmlValue(r[["name"]]),
    organism = xmlValue(r[["organism"]]),
    known_action = xmlValue(r[["known-action"]]),
    inhibition_strength = xmlValue(r[["inhibition-strength"]]),
    induction_strength = xmlValue(r[["induction-strength"]]),
    position = ifelse(is.null(xmlGetAttr(r, name = "position")),
      NA, xmlGetAttr(r, name = "position")
    ),
    parent_key = drug_key
  )
}

get_enzymes_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ get_enzyme_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
  ))
}

# Extract drug enzymes actions df
get_enzymes_actions_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ drug_sub_df(.x, "actions", id = "id")
  ))
}

# Extract drug articles df
get_enzymes_articles_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "articles", id = "id")
  ))
}

# Extract drug textbooks df
get_enzymes_textbooks_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "textbooks", id = "id")
  ))
}

# Extract drug links df
get_enzymes_links_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ drug_sub_df(.x, "references", seconadary_node = "links", id = "id")
  ))
}

get_enzymes_polypeptide_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ get_polypeptide_rec(.x)
  ))
}

get_enzy_poly_ex_identity_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ get_poly_ex_identity(.x)
  ))
}

get_enzy_poly_syn_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ get_polypeptide_syn(.x)
  ))
}

get_enzy_poly_pfams_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ get_polypeptide_pfams(.x)
  ))
}

get_enzy_poly_go_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ get_polypeptide_go(.x)
  ))
}

#' Extracts the drug enzymes actions element and return data as data frame.
#'
#' \code{parse_drug_enzymes_actions} returns data frame of drug enzymes
#' actions elements.
#'
#' This functions extracts the enzymes actions element of drug node in
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
#' @return drug enzymes actions node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_enzymes_actions()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_actions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_actions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_actions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_actions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_actions(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_actions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_enzymes_actions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_enzymes_actions <- readr::read_csv(path)
    } else {
      drug_enzymes_actions <-
        map_df(pkg_env$children, ~ get_enzymes_actions_df(.x)) %>%
        unique()
      write_csv(drug_enzymes_actions, save_csv, csv_path)
    }

    if (nrow(drug_enzymes_actions) > 0) {
      colnames(drug_enzymes_actions) <- c("action", "enzyme_id")
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_enzymes_actions,
        table_name = "drug_enzymes_actions",
        save_table_only = TRUE
      )
    }
    return(drug_enzymes_actions)
  }

#' Extracts the drug enzymes articles element and return data as data frame.
#'
#' \code{parse_drug_enzymes_articles} returns data frame of drug enzymes
#' articles elements.
#'
#' This functions extracts the enzymes articles element of drug node in drugbank
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
#' @return drug enzymes articles node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_enzymes_articles()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_articles(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_articles(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_articles(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_articles(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_articles(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_drug_enzymes_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_enzymes_articles", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_enzymes_articles <- readr::read_csv(path)
    } else {
      drug_enzymes_articles <-
        map_df(pkg_env$children, ~ get_enzymes_articles_df(.x)) %>%
        unique()

      write_csv(drug_enzymes_articles, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_enzymes_articles,
        table_name = "drug_enzymes_articles",
        save_table_only = TRUE
      )
    }
    return(drug_enzymes_articles)
  }

#' Extracts the drug enzymes textbooks element and return data as data frame.
#'
#' \code{parse_drug_enzymes_textbooks} returns data frame of drug enzymes
#' textbooks elements.
#'
#' This functions extracts the enzymes textbooks element of drug node in
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
#' @param override_csv override existing csv, if any, in case it is true in
#' the new parse operation
#' @return drug enzymes textbooks node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_enzymes_textbooks()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_textbooks(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_textbooks(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_textbooks(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_textbooks(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_textbooks(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_drug_enzymes_textbooks <- function(save_table = FALSE,
                                         save_csv = FALSE,
                                         csv_path = ".",
                                         override_csv = FALSE) {
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
      con = pkg_env$con,
      df = drug_enzymes_textbooks,
      table_name = "drug_enzymes_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_enzymes_textbooks)
}

#' Extracts the drug enzymes links element and return data as data frame.
#'
#' \code{parse_drug_enzymes_links} returns data frame of drug enzymes links
#' elements.
#'
#' This functions extracts the enzymes links element of drug node in
#' \strong{DrugBank}
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
#' @return drug enzymes links node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_enzymes_links()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_links(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_links(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_links(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_enzymes_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_enzymes_links <- readr::read_csv(path)
    } else {
      drug_enzymes_links <-
        map_df(pkg_env$children, ~ get_enzymes_links_df(.x)) %>%
        unique()

      write_csv(drug_enzymes_links, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_enzymes_links,
        table_name = "drug_enzymes_links",
        save_table_only = TRUE
      )
    }
    return(drug_enzymes_links)
  }

#' Extracts the drug enzymes polypeptides element and return data as data frame.
#'
#' \code{parse_enzy_poly} returns data frame of drug enzymes
#' polypeptides elements.
#'
#' This functions extracts the enzymes polypeptides element of drug node in
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
#' @param override_csv override existing csv, if any, in case it is true
#' in the new parse operation
#' @return drug enzymes polypeptides node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_enzy_poly()
#'
#' # save in database and return parsed dataframe
#' parse_enzy_poly(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_enzy_poly(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_enzy_poly <- function(save_table = FALSE,
                            save_csv = FALSE,
                            csv_path = ".",
                            override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_enzymes_polypeptides", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_enzymes_polypeptides <- readr::read_csv(path)
  } else {
    drug_enzymes_polypeptides <-
      map_df(pkg_env$children, ~ get_enzymes_polypeptide_df(.x)) %>%
      unique()

    write_csv(drug_enzymes_polypeptides, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_enzymes_polypeptides,
      table_name = "drug_enzymes_polypeptides",
      save_table_only = TRUE,
      field_types = list(
        general_function =
          paste("varchar(",
            max(
              nchar(drug_enzymes_polypeptides$general_function),
              na.rm = TRUE
            ), ")",
            sep = ""
          ),
        specific_function =
          paste("varchar(",
            max(
              nchar(drug_enzymes_polypeptides$specific_function),
              na.rm = TRUE
            ), ")",
            sep = ""
          ),
        amino_acid_sequence =
          paste("varchar(",
            max(
              nchar(drug_enzymes_polypeptides$amino_acid_sequence),
              na.rm = TRUE
            ), ")",
            sep = ""
          ),
        gene_sequence =
          paste("varchar(",
            max(
              nchar(drug_enzymes_polypeptides$gene_sequence),
              na.rm = TRUE
            ), ")",
            sep = ""
          )
      )
    )
  }
  return(drug_enzymes_polypeptides)
}

#' Extracts the drug enzymes polypeptides external identifiers
#'  element and return data as data frame.
#'
#' \code{parse_enzy_poly_ext_identitys} returns data
#'  frame of drug enzymes polypeptides external identifiers elements.
#'
#' This functions extracts the enzymes polypeptides external identifiers
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
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @return drug enzymes polypeptides external identifiers node
#'  attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_enzy_poly_ext_identitys()
#'
#' # save in database and return parsed dataframe
#' parse_enzy_poly_ext_identitys(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_ext_identitys(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_ext_identitys(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_ext_identitys(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_enzy_poly_ext_identitys(
#'   save_csv = TRUE, csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
parse_enzy_poly_ext_identitys <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path(
        "drug_enzy_poly_ex_identity",
        csv_path
      )
    if (!override_csv & file.exists(path)) {
      drug_enzy_poly_ex_identity <-
        readr::read_csv(path)
    } else {
      drug_enzy_poly_ex_identity <-
        map_df(
          pkg_env$children,
          ~ get_enzy_poly_ex_identity_df(.x)
        ) %>%
        unique()

      write_csv(
        drug_enzy_poly_ex_identity,
        save_csv,
        csv_path
      )
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_enzy_poly_ex_identity,
        table_name = "drug_enzymes_polypeptides_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(drug_enzy_poly_ex_identity)
  }


#' Extracts the drug enzymes polypeptides syn
#'  element and return data as data frame.
#'
#' \code{parse_enzy_poly_syn} returns data frame of drug
#' enzymes polypeptides syn elements.
#'
#' This functions extracts the enzymes polypeptides syn
#' element of drug node in \strong{DrugBank}
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
#' @return drug enzymes polypeptides syn node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_enzy_poly_syn()
#'
#' # save in database and return parsed dataframe
#' parse_enzy_poly_syn(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_syn(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does
#' # not exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_syn(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' # given location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_syn(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_enzy_poly_syn(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_enzy_poly_syn <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_enzy_poly_syn", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_enzy_poly_syn <- readr::read_csv(path)
    } else {
      drug_enzy_poly_syn <-
        map_df(
          pkg_env$children,
          ~ get_enzy_poly_syn_df(.x)
        ) %>%
        unique()

      write_csv(drug_enzy_poly_syn, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_enzy_poly_syn,
        table_name = "drug_enzymes_polypeptides_syn",
        save_table_only = TRUE
      )
    }
    return(drug_enzy_poly_syn)
  }

#' Extracts the drug enzymes polypeptides pfams element and return
#'  data as data frame.
#'
#' \code{parse_enzy_poly_pfams} returns data frame of drug
#' enzymes polypeptides pfams elements.
#'
#' This functions extracts the enzymes polypeptides pfams element of drug
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
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @return drug groups node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_enzy_poly_pfams()
#'
#' # save in database and return parsed dataframe
#' parse_enzy_poly_pfams(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_pfams(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_enzy_poly_pfams(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
parse_enzy_poly_pfams <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_enzymes_polypeptide_pfams", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_enzymes_polypeptide_pfams <- readr::read_csv(path)
    } else {
      drug_enzymes_polypeptide_pfams <-
        map_df(
          pkg_env$children,
          ~ get_enzy_poly_pfams_df(.x)
        ) %>%
        unique()

      write_csv(drug_enzymes_polypeptide_pfams, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_enzymes_polypeptide_pfams,
        table_name = "drug_enzymes_polypeptides_pfams",
        save_table_only = TRUE
      )
    }
    return(drug_enzymes_polypeptide_pfams)
  }

#' Extracts the drug groups element and return data as data frame.
#'
#' \code{parse_enzy_poly_go} returns data
#' frame of drug enzymes polypeptides go classifiers elements.
#'
#' This functions extracts the enzymes polypeptides go classifiers
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
#' @return drug enzymes polypeptides go classifiers node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_enzy_poly_go()
#'
#' # save in database and return parsed dataframe
#' parse_enzy_poly_go(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_go(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_go(
#'   ssave_table = TRUE,
#'   save_csv = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_enzy_poly_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE
#' )
#'
#' # save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_enzy_poly_go(
#'   save_csv = TRUE,
#'   csv_path = TRUE, override = TRUE
#' )
#' }
#' @export
parse_enzy_poly_go <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path(
        "drug_enzy_poly_go",
        csv_path
      )
    if (!override_csv & file.exists(path)) {
      drug_enzy_poly_go <- readr::read_csv(path)
    } else {
      drug_enzy_poly_go <-
        map_df(
          pkg_env$children,
          ~ get_enzy_poly_go_df(.x)
        ) %>%
        unique()

      write_csv(
        drug_enzy_poly_go,
        save_csv,
        csv_path
      )
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_enzy_poly_go,
        table_name = "drug_enzy_poly_go",
        save_table_only = TRUE
      )
    }
    return(drug_enzy_poly_go)
  }

#' Extracts the drug enzymes element and return data as data frame.
#'
#' \code{parse_drug_enzymes} returns data frame of drug enzymes elements.
#'
#' This functions extracts the enzymes element of drug node in \strong{DrugBank}
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
#' @return drug enzymes node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed dataframe
#' parse_drug_enzymes()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes <- function(save_table = FALSE,
                               save_csv = FALSE,
                               csv_path = ".",
                               override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_enzymes", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_enzymes <- readr::read_csv(path)
  } else {
    drug_enzymes <-
      map_df(pkg_env$children, ~ get_enzymes_df(.x)) %>%
      unique()

    write_csv(drug_enzymes, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(
      con = pkg_env$con,
      df = drug_enzymes,
      table_name = "drug_enzymes"
    )
  }
  return(drug_enzymes)
}
