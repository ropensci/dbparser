get_enzyme_rec <- function(r, drug_key) {
  tibble(
    id = xmlValue(r[["id"]]),
    name = xmlValue(r[["name"]]),
    organism = xmlValue(r[["organism"]]),
    known_action = xmlValue(r[["known-action"]]),
    inhibition_strength = xmlValue(r[["inhibition-strength"]]),
    induction_strength = xmlValue(r[["induction-strength"]]),
    position = ifelse(is.null(xmlGetAttr(r, name = "position")),
                      NA, xmlGetAttr(r, name = "position")),
    parent_key = drug_key
  )
}

get_enzymes_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~ get_enzyme_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

# Extract drug enzymes actions df
get_enzymes_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~ drug_sub_df(.x, "actions", id = "id")))
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
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~ get_polypeptide_rec(.x)))
}

get_enzymes_polypeptide_external_identifiers_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["enzymes"]]),
    ~ get_polypeptide_external_identifiers(.x)
  ))
}

get_enzymes_polypeptide_synonyms_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~ get_polypeptide_synonyms(.x)))
}

get_enzymes_polypeptide_pfams_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~ get_polypeptide_pfams(.x)))
}

get_enzymes_polypeptide_go_classifiers_df <- function(rec) {
  return(map_df(xmlChildren(rec[["enzymes"]]),
                ~ get_polypeptide_go_classifiers(.x)))
}

#' Extracts the drug enzymes actions element and return data as data frame.
#'
#' \code{parse_drug_enzymes_actions} returns data frame of drug enzymes
#' actions elements.
#'
#' This functions extracts the enzymes actions element of drug node in drug bank
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes actions node attributes date frame
#'
#' @examples
#' \donttest{
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
parse_drug_enzymes_actions <- function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
  drug_enzymes_actions <-
    map_df(pkg.env$children, ~ get_enzymes_actions_df(.x)) %>%
    unique()

  if (nrow(drug_enzymes_actions) > 0) {
    colnames(drug_enzymes_actions) <- c("action", "enzyme_id")
  }

  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_enzymes_actions,
      table_name = "drug_enzymes_actions",
      save_table_only = TRUE
    )
  }
  return(drug_enzymes_actions)
}

#' Extracts the drug enzymes articles element and return data as data frame.
#'
#' \code{parse_drug_enzymes_articles} returns data frame of drug enzymes articles elements.
#'
#' This functions extracts the enzymes articles element of drug node in drug bank
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes articles node attributes date frame
#'
#' @examples
#' \donttest{
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
#' parse_drug_enzymes_articles(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_articles <- function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
  drug_enzymes_articles <-
    map_df(pkg.env$children, ~ get_enzymes_articles_df(.x)) %>%
    unique()
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_enzymes_articles,
      table_name = "drug_enzymes_articles",
      save_table_only = TRUE
    )
  }
  return(drug_enzymes_articles)
}

#' Extracts the drug enzymes textbooks element and return data as data frame.
#'
#' \code{parse_drug_enzymes_textbooks} returns data frame of drug enzymes textbooks elements.
#'
#' This functions extracts the enzymes textbooks element of drug node in drug bank
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes textbooks node attributes date frame
#'
#' @examples
#' \donttest{
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
#' parse_drug_enzymes_textbooks(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_textbooks <- function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
  drug_enzymes_textbooks <-
    map_df(pkg.env$children, ~ get_enzymes_textbooks_df(.x)) %>%
    unique()
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_enzymes_textbooks,
      table_name = "drug_enzymes_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_enzymes_textbooks)
}

#' Extracts the drug enzymes links element and return data as data frame.
#'
#' \code{parse_drug_enzymes_links} returns data frame of drug enzymes links elements.
#'
#' This functions extracts the enzymes links element of drug node in drug bank
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes links node attributes date frame
#'
#' @examples
#' \donttest{
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
#' #location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_links <- function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
  drug_enzymes_links <-
    map_df(pkg.env$children, ~ get_enzymes_links_df(.x)) %>%
    unique()
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_enzymes_links,
      table_name = "drug_enzymes_links",
      save_table_only = TRUE
    )
  }
  return(drug_enzymes_links)
}

#' Extracts the drug enzymes polypeptides element and return data as data frame.
#'
#' \code{parse_drug_enzymes_polypeptides} returns data frame of drug enzymes
#' polypeptides elements.
#'
#' This functions extracts the enzymes polypeptides element of drug node in drug bank
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes polypeptides node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_enzymes_polypeptides()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_polypeptides(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_polypeptides(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_polypeptides <- function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
  drug_enzymes_polypeptides <-
    map_df(pkg.env$children, ~ get_enzymes_polypeptide_df(.x)) %>%
    unique()
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_enzymes_polypeptides,
      table_name = "drug_enzymes_polypeptides",
      save_table_only = TRUE,
      field.types = list(
        general_function = paste("varchar(",
                                 max(
                                   nchar(drug_enzymes_polypeptides$general_function)
                                 ),
                                 ")", sep = ""),
        specific_function = paste("varchar(",
                                  max(
                                    nchar(drug_enzymes_polypeptides$specific_function)
                                  ),
                                  ")", sep = ""),
        amino_acid_sequence = paste("varchar(",
                                    max(
                                      nchar(drug_enzymes_polypeptides$amino_acid_sequence)
                                    ),
                                    ")", sep = ""),
        gene_sequence = paste("varchar(",
                              max(
                                nchar(drug_enzymes_polypeptides$gene_sequence)
                              ),
                              ")", sep = "")
      )
    )
  }
  return(drug_enzymes_polypeptides)
}

#' Extracts the drug enzymes polypeptides external identifiers
#'  element and return data as data frame.
#'
#' \code{parse_drug_enzymes_polypeptides_external_identifiers} returns data
#'  frame of drug enzymes polypeptides external identifiers elements.
#'
#' This functions extracts the enzymes polypeptides external identifiers
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes polypeptides external identifiers node
#'  attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_enzymes_polypeptides_external_identifiers()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_polypeptides_external_identifiers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_external_identifiers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_external_identifiers(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_external_identifiers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_polypeptides_external_identifiers(
#' save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_polypeptides_external_identifiers <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
    drug_enzymes_polypeptide_external_identifiers <-
      map_df(pkg.env$children,
             ~ get_enzymes_polypeptide_external_identifiers_df(.x)) %>%
      unique()
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_enzymes_polypeptide_external_identifiers,
        table_name = "drug_enzymes_polypeptides_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(drug_enzymes_polypeptide_external_identifiers)
  }


#' Extracts the drug enzymes polypeptides synonyms
#'  element and return data as data frame.
#'
#' \code{parse_drug_enzymes_polypeptides_synonyms} returns data frame of drug
#' enzymes polypeptides synonyms elements.
#'
#' This functions extracts the enzymes polypeptides synonyms
#' element of drug node in drug bank
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes polypeptides synonyms node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_enzymes_polypeptides_synonyms()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_polypeptides_synonyms(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_synonyms(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does
#' # not exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_synonyms(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' # given location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_synonyms(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_polypeptides_synonyms(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_polypeptides_synonyms <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
    drug_enzymes_polypeptide_synonyms <-
      map_df(pkg.env$children,
             ~ get_enzymes_polypeptide_synonyms_df(.x)) %>%
      unique()
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_enzymes_polypeptide_synonyms,
        table_name = "drug_enzymes_polypeptides_synonyms",
        save_table_only = TRUE
      )
    }
    return(drug_enzymes_polypeptide_synonyms)
  }

#' Extracts the drug enzymes polypeptides pfams element and return
#'  data as data frame.
#'
#' \code{parse_drug_enzymes_polypeptides_pfams} returns data frame of drug
#' enzymes polypeptides pfams elements.
#'
#' This functions extracts the enzymes polypeptides pfams element of drug
#'  node in drug bank
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug groups node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_enzymes_polypeptides_pfams()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_polypeptides_pfams(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_pfams(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_pfams(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_pfams(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_polypeptides_pfams(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_polypeptides_pfams <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
    drug_enzymes_polypeptide_pfams <-
      map_df(pkg.env$children,
             ~ get_enzymes_polypeptide_pfams_df(.x)) %>%
      unique()

    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_enzymes_polypeptide_pfams,
        table_name = "drug_enzymes_polypeptides_pfams",
        save_table_only = TRUE
      )
    }
    return(drug_enzymes_polypeptide_pfams)
  }

#' Extracts the drug groups element and return data as data frame.
#'
#' \code{parse_drug_enzymes_polypeptides_go_classifiers} returns data
#' frame of drug enzymes polypeptides go classifiers elements.
#'
#' This functions extracts the enzymes polypeptides go classifiers
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes polypeptides go classifiers node attributes date frame
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_enzymes_polypeptides_go_classifiers()
#'
#' # save in database and return parsed dataframe
#' parse_drug_enzymes_polypeptides_go_classifiers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_go_classifiers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_go_classifiers(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_enzymes_polypeptides_go_classifiers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_enzymes_polypeptides_go_classifiers(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_enzymes_polypeptides_go_classifiers <-
  function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
    drug_enzymes_polypeptides_go_classifiers <-
      map_df(pkg.env$children,
             ~ get_enzymes_polypeptide_go_classifiers_df(.x)) %>%
      unique()

    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_enzymes_polypeptides_go_classifiers,
        table_name = "drug_enzymes_polypeptides_go_classifiers",
        save_table_only = TRUE
      )
    }
    return(drug_enzymes_polypeptides_go_classifiers)
  }

#' Extracts the drug enzymes element and return data as data frame.
#'
#' \code{parse_drug_enzymes} returns data frame of drug enzymes elements.
#'
#' This functions extracts the enzymes element of drug node in drug bank
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
#' @param csv_path location to save csv files into it, default is current location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the new parse operation
#' @return drug enzymes node attributes date frame
#'
#' @examples
#' \donttest{
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
parse_drug_enzymes <- function(save_table = FALSE, save_csv = FALSE, csv_path = ".", override_csv = FALSE) {
  drug_enzymes <-
    map_df(pkg.env$children, ~ get_enzymes_df(.x)) %>%
    unique()

  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_enzymes,
                  table_name = "drug_enzymes")
  }
  return(drug_enzymes)
}
