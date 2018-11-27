get_carriers_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]),
                ~ get_organizm_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

get_carriers_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]),
                ~ drug_sub_df(.x, "actions", id = "id")))
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

get_carriers_polypeptide_external_identifiers_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["carriers"]]),
    ~ get_polypeptide_external_identifiers(.x)
  ))
}

get_carriers_polypeptide_synonyms_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]), ~ get_polypeptide_synonyms(.x)))
}

get_carriers_polypeptide_pfams_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]), ~ get_polypeptide_pfams(.x)))
}

get_carriers_polypeptide_go_classifiers_df <- function(rec) {
  return(map_df(xmlChildren(rec[["carriers"]]),
                ~ get_polypeptide_go_classifiers(.x)))
}

#' Extracts the drug carriers actions element and return data as data frame.
#'
#' \code{parse_drug_carriers_actions} returns data frame of drug
#'  carriers actions elements.
#'
#' This functions extracts the carriers actions element of drug
#'  node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers actions node attributs date frame
#'
#' @examples
#' parse_drug_carriers_actions()
#' parse_drug_carriers_actions(TRUE)
#' parse_drug_carriers_actions(save_table = FALSE)
#' @export
parse_drug_carriers_actions <- function(save_table = FALSE) {
  drug_carriers_actions <-
    map_df(pkg.env$children, ~ get_carriers_actions_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
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
#' \code{parse_drug_carriers_articles} returns data frame of
#' drug carriers articles elements.
#'
#' This functions extracts the carriers articles element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers_articles node attributs date frame
#'
#' @examples
#' parse_drug_carriers_articles()
#' parse_drug_carriers_articles(TRUE)
#' parse_drug_carriers_articles(save_table = FALSE)
#' @export
parse_drug_carriers_articles <- function(save_table = FALSE) {
  drug_carriers_articles <-
    map_df(pkg.env$children, ~ get_carriers_articles_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_carriers_articles,
      table_name = "drug_carriers_articles",
      save_table_only = TRUE
    )
  }
  return(drug_carriers_articles)
}


#' Extracts the drug carriers textbooks element and return data as data frame.
#'
#' \code{parse_drug_carriers_textbooks} returns data frame of drug carriers
#'  textbooks elements.
#'
#' This functions extracts the carriers textbooks element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers textbooks node attributs date frame
#'
#' @examples
#' parse_drug_carriers_textbooks()
#' parse_drug_carriers_textbooks(TRUE)
#' parse_drug_carriers_textbooks(save_table = FALSE)
#' @export
parse_drug_carriers_textbooks <- function(save_table = FALSE) {
  drug_carriers_textbooks <-
    map_df(pkg.env$children, ~ get_carriers_textbooks_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
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
#' This functions extracts the carriers links element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers_links node attributs date frame
#'
#' @examples
#' parse_drug_carriers_links()
#' parse_drug_carriers_links(TRUE)
#' parse_drug_carriers_links(save_table = FALSE)
#' @export
parse_drug_carriers_links <- function(save_table = FALSE) {
  drug_carriers_links <- map_df(pkg.env$children, ~ get_carriers_links_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
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
#' node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers polypeptides node attributs date frame
#'
#' @examples
#' parse_drug_carriers_polypeptides()
#' parse_drug_carriers_polypeptides(TRUE)
#' parse_drug_carriers_polypeptides(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides <- function(save_table = FALSE) {
  drug_carriers_polypeptides <-
    map_df(pkg.env$children, ~ get_carriers_polypeptide_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_carriers_polypeptides,
      table_name = "drug_carriers_polypeptides",
      save_table_only = TRUE,
      field.types = list(
        general_function = paste("varchar(",
                                 max(
                                   nchar(drug_carriers_polypeptides$general_function)
                                 ),
                                 ")", sep = ""),
        specific_function = paste("varchar(",
                                  max(
                                    nchar(drug_carriers_polypeptides$specific_function)
                                  ),
                                  ")", sep = ""),
        amino_acid_sequence = paste("varchar(",
                                    max(
                                      nchar(drug_carriers_polypeptides$amino_acid_sequence)
                                    ),
                                    ")", sep = ""),
        gene_sequence = paste("varchar(max)", sep = "")
      )
    )
  }
  return(drug_carriers_polypeptides)
}

#' Extracts the drug carriers polypeptides external identifiers
#'  element and return data as data frame.
#'
#' \code{parse_drug_carriers_polypeptides_external_identifiers} returns
#'  data frame of drug carriers polypeptides external identifiers elements.
#'
#' This functions extracts the carriers polypeptides external identifiers
#'  element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers polypeptides external identifiers
#'  node attributs date frame
#'
#' @examples
#' parse_drug_carriers_polypeptides_external_identifiers()
#' parse_drug_carriers_polypeptides_external_identifiers(TRUE)
#' parse_drug_carriers_polypeptides_external_identifiers(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides_external_identifiers <-
  function(save_table = FALSE) {
    drug_carriers_polypeptide_external_identifiers <-
      map_df(pkg.env$children,
             ~ get_carriers_polypeptide_external_identifiers_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_carriers_polypeptide_external_identifiers,
        table_name = "drug_carriers_polypeptides_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_polypeptide_external_identifiers)
  }

#' Extracts the drug carriers polypeptides synonyms element and return data as data frame.
#'
#' \code{parse_drug_carriers_polypeptides_synonyms} returns
#'  data frame of drug carriers polypeptides synonyms elements.
#'
#' This functions extracts the carriers polypeptides synonyms
#'  element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers polypeptides synonyms node attributs date frame
#'
#' @examples
#' parse_drug_carriers_polypeptides_synonyms()
#' parse_drug_carriers_polypeptides_synonyms(TRUE)
#' parse_drug_carriers_polypeptides_synonyms(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides_synonyms <-
  function(save_table = FALSE) {
    drug_carriers_polypeptide_synonyms <- map_df(pkg.env$children,
                                                 ~ get_carriers_polypeptide_synonyms_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_carriers_polypeptide_synonyms,
        table_name = "drug_carriers_polypeptides_synonyms",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_polypeptide_synonyms)
  }

#' Extracts the drug carriers polypeptides pfams element and return data as data frame.
#'
#' \code{parse_drug_carriers_polypeptides_pfams} returns data frame
#'  of drug carriers polypeptides pfams elements.
#'
#' This functions extracts the carriers polypeptides pfams element of
#'  drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers polypeptides pfams node attributs date frame
#'
#' @examples
#' parse_drug_carriers_polypeptides_pfams()
#' parse_drug_carriers_polypeptides_pfams(TRUE)
#' parse_drug_carriers_polypeptides_pfams(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides_pfams <-
  function(save_table = FALSE) {
    drug_carriers_polypeptide_pfams <- map_df(pkg.env$children,
                                              ~ get_carriers_polypeptide_pfams_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_carriers_polypeptide_pfams,
        table_name = "drug_carriers_polypeptide_pfams",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_polypeptide_pfams)
  }

#' Extracts the drug carriers polypeptides go classifiers
#'  element and return data as data frame.
#'
#' \code{parse_drug_carriers_polypeptides_go_classifiers} returns
#' data frame of drug carriers polypeptides go classifiers elements.
#'
#' This functions extracts the carriers polypeptides go
#' classifiers element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers polypeptides go classifiers node attributs date frame
#'
#' @examples
#' parse_drug_carriers_polypeptides_go_classifiers()
#' parse_drug_carriers_polypeptides_go_classifiers(TRUE)
#' parse_drug_carriers_polypeptides_go_classifiers(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides_go_classifiers <-
  function(save_table = FALSE) {
    drug_carriers_polypeptides_go_classifiers <- map_df(pkg.env$children,
                                                        ~ get_carriers_polypeptide_go_classifiers_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_carriers_polypeptides_go_classifiers,
        table_name = "drug_carriers_polypeptides_go_classifiers",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_polypeptides_go_classifiers)
  }

#' Extracts the drug carriers element and return data as data frame.
#'
#' \code{parse_drug_carriers} returns data frame of drug carriers elements.
#'
#' This functions extracts the carriers element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug carriers node attributs date frame
#'
#' @examples
#' parse_drug_carriers()
#' parse_drug_carriers(TRUE)
#' parse_drug_carriers(save_table = FALSE)
#' @export
parse_drug_carriers <- function(save_table = FALSE) {
  drug_carriers <- map_df(pkg.env$children, ~ get_carriers_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_carriers,
      table_name = "drug_carriers",
      foreign_key = "drug_key"
    )
  }
  return(drug_carriers)
}
