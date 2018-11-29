get_transporters_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]),
                ~ get_organizm_rec(.x, xmlValue(rec["drugbank-id"][[1]]))))
}

get_transporters_actions_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]), ~ drug_sub_df(.x, "actions", id = "id")))
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
  return(map_df(xmlChildren(rec[["transporters"]]), ~ get_polypeptide_synonyms(.x)))
}

get_transporters_polypeptide_pfams_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]), ~ get_polypeptide_pfams(.x)))
}

get_transporters_polypeptide_go_classifiers_df <- function(rec) {
  return(map_df(xmlChildren(rec[["transporters"]]), ~ get_polypeptide_go_classifiers(.x)))
}

#' Extracts the drug transporters actions element and return data as data frame.
#'
#' \code{parse_drug_transporters actions} returns data frame of drug transporters
#' actions elements.
#'
#' This functions extracts the transporters actions element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters actions node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_actions()
#' parse_drug_transporters_actions(TRUE)
#' parse_drug_transporters_actions(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_actions <- function(save_table = FALSE) {
  drug_transporters_actions <-
    map_df(pkg.env$children, ~ get_transporters_actions_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_transporters_actions,
      table_name = "drug_transporters_actions",
      save_table_only = TRUE
    )
  }
  return(drug_transporters_actions)
}


#' Extracts the drug transporters articles element and return data as data frame.
#'
#' \code{parse_drug_transporters articles} returns data frame of drug transporters articles elements.
#'
#' This functions extracts the transporters articles element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters articles node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_articles()
#' parse_drug_transporters_articles(TRUE)
#' parse_drug_transporters_articles(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_articles <- function(save_table = FALSE) {
  drug_transporters_articles <-
    map_df(pkg.env$children, ~ get_transporters_articles_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_transporters_articles,
      table_name = "drug_transporters_articles",
      save_table_only = TRUE
    )
  }
  return(drug_transporters_articles)
}


#' Extracts the drug transporters textbooks element and return data as data frame.
#'
#' \code{parse_drug_transporters_textbooks} returns data frame of drug transporters
#'  textbooks elements.
#'
#' This functions extracts the transporters textbooks element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters textbooks node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_textbooks()
#' parse_drug_transporters_textbooks(TRUE)
#' parse_drug_transporters_textbooks(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_textbooks <- function(save_table = FALSE) {
  drug_transporters_textbooks <-
    map_df(pkg.env$children, ~ get_transporters_textbooks_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_transporters_textbooks,
      table_name = "drug_transporters_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_transporters_textbooks)
}


#' Extracts the drug transporters links element and return data as data frame.
#'
#' \code{parse_drug_transporters_links} returns data frame of drug transporters_ inks elements.
#'
#' This functions extracts the transporters links element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters links node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_links()
#' parse_drug_transporters_links(TRUE)
#' parse_drug_transporters_links(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_links <- function(save_table = FALSE) {
  drug_transporters_links <-
    map_df(pkg.env$children, ~ get_transporters_links_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters polypeptides node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_polypeptides()
#' parse_drug_transporters_polypeptides(TRUE)
#' parse_drug_transporters_polypeptides(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_polypeptides <-
  function(save_table = FALSE) {
    drug_transporters_polypeptides <-
      map_df(pkg.env$children, ~ get_transporters_polypeptide_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_transporters_polypeptides,
        table_name = "drug_transporters_polypeptides",
        save_table_only = TRUE,
        field.types = list(
          general_function = paste("varchar(",
                                   max(
                                     nchar(drug_transporters_polypeptides$general_function)
                                   ),
                                   ")", sep = ""),
          specific_function = paste("varchar(",
                                    max(
                                      nchar(drug_transporters_polypeptides$specific_function)
                                    ),
                                    ")", sep = ""),
          amino_acid_sequence = paste("varchar(",
                                      max(
                                        nchar(drug_transporters_polypeptides$amino_acid_sequence)
                                      ),
                                      ")", sep = ""),
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters polypeptides external identifiers
#'  node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_polypeptides_external_identifiers()
#' parse_drug_transporters_polypeptides_external_identifiers(TRUE)
#' parse_drug_transporters_polypeptides_external_identifiers(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_polypeptides_external_identifiers <-
  function(save_table = FALSE) {
    drug_transporters_polypeptide_external_identifiers <-
      map_df(pkg.env$children,
             ~ get_transporters_polypeptide_external_identifiers_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
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
#' \code{parse_drug_transporters_polypeptides_synonyms} returns data
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters polypeptides synonyms node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_polypeptides_synonyms()
#' parse_drug_transporters_polypeptides_synonyms(TRUE)
#' parse_drug_transporters_polypeptides_synonyms(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_polypeptides_synonyms <-
  function(save_table = FALSE) {
    drug_transporter_polypeptide_synonyms <- map_df(pkg.env$children,
                                                    ~ get_transporters_polypeptide_synonyms_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
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
#' \code{parse_drug_transporters_polypeptides_pfams} returns data frame
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters polypeptides pfams node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_polypeptides_pfams()
#' parse_drug_transporters_polypeptides_pfams(TRUE)
#' parse_drug_transporters_polypeptides_pfams(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_polypeptides_pfams <-
  function(save_table = FALSE) {
    drug_transporters_polypeptides_pfams <- map_df(pkg.env$children,
                                                   ~ get_transporters_polypeptide_pfams_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
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
#' \code{parse_drug_transporters_polypeptides_go_classifiers}
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters polypeptides go classifiers node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters_polypeptides_go_classifiers()
#' parse_drug_transporters_polypeptides_go_classifiers(TRUE)
#' parse_drug_transporters_polypeptides_go_classifiers(save_table = FALSE)
#' }
#' @export
parse_drug_transporters_polypeptides_go_classifiers <-
  function(save_table = FALSE) {
    drug_transporters_polypeptides_go_classifiers <- map_df(pkg.env$children,
                                                            ~ get_transporters_polypeptide_go_classifiers_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_transporters_polypeptides_go_classifiers,
        table_name = "drug_transporters_polypeptides_go_classifiers",
        save_table_only = TRUE
      )
    }
    return(drug_transporters_polypeptides_go_classifiers)
  }




#' Extracts the drug transporters element and return data as data frame.
#'
#' \code{parse_drug_transporters} returns data frame of drug transporters elements.
#'
#' This functions extracts the transporters element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug transporters node attributs date frame
#'
#' @examples
#' \dontrun{
#' parse_drug_transporters()
#' parse_drug_transporters(TRUE)
#' parse_drug_transporters(save_table = FALSE)
#' }
#' @export
parse_drug_transporters <- function(save_table = FALSE) {
  drug_transporters <- map_df(pkg.env$children, ~ get_transporters_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_transporters,
      table_name = "drug_transporters",
      foreign_key = "drug_key"
    )
  }
  return(drug_transporters)
}
