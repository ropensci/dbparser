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
#'  returns data frame of drug targets polypeptides external identifiers elements.
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
#' @return drug targets polypeptides external identifiers node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_polypeptides_external_identifiers()
#' parse_drug_targets_polypeptides_external_identifiers(TRUE)
#' parse_drug_targets_polypeptides_external_identifiers(save_table = FALSE)
#' }
#' @export
parse_drug_targets_polypeptides_external_identifiers <-
  function(save_table = FALSE) {
    drug_targets_polypeptide_external_identifiers <-
      map_df(pkg.env$children,
             ~ get_targets_polypeptide_external_identifiers_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_targets_polypeptide_external_identifiers,
        table_name = "drug_targets_polypeptides_external_identifiers",
        save_table_only = TRUE
      )
    }
    return(tibble::as_tibble(drug_targets_polypeptide_external_identifiers))
  }


#' Extracts the drug targets polypeptides synonyms element
#' and return data as data frame.
#'
#' \code{parse_drug_targets_polypeptides_synonyms} returns data
#'  frame of drug targets polypeptides synonyms elements.
#'
#' This functions extracts the targets polypeptides synonyms element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets polypeptides synonyms node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_polypeptides_synonyms()
#' parse_drug_targets_polypeptides_synonyms(TRUE)
#' parse_drug_targets_polypeptides_synonyms(save_table = FALSE)
#' }
#' @export
parse_drug_targets_polypeptides_synonyms <-
  function(save_table = FALSE) {
    drug_targets_polypeptide_synonyms <- map_df(pkg.env$children,
                                                ~ get_targets_polypeptide_synonyms_df(.x))
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
#' This functions extracts the targets polypeptides pfams element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets polypeptides pfams node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_polypeptides_pfams()
#' parse_drug_targets_polypeptides_pfams(TRUE)
#' parse_drug_targets_polypeptides_pfams(save_table = FALSE)
#' }
#' @export
parse_drug_targets_polypeptides_pfams <-
  function(save_table = FALSE) {
    drug_targets_polypeptide_pfams <- map_df(pkg.env$children,
                                             ~ get_targets_polypeptide_pfams_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_targets_polypeptide_pfams,
        table_name = "drug_targets_polypeptides_pfams",
        save_table_only = TRUE
      )
    }
    return(tibble::as_tibble(drug_targets_polypeptide_pfams))
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
#' @return drug targets polypeptides go classifiers node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_polypeptides_go_classifiers()
#' parse_drug_targets_polypeptides_go_classifiers(TRUE)
#' parse_drug_targets_polypeptides_go_classifiers(save_table = FALSE)
#' }
#' @export
parse_drug_targets_polypeptides_go_classifiers <-
  function(save_table = FALSE) {
    drug_targets_polypeptides_go_classifiers <- map_df(pkg.env$children,
                                                       ~ get_targets_polypeptide_go_classifiers_df(.x))
    if (save_table) {
      save_drug_sub(
        con = pkg.env$con,
        df = drug_targets_polypeptides_go_classifiers,
        table_name = "drug_targets_polypeptides_go_classifiers",
        save_table_only = TRUE
      )
    }
    return(tibble::as_tibble(drug_targets_polypeptides_go_classifiers))
  }

#' Extracts the drug targets actions element and return data as data frame.
#'
#' \code{parse_drug_targets_actions} returns data frame of drug targets
#' actions elements.
#'
#' This functions extracts the targets actions element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets actions node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_actions()
#' parse_drug_targets_actions(TRUE)
#' parse_drug_targets_actions(save_table = FALSE)
#' }
#' @export
parse_drug_targets_actions <- function(save_table = FALSE) {
  drug_targets_actions <-
    map_df(pkg.env$children, ~ get_targets_actions_df(.x))

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
  return(tibble::as_tibble(drug_targets_actions))
}

#' Extracts the drug targets articles element and return data as data frame.
#'
#' \code{parse_drug_targets_articles} returns data frame of drug targets
#' articles elements.
#'
#' This functions extracts the targets articles element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets articles node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_articles()
#' parse_drug_targets_articles(TRUE)
#' parse_drug_targets_articles(save_table = FALSE)
#' }
#' @export
parse_drug_targets_articles <- function(save_table = FALSE) {
  drug_targets_articles <-
    map_df(pkg.env$children, ~ get_targets_articles_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets_articles,
      table_name = "drug_targets_articles",
      save_table_only = TRUE
    )
  }
  return(tibble::as_tibble(drug_targets_articles))
}

#' Extracts the drug targets textbooks element and return data as data frame.
#'
#' \code{parse_drug_targets_textbooks} returns data frame of drug
#'  targets textbooks elements.
#'
#' This functions extracts the targets textbooks element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets textbooks node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_textbooks()
#' parse_drug_targets_textbooks(TRUE)
#' parse_drug_targets_textbooks(save_table = FALSE)
#' }
#' @export
parse_drug_targets_textbooks <- function(save_table = FALSE) {
  drug_targets_textbooks <-
    map_df(pkg.env$children, ~ get_targets_textbooks_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets_textbooks,
      table_name = "drug_targets_textbooks",
      save_table_only = TRUE
    )
  }
  return(tibble::as_tibble(drug_targets_textbooks))
}

#' Extracts the drug targets links element and return data as data frame.
#'
#' \code{parse_drug_targets_links} returns data frame of drug targets links elements.
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
#' @return drug targets_links node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_links()
#' parse_drug_targets_links(TRUE)
#' parse_drug_targets_links(save_table = FALSE)
#' }
#' @export
parse_drug_targets_links <- function(save_table = FALSE) {
  drug_targets_links <- map_df(pkg.env$children, ~ get_targets_links_df(.x))
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
                      )) + 100, ")", sep = ""),
        url = paste("varchar(", max(nchar(
          drug_targets_links$url
        )) + 100, ")", sep = "")
      )
    )
  }
  return(tibble::as_tibble(drug_targets_links))
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
#' @return drug targets polypeptides node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets_polypeptides()
#' parse_drug_targets_polypeptides(TRUE)
#' parse_drug_targets_polypeptides(save_table = FALSE)
#' }
#' @export
parse_drug_targets_polypeptides <- function(save_table = FALSE) {
  drug_targets_polypeptides <-
    map_df(pkg.env$children, ~ get_targets_polypeptide_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets_polypeptides,
      table_name = "drug_targets_polypeptides",
      save_table_only = TRUE,
      field.types = list(
        general_function = paste("varchar(",
                                 max(
                                   nchar(drug_targets_polypeptides$general_function)
                                 ),
                                 ")", sep = ""),
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
#' @return drug target node attributes date frame
#'
#' @examples
#' \donttest{
#' parse_drug_targets()
#' parse_drug_targets(TRUE)
#' parse_drug_targets(save_table = FALSE)
#' }
#' @export
parse_drug_targets <- function(save_table = FALSE) {
  drug_targets <- map_df(pkg.env$children, ~ get_targets_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_targets,
      table_name = "drug_targets",
      foreign_key = "drug_key"
    )
  }
  return(drug_targets)
}
