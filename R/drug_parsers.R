#' Extracts the main drug elements and return data as tibble.
#'
#' \code{parse_drug} returns tibble of drugs main elements.
#'
#' This functions extracts the main element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug main node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug()
#'
#' # save in database and return parsed dataframe
#' parse_drug(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' #  and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drugs", csv_path)
    if (!override_csv & file.exists(path)) {
      drugs <- readr::read_csv(path)
    } else {
      drugs <- map_df(pkg_env$children, ~ drug_df(.x)) %>% unique()
      write_csv(drugs, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drugs,
        table_name = "drug",
        primary_key = "primary_key",
        foreign_key = NULL,
        field_types = list(
          primary_key = paste0("varchar(", max(nchar(
            drugs$primary_key
          )), ")"),
          other_keys = paste0("varchar(",
                              max(nchar(
                                drugs$other_keys
                              ), na.rm = T), ")"),
          type = paste0("varchar(", max(nchar(drugs$type), na.rm = T), ")"),
          name = paste0("varchar(", max(nchar(drugs$name), na.rm = T), ")"),
          description = paste0("varchar(", max(nchar(
            drugs$description
          ),
          na.rm = T) + 10, ")"),
          cas_number = paste0("varchar(", max(nchar(
            drugs$cas_number
          ),
          na.rm = T), ")"),
          unii = paste0("varchar(", max(nchar(drugs$unii), na.rm = T), ")"),
          state = paste0("varchar(", max(nchar(drugs$state), na.rm = T), ")"),
          mechanism_of_action = paste0("varchar(", max(
            nchar(drugs$mechanism_of_action), na.rm = T
          ) + 10, ")"),
          pharmacodynamics = paste0("varchar(", max(
            nchar(drugs$pharmacodynamics), na.rm = T
          ) + 10, ")"),
          indication = paste0("varchar(", max(nchar(
            drugs$indication
          ), na.rm = T) + 10, ")"),
          absorption = paste0("varchar(", max(nchar(
            drugs$absorption
          ), na.rm = T) + 10, ")"),
          route_of_elimination = paste0("varchar(", max(
            nchar(drugs$route_of_elimination), na.rm = T
          ) + 10, ")"),
          metabolism = paste0("varchar(", max(nchar(
            drugs$metabolism
          ), na.rm = T) + 10, ")"),
          international_brands = paste0("varchar(", max(
            nchar(drugs$international_brands), na.rm = T
          ) + 10, ")"),
          fda_label = paste0("varchar(", max(nchar(
            drugs$fda_label
          ), na.rm = T), ")"),
          msds = paste0("varchar(", max(nchar(drugs$msds), na.rm = T), ")"),
          protein_binding = paste0("varchar(", max(
            nchar(drugs$protein_binding), na.rm = T
          ) + 10, ")"),
          synthesis_reference = paste0("varchar(", max(
            nchar(drugs$synthesis_reference), na.rm = T
          ) + 10, ")"),
          clearance = paste0("varchar(", max(nchar(
            drugs$clearance
          ), na.rm = T) + 10, ")"),
          half_life = paste0("varchar(", max(nchar(
            drugs$half_life
          ), na.rm = T) + 10, ")"),
          volume_of_distribution = paste0("varchar(", max(
            nchar(drugs$volume_of_distribution), na.rm = T
          ) + 10, ")"),
          toxicity = "varchar(max)"
        )
      )
    }

    return(drugs)
  }

#' Extracts the drug groups element and return data as tibble.
#'
#' \code{parse_drug_groups} returns tibble of drug groups elements.
#'
#' This functions extracts the groups element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug groups node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_groups()
#'
#' # save in database and return parsed dataframe
#' parse_drug_groups(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_groups(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_groups(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_groups(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_groups(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_groups <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_groups", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_groups <- readr::read_csv(path)
    } else {
      drug_groups <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "groups")) %>% unique()
      write_csv(drug_groups, save_csv, csv_path)
    }


    if (nrow(drug_groups) > 0) {
      colnames(drug_groups) <- c("group", "drugbank_id")
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_groups,
                    table_name = "drug_groups")
    }
    return(drug_groups)
  }

#' Extracts the drug articles element and return data as tibble.
#'
#' \code{parse_drug_articles} returns tibble of drug articles elements.
#'
#' This functions extracts the articles element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug articles node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_articles()
#'
#' # save in database and return parsed dataframe
#' parse_drug_articles(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_articles(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it
#' # does not exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_articles(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_articles(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_articles(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_articles", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_articles <- readr::read_csv(path)
    } else {
      drug_articles <-
        map_df(
          pkg_env$children,
          ~ drug_sub_df(.x, "general-references",
                        seconadary_node = "articles")
        ) %>% unique()

      write_csv(drug_articles, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_articles,
                    table_name = "drug_articles")
    }
    return(drug_articles)
  }

#' Extracts the drug books element and return data as tibble.
#'
#' \code{parse_drug_books} returns tibble of drug books elements.
#'
#' This functions extracts the books element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug books node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_books()
#'
#' # save in database and return parsed dataframe
#' parse_drug_books(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_books(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_books(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given l
#' # ocation and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_books(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_books(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_books <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_books", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_books <- readr::read_csv(path)
    } else {
      drug_books <-
        map_df(
          pkg_env$children,
          ~ drug_sub_df(.x, "general-references", seconadary_node = "textbooks")
        ) %>% unique()

      write_csv(drug_books, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_books,
                    table_name = "drug_books")
    }
    return(drug_books)
  }

#' Extracts the drug links element and return data as tibble.
#'
#' \code{parse_drug_links} returns tibble of drug links elements.
#'
#' This functions extracts the links element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug links node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_links()
#'
#' # save in database and return parsed dataframe
#' parse_drug_links(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_links(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_links(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_links <- readr::read_csv(path)
    } else {
      drug_links <-
        map_df(
          pkg_env$children,
          ~ drug_sub_df(.x, "general-references", seconadary_node = "links")
        ) %>% unique()

      write_csv(drug_links, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_links,
                    table_name = "drug_links")
    }
    return(drug_links)
  }


#' Extracts the drug synonyms element and return data as tibble.
#'
#' \code{parse_drug_synonyms} returns tibble of drug synonyms elements.
#'
#' This functions extracts the synonyms element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug synonyms node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_synonyms()
#'
#' # save in database and return parsed dataframe
#' parse_drug_synonyms(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_synonyms(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_synonyms(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_synonyms(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_synonyms(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_synonyms <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_synonyms", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_synonyms <- readr::read_csv(path)
    } else {
      drug_synonyms <-
        map_df(pkg_env$children, ~ get_synonyms_df(.x)) %>% unique()
      write_csv(drug_synonyms, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = pkg_env$con,
        df = drug_synonyms,
        table_name = "drug_synonyms",
        field_types = list(synonym = "varchar(534)")
      )
    }
    return(drug_synonyms)
  }

#' Extracts the drug products element and return data as tibble.
#'
#' \code{parse_drug_products} returns tibble of drug products elements.
#'
#' This functions extracts the products element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug products node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_products()
#'
#' # save in database and return parsed dataframe
#' parse_drug_products(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_products(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_products(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_products(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_products(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_products <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_products", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_products <- readr::read_csv(path)
    } else {
      drug_products <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "products")) %>% unique()
      write_csv(drug_products, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_products,
                    table_name = "drug_products")
    }
    return(drug_products)
  }

#' Extracts the drug calculated properties element and return data as tibble.
#'
#' \code{parse_drug_calculated_properties} returns tibble of drug calculated
#' properties elements.
#'
#' This functions extracts the calculated properties element of drug node in
#'  drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug calculated properties node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_calculated_properties()
#'
#' # save in database and return parsed dataframe
#' parse_drug_calculated_properties(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_calculated_properties(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_calculated_properties(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_calculated_properties(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_calculated_properties(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_calculated_properties <- function(save_table = FALSE,
                                             save_csv = FALSE,
                                             csv_path = ".",
                                             override_csv = FALSE) {
  path <-
    get_dataset_full_path("drug_calculated_properties", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_calculated_properties <- readr::read_csv(path)
  } else {
    drug_calculated_properties <-
      map_df(pkg_env$children,
             ~ drug_sub_df(.x, "calculated-properties")) %>% unique()

    write_csv(drug_calculated_properties, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(con = pkg_env$con,
                  df = drug_calculated_properties,
                  table_name = "drug_calculated_properties")
  }
  return(drug_calculated_properties)
}

#' Extracts the drug international brands and return data as tibble.
#'
#' \code{parse_drug_international_brands} returns tibble of drug products
#' elements.
#'
#' This functions extracts the international brands element of drug node in
#' drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#'  the new parse operation
#' @return drug international brands node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_international_brands()
#'
#' # save in database and return parsed dataframe
#' parse_drug_international_brands(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_international_brands(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' #  current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_international_brands(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_international_brands(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_international_brands(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_international_brands <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_international_brands", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_international_brands <- readr::read_csv(path)
    } else {
      drug_international_brands <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "international-brands")) %>%
        unique()

      write_csv(drug_international_brands, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_international_brands,
                    table_name = "international_brands")
    }
    return(drug_international_brands)
  }

#' Extracts the drug salts and return data as tibble.
#'
#' \code{parse_drug_salts} returns tibble of drug products elements.
#'
#' This functions extracts the salts element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed dataframe if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in t
#' he new parse operation
#' @return drug salts node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_salts()
#'
#' # save in database and return parsed dataframe
#' parse_drug_salts(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_salts(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_salts(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_salts(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_salts(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_salts <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_salts", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_salts <- readr::read_csv(path)
    } else {
      drug_salts <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "salts")) %>%
        unique()

      write_csv(drug_salts, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_salts,
                    table_name = "salts")
    }
    return(drug_salts)
  }

#' Extracts the drug mixtures element and return data as tibble.
#'
#' \code{parse_drug_mixtures} returns tibble of drug mixtures elements.
#'
#' This functions extracts the mixtures element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug mixtures node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_mixtures()
#'
#' # save in database and return parsed dataframe
#' parse_drug_mixtures(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_mixtures(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_mixtures(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_mixtures(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_mixtures(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_mixtures <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_mixtures", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_mixtures <- readr::read_csv(path)
    } else {
      drug_mixtures <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "mixtures")) %>% unique()

      write_csv(drug_mixtures, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_mixtures,
                    table_name = "drug_mixtures")
    }
    return(drug_mixtures)
  }

#' Extracts the drug packagers element and return data as tibble.
#'
#' \code{parse_drug_packagers} returns tibble of drug packagers elements.
#'
#' This functions extracts the packagers element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug packagers node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_packagers()
#'
#' # save in database and return parsed dataframe
#' parse_drug_packagers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_packagers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not
#' # exist in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_packagers(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_packagers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_packagers(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_packagers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_packagers", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_packagers <- readr::read_csv(path)
    } else {
      drug_packagers <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "packagers")) %>% unique()
      write_csv(drug_packagers, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_packagers,
                    table_name = "drug_packagers")
    }
    return(drug_packagers)
  }


#' Extracts the drug categories element and return data as tibble.
#'
#' \code{parse_drug_categories} returns tibble of drug categories elements.
#'
#' This functions extracts the categories element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug categories node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_categories()
#'
#' # save in database and return parsed dataframe
#' parse_drug_categories(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_categories(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' #  in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_categories(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_categories(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_categories(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_categories <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_categories", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_categories <- readr::read_csv(path)
    } else {
      drug_categories <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "categories")) %>% unique()
      write_csv(drug_categories, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_categories,
                    table_name = "drug_categories")
    }
    return(drug_categories)
  }

#' Extracts the drug affected organisms element and return data as tibble.
#'
#' \code{parse_drug_affected_organisms} returns tibble of drug affected
#' organisms elements.
#'
#' This functions extracts the affected organisms element of drug node in
#' drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug affected organisms node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_affected_organisms()
#'
#' # save in database and return parsed dataframe
#' parse_drug_affected_organisms(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_affected_organisms(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_affected_organisms(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_affected_organisms(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_affected_organisms(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_affected_organisms <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_affected_organisms", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_affected_organisms <- readr::read_csv(path)
    } else {
      drug_affected_organisms <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "affected-organisms")) %>%
        unique()
      write_csv(drug_affected_organisms, save_csv, csv_path)
    }


    if (nrow(drug_affected_organisms) > 0) {
      colnames(drug_affected_organisms) <-
        c("affected_organism", "drugbank_id")
    }


    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_affected_organisms,
                    table_name = "drug_affected_organisms")
    }
    return(drug_affected_organisms)
  }

#' Extracts the drug dosages element and return data as tibble.
#'
#' \code{parse_drug_dosages} returns tibble of drug dosages elements.
#'
#' This functions extracts the dosages element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug dosages node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_dosages()
#'
#' # save in database and return parsed dataframe
#' parse_drug_dosages(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_dosages(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_dosages(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' #  and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_dosages(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_dosages(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_dosages <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_dosages", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_dosages <- readr::read_csv(path)
    } else {
      drug_dosages <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "dosages")) %>% unique()

      write_csv(drug_dosages, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_dosages,
                    table_name = "drug_dosages")
    }
    return(drug_dosages)
  }


#' Extracts the drug ahfs codes element and return data as tibble.
#'
#' \code{parse_drug_ahfs_codes} returns tibble of drug ahfs codes elements.
#'
#' This functions extracts the ahfs codes element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug ahfs codes node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_ahfs_codes()
#'
#' # save in database and return parsed dataframe
#' parse_drug_ahfs_codes(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_ahfs_codes(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_ahfs_codes(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_ahfs_codes(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_ahfs_codes(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_ahfs_codes <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_ahfs_codes", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_ahfs_codes <- readr::read_csv(path)
    } else {
      drug_ahfs_codes <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "ahfs-codes")) %>% unique()
      write_csv(drug_ahfs_codes, save_csv, csv_path)
    }

    if (nrow(drug_ahfs_codes) > 0) {
      colnames(drug_ahfs_codes) <- c("ahfs_code", "drugbank_id")
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_ahfs_codes,
                    table_name = "drug_ahfs_codes")
    }
    return(drug_ahfs_codes)
  }

#' Extracts the drug pdb entries element and return data as tibble.
#'
#' \code{parse_drug_pdb_entries} returns tibble of drug pdb entries elements.
#'
#' This functions extracts the pdb entries element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug pdb entries node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_pdb_entries()
#'
#' # save in database and return parsed dataframe
#' parse_drug_pdb_entries(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pdb_entries(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pdb_entries(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_pdb_entries(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_pdb_entries(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_pdb_entries <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_pdb_entries", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_pdb_entries <- readr::read_csv(path)
    } else {
      drug_pdb_entries <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "pdb-entries")) %>% unique()
      write_csv(drug_pdb_entries, save_csv, csv_path)
    }

    if (nrow(drug_pdb_entries) > 0) {
      colnames(drug_pdb_entries) <- c("pdb_entry", "drugbank_id")
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_pdb_entries,
                    table_name = "drug_pdb_entries")
    }
    return(drug_pdb_entries)
  }

#' Extracts the drug patents element and return data as tibble.
#'
#' \code{parse_drug_patents} returns tibble of drug patents elements.
#'
#' This functions extracts the patents element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug patents node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_patents()
#'
#' # save in database and return parsed dataframe
#' parse_drug_patents(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_patents(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # 'in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_patents(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_patents(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_patents(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_patents <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_patents", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_patents <- readr::read_csv(path)
    } else {
      drug_patents <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "patents")) %>% unique()

      write_csv(drug_patents, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_patents,
                    table_name = "drug_patents")
    }
    return(drug_patents)
  }

#' Extracts the drug food interactions element and return data as tibble.
#'
#' \code{parse_drug_food_interactions} returns tibble of drug food
#' interactions elements.
#'
#' This functions extracts the food interactions element of drug node in
#' drugbank xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug food interactions node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_food_interactions()
#'
#' # save in database and return parsed dataframe
#' parse_drug_food_interactions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_food_interactions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_food_interactions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_food_interactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' #  and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_food_interactions(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_food_interactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_food_interactions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_food_interactions <- readr::read_csv(path)
    } else {
      drug_food_interactions <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "food-interactions")) %>%
        unique()
      write_csv(drug_food_interactions, save_csv, csv_path)
    }



    if (nrow(drug_food_interactions) > 0) {
      colnames(drug_food_interactions) <-
        c("food_interaction", "drugbank_id")
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_food_interactions,
                    table_name = "drug_food_interactions")
    }
    return(drug_food_interactions)
  }

#' Extracts the drug interactions element and return data as tibble.
#'
#' \code{parse_drug_interactions} returns tibble of drug interactions elements.
#'
#' This functions extracts the interactions element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug interactions node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_interactions()
#'
#' # save in database and return parsed dataframe
#' parse_drug_interactions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_interactions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_interactions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_interactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_interactions(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_interactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_drug_interactions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_drug_interactions <- readr::read_csv(path)
    } else {
      drug_drug_interactions <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "drug-interactions")) %>%
        unique()

      write_csv(drug_drug_interactions, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_drug_interactions,
                    table_name = "drug_drug_interactions")
    }
    return(drug_drug_interactions)
  }

#' Extracts the drug experimental properties element and return data as tibble.
#'
#' \code{parse_drug_experimental_properties} returns tibble of drug
#'  experimental
#'  properties elements.
#'
#' This functions extracts the experimental properties element of drug node in
#'  drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug experimental properties node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_experimental_properties()
#'
#' # save in database and return parsed dataframe
#' parse_drug_experimental_properties(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_experimental_properties(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_experimental_properties(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_experimental_properties(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current
#' # location and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_experimental_properties(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_experimental_properties <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("drug_experimental_properties", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_experimental_properties <- readr::read_csv(path)
    } else {
      drug_experimental_properties <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "experimental-properties")) %>% unique()
      write_csv(drug_experimental_properties, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_experimental_properties,
                    table_name = "drug_experimental_properties")
    }
    return(drug_experimental_properties)
  }

#' Extracts the drug external identifiers element and return data as tibble.
#'
#' \code{parse_drug_external_identifiers} returns tibble of external
#' identifiers groups elements.
#'
#' This functions extracts the external identifiers element of drug node in
#' drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug external identifiers node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_external_identifiers()
#'
#' # save in database and return parsed dataframe
#' parse_drug_external_identifiers(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_external_identifiers(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_external_identifiers(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_external_identifiers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location and
#' # return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_external_identifiers(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_drug_external_identifiers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_external_identifiers", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_external_identifiers <- readr::read_csv(path)
    } else {
      drug_external_identifiers <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "external-identifiers"))

      write_csv(drug_external_identifiers, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_external_identifiers,
                    table_name = "drug_external_identifiers")
    }
    return(drug_external_identifiers)
  }

#' Extracts the drug external links element and return data as tibble.
#'
#' \code{parse_drug_external_links} returns tibble of drug external links
#' elements.
#'
#' This functions extracts the external links element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug external links node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_external_links()
#'
#' # save in database and return parsed dataframe
#' parse_drug_external_links(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location and
#' # return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_external_links(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_external_links(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_external_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_external_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_external_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_external_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_external_links <- readr::read_csv(path)
    } else {
      drug_external_links <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "external-links")) %>%
        unique()

      write_csv(drug_external_links, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_external_links,
                    table_name = "drug_external_links")
    }
    return(drug_external_links)
  }

#' Extracts the drug snp effects element and return data as tibble.
#'
#' \code{parse_drug_snp_effects} returns tibble of snp effects groups elements.
#'
#' This functions extracts the snp effects element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug snp effects node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_drug_snp_effects()
#'
#' # save in database and return parsed dataframe
#' parse_drug_snp_effects(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_snp_effects(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist
#' # in current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_snp_effects(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_drug_snp_effects(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_drug_snp_effects(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
parse_drug_snp_effects <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <- get_dataset_full_path("drug_snp_effects", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_snp_effects <- readr::read_csv(path)
    } else {
      drug_snp_effects <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "snp-effects")) %>% unique()

      write_csv(drug_snp_effects, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = drug_snp_effects,
                    table_name = "drug_snp_effects")
    }
    return(drug_snp_effects)
  }

#' Extracts the drug snp adverse drug reactions element and return data as
#' tibble.
#'
#' \code{parse_snp_adverse_reactions} returns tibble of drug
#'  snp adverse drug reactions elements.
#'
#' This functions extracts the groups element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
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
#' @return drug snp adverse drug reactions node attributes tibble
#'
#' @examples
#' \donttest{
#' # return only the parsed dataframe
#' parse_snp_adverse_reactions()
#'
#' # save in database and return parsed dataframe
#' parse_snp_adverse_reactions(save_table = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_snp_adverse_reactions(save_csv = TRUE)
#'
#' # save in database, save parsed dataframe as csv if it does not exist in
#' # current location and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_snp_adverse_reactions(ssave_table = TRUE, save_csv = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in given location
#' # and return parsed dataframe.
#' # If the csv exist before read it and return its data.
#' parse_snp_adverse_reactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed dataframe as csv if it does not exist in current location
#' # and return parsed dataframe.
#' # If the csv exist override it and return it.
#' parse_snp_adverse_reactions(save_csv = TRUE, csv_path = TRUE,
#' override = TRUE)
#' }
#' @export
parse_snp_adverse_reactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    path <-
      get_dataset_full_path("snp_adverse_reactions", csv_path)
    if (!override_csv & file.exists(path)) {
      snp_adverse_reactions <- readr::read_csv(path)
    } else {
      snp_adverse_reactions <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "snp-adverse-drug-reactions")) %>% unique()

      write_csv(snp_adverse_reactions, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = pkg_env$con,
                    df = snp_adverse_reactions,
                    table_name = "snp_adverse_reactions")
    }
    return(snp_adverse_reactions)
  }
