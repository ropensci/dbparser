#' Extracts the main drug elements and return data as data frame.
#'
#' \code{parse_drug} returns data frame of drugs main elements.
#'
#' This functions extracts the main element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug main node attributs data frame
#'
#' @examples
#' parse_drug()
#' parse_drug(FALSE)
#' parse_drug(save_table = FALSE)
#' @export
parse_drug <- function(save_table = TRUE) {
    # db connection
    drugs <- map_df(children, ~drug_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drugs,
                    table_name = "drug",
                    primary_key = "primary_key",
                    foreign_key = NULL,
                    field.types = list(description = "varchar(6349)",
                                       mechanism_of_action = "varchar(7189)",
                                       pharmacodynamics = "varchar(3179)",
                                       indication = "varchar(3165)",
                                       absorption = "nvarchar(3579)",
                                       route_of_elimination = "varchar(1324)",
                                       metabolism = "varchar(2926)",
                                       international_brands = "varchar(2904)",
                                       protein_binding = "varchar(778)",
                                       synthesis_reference = "varchar(946)",
                                       clearance = "varchar(2128)",
                                       half_life = "varchar(1173)",
                                       route_of_elimination = "varchar(1324)",
                                       absorption = "varchar(3579)",
                                       volume_of_distribution = "varchar(1378)",
                                       toxicity = "varchar(max)",
                                       created = "date",
                                       updated = "date"))
    }

    return(drugs)
}

#' Extracts the drug groups element and return data as data frame.
#'
#' \code{parse_drug_groups} returns data frame of drug groups elements.
#'
#' This functions extracts the groups element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug groups node attributs date frame
#'
#' @examples
#' parse_drug_groups()
#' parse_drug_groups(FALSE)
#' parse_drug_groups(save_table = FALSE)
#' @export
parse_drug_groups <- function(save_table = TRUE) {
    drug_groups <- map_df(children, ~drug_sub_df(.x, "groups"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_groups, table_name = "drug_groups")
    }
    return(drug_groups)
}

#' Extracts the drug articles element and return data as data frame.
#'
#' \code{parse_drug_articles} returns data frame of drug articles elements.
#'
#' This functions extracts the articles element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug articles node attributs date frame
#'
#' @examples
#' parse_drug_articles()
#' parse_drug_articles(FALSE)
#' parse_drug_articles(save_table = FALSE)
#' @export
parse_drug_articles <- function(save_table = TRUE) {
    drug_articles <- map_df(children, ~drug_sub_df(.x, "general-references",
                                                   seconadary_node = "articles"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_articles, table_name = "drug_articles")
    }
    return(drug_articles)
}

#' Extracts the drug books element and return data as data frame.
#'
#' \code{parse_drug_books} returns data frame of drug books elements.
#'
#' This functions extracts the books element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug books node attributs date frame
#'
#' @examples
#' parse_drug_books()
#' parse_drug_books(FALSE)
#' parse_drug_books(save_table = FALSE)
#' @export
parse_drug_books <- function(save_table = TRUE) {
    drug_books <- map_df(children, ~drug_sub_df(.x, "general-references", seconadary_node = "textbooks"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_books, table_name = "drug_books")
    }
    return(drug_books)
}

#' Extracts the drug links element and return data as data frame.
#'
#' \code{parse_drug_links} returns data frame of drug links elements.
#'
#' This functions extracts the links element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug links node attributs date frame
#'
#' @examples
#' parse_drug_links()
#' parse_drug_links(FALSE)
#' parse_drug_links(save_table = FALSE)
#' @export
parse_drug_links <- function(save_table = TRUE) {
    drug_links <- map_df(children, ~drug_sub_df(.x, "general-references", seconadary_node = "links"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_links, table_name = "drug_links")
    }
    return(drug_links)
}

#' Extracts the drug classfications element and return data as data frame.
#'
#' \code{parse_drug_classfications} returns data frame of drug classfications elements.
#'
#' This functions extracts the classfications element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug classfications node attributs date frame
#'
#' @examples
#' parse_drug_classfications()
#' parse_drug_classfications(FALSE)
#' parse_drug_classfications(save_table = FALSE)
#' @export
parse_drug_classfications <- function(save_table = TRUE) {
    drug_classfications <- map_df(children, ~drug_classfications_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_classfications, table_name = "drug_classfications")
    }
    return(drug_classfications)
}

#' Extracts the drug synonyms element and return data as data frame.
#'
#' \code{parse_drug_synonyms} returns data frame of drug synonyms elements.
#'
#' This functions extracts the synonyms element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug synonyms node attributs date frame
#'
#' @examples
#' parse_drug_synonyms()
#' parse_drug_synonyms(FALSE)
#' parse_drug_synonyms(save_table = FALSE)
#' @export
parse_drug_synonyms <- function(save_table = TRUE) {
    drug_synonyms <- map_df(children, ~get_synonyms_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_synonyms,
                    table_name = "drug_synonyms",
                    field.types = list(synonym = "varchar(534)"))
    }
    return(drug_synonyms)
}

#' Extracts the drug products element and return data as data frame.
#'
#' \code{parse_drug_products} returns data frame of drug products elements.
#'
#' This functions extracts the products element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug products node attributs date frame
#'
#' @examples
#' parse_drug_products()
#' parse_drug_products(FALSE)
#' parse_drug_products(save_table = FALSE)
#' @export
parse_drug_products <- function(save_table = TRUE) {
    drug_products <- map_df(children, ~drug_sub_df(.x, "products"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_products, table_name = "drug_products")
    }
    return(drug_products)
}

#' Extracts the drug mixtures element and return data as data frame.
#'
#' \code{parse_drug_mixtures} returns data frame of drug mixtures elements.
#'
#' This functions extracts the mixtures element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug mixtures node attributs date frame
#'
#' @examples
#' parse_drug_mixtures()
#' parse_drug_mixtures(FALSE)
#' parse_drug_mixtures(save_table = FALSE)
#' @export
parse_drug_mixtures <- function(save_table = TRUE) {
    drug_mixtures <- map_df(children, ~drug_sub_df(.x, "mixtures"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_mixtures, table_name = "drug_mixtures")
    }
    return(drug_mixtures)
}

#' Extracts the drug packagers element and return data as data frame.
#'
#' \code{parse_drug_packagers} returns data frame of drug packagers elements.
#'
#' This functions extracts the packagers element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug packagers node attributs date frame
#'
#' @examples
#' parse_drug_packagers()
#' parse_drug_packagers(FALSE)
#' parse_drug_packagers(save_table = FALSE)
#' @export
parse_drug_packagers <- function(save_table = TRUE) {
    drug_packagers <- map_df(children, ~drug_sub_df(.x, "packagers"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_packagers, table_name = "drug_packagers")
    }
    return(drug_packagers)
}

#' Extracts the drug manufacturers element and return data as data frame.
#'
#' \code{parse_drug_manufacturers} returns data frame of drug manufacturers elements.
#'
#' This functions extracts the manufacturers element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug manufacturers node attributs date frame
#'
#' @examples
#' parse_drug_manufacturers()
#' parse_drug_manufacturers(FALSE)
#' parse_drug_manufacturers(save_table = FALSE)
#' @export
parse_drug_manufacturers <- function(save_table = TRUE) {
    drug_manufacturers <- map_df(children, ~drug_sub_df(.x, "manufacturers"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_manufacturers, table_name = "drug_manufacturers")
    }
    return(drug_manufacturers)
}

#' Extracts the drug prices element and return data as data frame.
#'
#' \code{parse_drug_prices} returns data frame of drug prices elements.
#'
#' This functions extracts the prices element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug prices node attributs date frame
#'
#' @examples
#' parse_drug_prices()
#' parse_drug_prices(FALSE)
#' parse_drug_prices(save_table = FALSE)
#' @export
parse_drug_prices <- function(save_table = TRUE) {
    drug_prices <- map_df(children, ~get_prices_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_prices, table_name = "drug_prices")
    }
    return(drug_prices)
}

#' Extracts the drug categories element and return data as data frame.
#'
#' \code{parse_drug_categories} returns data frame of drug categories elements.
#'
#' This functions extracts the categories element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug categories node attributs date frame
#'
#' @examples
#' parse_drug_categories()
#' parse_drug_categories(FALSE)
#' parse_drug_categories(save_table = FALSE)
#' @export
parse_drug_categories <- function(save_table = TRUE) {
    drug_categories <- map_df(children, ~drug_sub_df(.x, "categories"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_categories, table_name = "drug_categories")
    }
    return(drug_categories)
}

#' Extracts the drug affected organisms element and return data as data frame.
#'
#' \code{parse_drug_affected_organisms} returns data frame of drug affected organisms elements.
#'
#' This functions extracts the affected organisms element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug affected organisms node attributs date frame
#'
#' @examples
#' parse_drug_affected_organisms()
#' parse_drug_affected_organisms(FALSE)
#' parse_drug_affected_organisms(save_table = FALSE)
#' @export
parse_drug_affected_organisms <- function(save_table = TRUE) {
    drug_affected_organisms <- map_df(children, ~drug_sub_df(.x, "affected-organisms"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_affected_organisms, table_name = "drug_affected_organisms")
    }
    return(drug_affected_organisms)
}

#' Extracts the drug dosages element and return data as data frame.
#'
#' \code{parse_drug_dosages} returns data frame of drug dosages elements.
#'
#' This functions extracts the dosages element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug dosages node attributs date frame
#'
#' @examples
#' parse_drug_dosages()
#' parse_drug_dosages(FALSE)
#' parse_drug_dosages(save_table = FALSE)
#' @export
parse_drug_dosages <- function(save_table = TRUE) {
    drug_dosages <- map_df(children, ~drug_sub_df(.x, "dosages"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_dosages, table_name = "drug_dosages")
    }
    return(drug_dosages)
}

#' Extracts the drug atc codes element and return data as data frame.
#'
#' \code{parse_drug_atc_codes} returns data frame of drug atc codes elements.
#'
#' This functions extracts the atc codes element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug atc_codes node attributs date frame
#'
#' @examples
#' parse_drug_atc_codes()
#' parse_drug_atc_codes(FALSE)
#' parse_drug_atc_codes(save_table = FALSE)
#' @export
parse_drug_atc_codes <- function(save_table = TRUE) {
    drug_atc_codes <- map_df(children, ~get_atc_codes_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_atc_codes, table_name = "drug_atc_codes")
    }
    return(drug_atc_codes)
}

#' Extracts the drug ahfs codes element and return data as data frame.
#'
#' \code{parse_drug_ahfs_codes} returns data frame of drug ahfs codes elements.
#'
#' This functions extracts the ahfs codes element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug ahfs codes node attributs date frame
#'
#' @examples
#' parse_drug_ahfs_codes()
#' parse_drug_ahfs_codes(FALSE)
#' parse_drug_ahfs_codes(save_table = FALSE)
#' @export
parse_drug_ahfs_codes <- function(save_table = TRUE) {
    drug_ahfs_codes <- map_df(children, ~drug_sub_df(.x, "ahfs-codes"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_ahfs_codes, table_name = "drug_ahfs_codes")
    }
    return(drug_ahfs_codes)
}

#' Extracts the drug pdb entries element and return data as data frame.
#'
#' \code{parse_drug_pdb_entries} returns data frame of drug pdb entries elements.
#'
#' This functions extracts the pdb entries element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug pdb entries node attributs date frame
#'
#' @examples
#' parse_drug_pdb_entries()
#' parse_drug_pdb_entries(FALSE)
#' parse_drug_pdb_entries(save_table = FALSE)
#' @export
parse_drug_pdb_entries <- function(save_table = TRUE) {
    drug_pdb_entries <- map_df(children, ~drug_sub_df(.x, "pdb-entries"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_pdb_entries, table_name = "drug_pdb_entries")
    }
    return(drug_pdb_entries)
}

#' Extracts the drug patents element and return data as data frame.
#'
#' \code{parse_drug_patents} returns data frame of drug patents elements.
#'
#' This functions extracts the patents element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug patents node attributs date frame
#'
#' @examples
#' parse_drug_patents()
#' parse_drug_patents(FALSE)
#' parse_drug_patents(save_table = FALSE)
#' @export
parse_drug_patents <- function(save_table = TRUE) {
    drug_patents <- map_df(children, ~drug_sub_df(.x, "patents"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_patents, table_name = "drug_patents")
    }
    return(drug_patents)
}

#' Extracts the drug food interactions element and return data as data frame.
#'
#' \code{parse_drug_food_interactions} returns data frame of drug food
#' interactions elements.
#'
#' This functions extracts the food interactions element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug food interactions node attributs date frame
#'
#' @examples
#' parse_drug_food_interactions()
#' parse_drug_food_interactions(FALSE)
#' parse_drug_food_interactions(save_table = FALSE)
#' @export
parse_drug_food_interactions <- function(save_table = TRUE) {
    drug_food_interactions <- map_df(children, ~drug_sub_df(.x, "food-interactions"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_food_interactions, table_name = "drug_food_interactions")
    }
    return(drug_food_interactions)
}

#' Extracts the drug interactions element and return data as data frame.
#'
#' \code{parse_drug_interactions} returns data frame of drug interactions elements.
#'
#' This functions extracts the interactions element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug interactions node attributs date frame
#'
#' @examples
#' parse_drug_interactions()
#' parse_drug_interactions(FALSE)
#' parse_drug_interactions(save_table = FALSE)
#' @export
parse_drug_interactions <- function(save_table = TRUE) {
    drug_drug_interactions <- map_df(children, ~drug_sub_df(.x, "drug-interactions"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_drug_interactions, table_name = "drug_drug_interactions")
    }
    return(drug_drug_interactions)
}

#' Extracts the drug sequences element and return data as data frame.
#'
#' \code{parse_drug_sequences} returns data frame of drug sequences elements.
#'
#' This functions extracts the sequences element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug sequences node attributs date frame
#'
#' @examples
#' parse_drug_sequences()
#' parse_drug_sequences(FALSE)
#' parse_drug_sequences(save_table = FALSE)
#' @export
parse_drug_sequences <- function(save_table = TRUE) {
    drug_sequences <- map_df(children, ~get_sequences_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_sequences, table_name = "drug_sequences")
    }
    return(drug_sequences)
}

#' Extracts the drug experimental properties element and return data as data frame.
#'
#' \code{parse_drug_experimental_properties} returns data frame of drug experimental
#'  properties elements.
#'
#' This functions extracts the experimental properties element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug experimental properties node attributs date frame
#'
#' @examples
#' parse_drug_experimental_properties()
#' parse_drug_experimental_properties(FALSE)
#' parse_drug_experimental_properties(save_table = FALSE)
#' @export
parse_drug_experimental_properties <- function(save_table = TRUE) {
    drug_experimental_properties <- map_df(children, ~drug_sub_df(.x, "experimental-properties"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_experimental_properties,
                    table_name = "drug_experimental_properties")
    }
    return(drug_experimental_properties)
}

#' Extracts the drug external identifiers element and return data as data frame.
#'
#' \code{parse_drug_external_identifiers} returns data frame of external
#' identifiers groups elements.
#'
#' This functions extracts the external identifiers element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug external identifiers node attributs date frame
#'
#' @examples
#' parse_drug_external_identifiers()
#' parse_drug_external_identifiers(FALSE)
#' parse_drug_external_identifiers(save_table = FALSE)
#' @export
parse_drug_external_identifiers <- function(save_table = TRUE) {
    drug_external_identifiers <- map_df(children, ~drug_sub_df(.x, "external-identifiers"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_external_identifiers, table_name = "drug_external_identifiers")
    }
    return(drug_external_identifiers)
}

#' Extracts the drug external links element and return data as data frame.
#'
#' \code{parse_drug_external_links} returns data frame of drug external links elements.
#'
#' This functions extracts the external links element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug external links node attributs date frame
#'
#' @examples
#' parse_drug_external_links()
#' parse_drug_external_links(FALSE)
#' parse_drug_external_links(save_table = FALSE)
#' @export
parse_drug_external_links <- function(save_table = TRUE) {
    drug_external_links <- map_df(children, ~drug_sub_df(.x, "external-links"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_external_links, table_name = "drug_external_links")
    }
    return(drug_external_links)
}


#' Extracts the drug pathway element and return data as data frame.
#'
#' \code{parse_drug_pathway} returns data frame of drug pathway elements.
#'
#' This functions extracts the groups element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug pathway node attributs date frame
#'
#' @examples
#' parse_drug_pathway()
#' parse_drug_pathway(FALSE)
#' parse_drug_pathway(save_table = FALSE)
#' @export
parse_drug_pathway <- function(save_table = TRUE) {
    drug_pathway <- map_df(children, ~get_pathways_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_pathway, table_name = "drug_pathway")
    }
    return(drug_pathway)
}

#' Extracts the drug pathway drugs element and return data as data frame.
#'
#' \code{parse_drug_pathway_drugs} returns data frame of drug pathway drugs elements.
#'
#' This functions extracts the pathway drugs element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug pathway drugs node attributs date frame
#'
#' @examples
#' parse_drug_pathway_drugs()
#' parse_drug_pathway_drugs(FALSE)
#' parse_drug_pathway_drugs(save_table = FALSE)
#' @export
parse_drug_pathway_drugs <- function(save_table = TRUE) {
    drug_pathway_drugs <- map_df(children, ~get_pathways_drugs_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_pathway_drugs,
                    table_name = "drug_pathway_drugs",
                    save_table_only = TRUE)
    }
    return(drug_pathway_drugs)
}

#' Extracts the drug pathway enzyme element and return data as data frame.
#'
#' \code{parse_drug_pathway_enzyme} returns data frame of drug pathway enzyme elements.
#'
#' This functions extracts the pathway enzyme element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug pathway enzyme node attributs date frame
#'
#' @examples
#' parse_drug_pathway_enzyme()
#' parse_drug_pathway_enzyme(FALSE)
#' parse_drug_pathway_enzyme(save_table = FALSE)
#' @export
parse_drug_pathway_enzyme <- function(save_table = TRUE) {
    drug_pathway_enzymes <- map_df(children, ~get_pathways_enzymes_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_pathway_enzymes,
                    table_name = "drug_pathway_enzyme",
                    save_table_only = TRUE)
    }
    return(drug_pathway_enzymes)
}

#' Extracts the drug snp effects element and return data as data frame.
#'
#' \code{parse_drug_snp_effects} returns data frame of snp effects groups elements.
#'
#' This functions extracts the snp effects element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug snp effects node attributs date frame
#'
#' @examples
#' parse_drug_snp_effects()
#' parse_drug_snp_effects(FALSE)
#' parse_drug_snp_effects(save_table = FALSE)
#' @export
parse_drug_snp_effects <- function(save_table = TRUE) {
    drug_snp_effects <- map_df(children, ~drug_sub_df(.x, "snp-effects"))
    if (save_table) {
      save_drug_sub(con = con, df = drug_snp_effects, table_name = "drug_snp_effects")
    }
    return(drug_snp_effects)
}

#' Extracts the drug snp adverse drug reactions element and return data as data frame.
#'
#' \code{parse_drug_snp_adverse_drug_reactions} returns data frame of drug
#'  snp adverse drug reactions elements.
#'
#' This functions extracts the groups element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug snp adverse drug reactions node attributs date frame
#'
#' @examples
#' parse_drug_snp_adverse_drug_reactions()
#' parse_drug_snp_adverse_drug_reactions(FALSE)
#' parse_drug_snp_adverse_drug_reactions(save_table = FALSE)
#' @export
parse_drug_snp_adverse_drug_reactions <- function(save_table = TRUE) {
    drug_snp_adverse_drug_reactions <- map_df(children, ~drug_sub_df(.x, "snp-adverse-drug-reactions"))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_snp_adverse_drug_reactions,
                    table_name = "drug_snp_adverse_drug_reactions")
    }
    return(drug_snp_adverse_drug_reactions)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes node attributs date frame
#'
#' @examples
#' parse_drug_enzymes()
#' parse_drug_enzymes(FALSE)
#' parse_drug_enzymes(save_table = FALSE)
#' @export
parse_drug_enzymes <- function(save_table = TRUE) {
    drug_enzymes <- map_df(children, ~get_enzymes_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_enzymes, table_name = "drug_enzymes")
    }
    return(drug_enzymes)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes actions node attributs date frame
#'
#' @examples
#' parse_drug_enzymes_actions()
#' parse_drug_enzymes_actions(FALSE)
#' parse_drug_enzymes_actions(save_table = FALSE)
#' @export
parse_drug_enzymes_actions <- function(save_table = TRUE) {
    drug_enzymes_actions <- map_df(children, ~get_enzymes_actions_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_enzymes_actions,
                    table_name = "drug_enzymes_actions",
                    save_table_only = TRUE)
    }
    return(drug_enzymes_actions)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets actions node attributs date frame
#'
#' @examples
#' parse_drug_targets_actions()
#' parse_drug_targets_actions(FALSE)
#' parse_drug_targets_actions(save_table = FALSE)
#' @export
parse_drug_targets_actions <- function(save_table = TRUE) {
    drug_targets_actions <- map_df(children, ~get_targets_actions_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_targets_actions,
                    table_name = "drug_targets_actions",
                    save_table_only = TRUE)
    }
    return(drug_targets_actions)
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
#' parse_drug_carriers_actions(FALSE)
#' parse_drug_carriers_actions(save_table = FALSE)
#' @export
parse_drug_carriers_actions <- function(save_table = TRUE) {
    drug_carriers_actions <- map_df(children, ~get_carriers_actions_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_carriers_actions,
                    table_name = "drug_carriers_actions",
                    save_table_only = TRUE)
    }
    return(drug_carriers_actions)
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
#' parse_drug_transporters_actions()
#' parse_drug_transporters_actions(FALSE)
#' parse_drug_transporters_actions(save_table = FALSE)
#' @export
parse_drug_transporters_actions <- function(save_table = TRUE) {
    drug_transporters_actions <- map_df(children, ~get_transporters_actions_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_transporters_actions,
                    table_name = "drug_transporters_actions",
                    save_table_only = TRUE)
    }
    return(drug_transporters_actions)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes articles node attributs date frame
#'
#' @examples
#' parse_drug_enzymes_articles()
#' parse_drug_enzymes_articles(FALSE)
#' parse_drug_enzymes_articles(save_table = FALSE)
#' @export
parse_drug_enzymes_articles <- function(save_table = TRUE) {
    drug_enzymes_articles <- map_df(children, ~get_enzymes_articles_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_enzymes_articles,
                    table_name = "drug_enzymes_articles",
                    save_table_only = TRUE)
    }
    return(drug_enzymes_articles)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets articles node attributs date frame
#'
#' @examples
#' parse_drug_targets_articles()
#' parse_drug_targets_articles(FALSE)
#' parse_drug_targets_articles(save_table = FALSE)
#' @export
parse_drug_targets_articles <- function(save_table = TRUE) {
    drug_targets_articles <- map_df(children, ~get_targets_articles_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_targets_articles,
                    table_name = "drug_targets_articles",
                    save_table_only = TRUE)
    }
    return(drug_targets_articles)
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
#' parse_drug_carriers_articles(FALSE)
#' parse_drug_carriers_articles(save_table = FALSE)
#' @export
parse_drug_carriers_articles <- function(save_table = TRUE) {
    drug_carriers_articles <- map_df(children, ~get_carriers_articles_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_carriers_articles,
                    table_name = "drug_carriers_articles",
                    save_table_only = TRUE)
    }
    return(drug_carriers_articles)
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
#' parse_drug_transporters_articles()
#' parse_drug_transporters_articles(FALSE)
#' parse_drug_transporters_articles(save_table = FALSE)
#' @export
parse_drug_transporters_articles <- function(save_table = TRUE) {
    drug_transporters_articles <- map_df(children, ~get_transporters_articles_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_transporters_articles,
                    table_name = "drug_transporters_articles",
                    save_table_only = TRUE)
    }
    return(drug_transporters_articles)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets textbooks node attributs date frame
#'
#' @examples
#' parse_drug_targets_textbooks()
#' parse_drug_targets_textbooks(FALSE)
#' parse_drug_targets_textbooks(save_table = FALSE)
#' @export
parse_drug_targets_textbooks <- function(save_table = TRUE) {
    drug_targets_textbooks <- map_df(children, ~get_targets_textbooks_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_targets_textbooks,
                    table_name = "drug_carriers_textbooks",
                    save_table_only = TRUE)
    }
    return(drug_targets_textbooks)
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
#' parse_drug_carriers_textbooks(FALSE)
#' parse_drug_carriers_textbooks(save_table = FALSE)
#' @export
parse_drug_carriers_textbooks <- function(save_table = TRUE) {
    drug_carriers_textbooks <- map_df(children, ~get_carriers_textbooks_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_carriers_textbooks,
                    table_name = "drug_carriers_textbooks",
                    save_table_only = TRUE)
    }
    return(drug_carriers_textbooks)
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
#' parse_drug_transporters_textbooks()
#' parse_drug_transporters_textbooks(FALSE)
#' parse_drug_transporters_textbooks(save_table = FALSE)
#' @export
parse_drug_transporters_textbooks <- function(save_table = TRUE) {
    drug_transporters_textbooks <- map_df(children, ~get_transporters_textbooks_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_transporters_textbooks,
                    table_name = "drug_transporters_textbooks", save_table_only = TRUE)
    }
    return(drug_transporters_textbooks)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes textbooks node attributs date frame
#'
#' @examples
#' parse_drug_enzymes_textbooks()
#' parse_drug_enzymes_textbooks(FALSE)
#' parse_drug_enzymes_textbooks(save_table = FALSE)
#' @export
parse_drug_enzymes_textbooks <- function(save_table = TRUE) {
    drug_enzymes_textbooks <- map_df(children, ~get_enzymes_textbooks_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_enzymes_textbooks,
                    table_name = "drug_enzymes_textbooks", save_table_only = TRUE)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes links node attributs date frame
#'
#' @examples
#' parse_drug_enzymes_links()
#' parse_drug_enzymes_links(FALSE)
#' parse_drug_enzymes_links(save_table = FALSE)
#' @export
parse_drug_enzymes_links <- function(save_table = TRUE) {
    drug_enzymes_links <- map_df(children, ~get_enzymes_links_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_enzymes_links,
                    table_name = "drug_enzymes_links", save_table_only = TRUE)
    }
    return(drug_enzymes_links)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets_links node attributs date frame
#'
#' @examples
#' parse_drug_targets_links()
#' parse_drug_targets_links(FALSE)
#' parse_drug_targets_links(save_table = FALSE)
#' @export
parse_drug_targets_links <- function(save_table = TRUE) {
    drug_targets_links <- map_df(children, ~get_targets_links_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_targets_links,
                    table_name = "drug_targets_links",
                    save_table_only = TRUE,
                    field.types = list(
                      title = paste("varchar(",
                                    max(nchar(drug_targets_links$title)) + 100, ")", sep = ""),
                      url = paste("varchar(", max(nchar(drug_targets_links$url)) + 100, ")", sep = "")))
    }
    return(drug_targets_links)
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
#' parse_drug_carriers_links(FALSE)
#' parse_drug_carriers_links(save_table = FALSE)
#' @export
parse_drug_carriers_links <- function(save_table = TRUE) {
    drug_carriers_links <- map_df(children, ~get_carriers_links_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_carriers_links,
                    table_name = "drug_carriers_links",
                    save_table_only = TRUE)
    }
    return(drug_carriers_links)
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
#' parse_drug_transporters_links()
#' parse_drug_transporters_links(FALSE)
#' parse_drug_transporters_links(save_table = FALSE)
#' @export
parse_drug_transporters_links <- function(save_table = TRUE) {
    drug_transporters_links <- map_df(children, ~get_transporters_links_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_transporters_links,
                    table_name = "drug_transporters_links",
                    save_table_only = TRUE)
    }
    return(drug_transporters_links)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes polypeptides node attributs date frame
#'
#' @examples
#' parse_drug_enzymes_polypeptides()
#' parse_drug_enzymes_polypeptides(FALSE)
#' parse_drug_enzymes_polypeptides(save_table = FALSE)
#' @export
parse_drug_enzymes_polypeptides <- function(save_table = TRUE) {
    drug_enzymes_polypeptides <- map_df(children, ~get_enzymes_polypeptide_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_enzymes_polypeptides,
                    table_name = "drug_enzymes_polypeptides",
                    save_table_only = TRUE,
                    field.types = list(
                      general_function = paste("varchar(",
                                               max(nchar(drug_enzymes_polypeptides$general_function)),
                                               ")", sep = ""),
                      specific_function = paste("varchar(",
                                                max(nchar(drug_enzymes_polypeptides$specific_function)),
                                                ")", sep = ""),
                      amino_acid_sequence = paste("varchar(",
                                                  max(nchar(drug_enzymes_polypeptides$amino_acid_sequence)),
                                                  ")", sep = ""),
                      gene_sequence = paste("varchar(",
                                            max(nchar(drug_enzymes_polypeptides$gene_sequence)),
                                            ")", sep = "")))
    }
    return(drug_enzymes_polypeptides)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets polypeptides node attributs date frame
#'
#' @examples
#' parse_drug_targets_polypeptides()
#' parse_drug_targets_polypeptides(FALSE)
#' parse_drug_targets_polypeptides(save_table = FALSE)
#' @export
parse_drug_targets_polypeptides <- function(save_table = TRUE) {
    drug_targets_polypeptides <- map_df(children, ~get_targets_polypeptide_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_targets_polypeptides,
                    table_name = "drug_targets_polypeptides",
                    save_table_only = TRUE,
                    field.types = list(
                      general_function = paste("varchar(",
                                               max(nchar(drug_targets_polypeptides$general_function)),
                                               ")", sep = ""),
                      specific_function = paste("varchar(max)", sep = ""),
                      amino_acid_sequence = paste("varchar(max)", sep = ""),
                      gene_sequence = paste("varchar(max)", sep = "")))
    }
    return(drug_targets_polypeptides)
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
#' parse_drug_carriers_polypeptides(FALSE)
#' parse_drug_carriers_polypeptides(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides <- function(save_table = TRUE) {
    drug_carriers_polypeptides <- map_df(children, ~get_carriers_polypeptide_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_carriers_polypeptides,
                    table_name = "drug_carriers_polypeptides",
                    save_table_only = TRUE,
                    field.types = list(
                      general_function = paste("varchar(",
                                               max(nchar(drug_carriers_polypeptides$general_function)),
                                               ")", sep = ""),
                      specific_function = paste("varchar(",
                                                max(nchar(drug_carriers_polypeptides$specific_function)),
                                                ")", sep = ""),
                      amino_acid_sequence = paste("varchar(",
                                                  max(nchar(drug_carriers_polypeptides$amino_acid_sequence)),
                                                  ")", sep = ""),
                      gene_sequence = paste("varchar(max)", sep = "")))
    }
    return(drug_carriers_polypeptides)
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
#' parse_drug_transporters_polypeptides()
#' parse_drug_transporters_polypeptides(FALSE)
#' parse_drug_transporters_polypeptides(save_table = FALSE)
#' @export
parse_drug_transporters_polypeptides <- function(save_table = TRUE) {
    drug_transporters_polypeptides <- map_df(children, ~get_transporters_polypeptide_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_transporters_polypeptides,
                    table_name = "drug_transporters_polypeptides",
                    save_table_only = TRUE,
                    field.types = list(
                      general_function = paste("varchar(",
                                               max(nchar(drug_transporters_polypeptides$general_function)),
                                               ")", sep = ""),
                      specific_function = paste("varchar(",
                                                max(nchar(drug_transporters_polypeptides$specific_function)),
                                                ")", sep = ""),
                      amino_acid_sequence = paste("varchar(",
                                                  max(nchar(drug_transporters_polypeptides$amino_acid_sequence)),
                                                  ")", sep = ""),
                      gene_sequence = paste("varchar(max)", sep = "")))
    }
    return(drug_transporters_polypeptides)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes polypeptides external identifiers node
#'  attributs date frame
#'
#' @examples
#' parse_drug_enzymes_polypeptides_external_identifiers()
#' parse_drug_enzymes_polypeptides_external_identifiers(FALSE)
#' parse_drug_enzymes_polypeptides_external_identifiers(save_table = FALSE)
#' @export
parse_drug_enzymes_polypeptides_external_identifiers <- function(save_table = TRUE) {
    drug_enzymes_polypeptide_external_identifiers <-
      map_df(children, ~get_enzymes_polypeptide_external_identifiers_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_enzymes_polypeptide_external_identifiers,
                    table_name = "drug_enzymes_polypeptides_external_identifiers",
                    save_table_only = TRUE)
    }
    return(drug_enzymes_polypeptide_external_identifiers)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets polypeptides external identifiers node attributs date frame
#'
#' @examples
#' parse_drug_targets_polypeptides_external_identifiers()
#' parse_drug_targets_polypeptides_external_identifiers(FALSE)
#' parse_drug_targets_polypeptides_external_identifiers(save_table = FALSE)
#' @export
parse_drug_targets_polypeptides_external_identifiers <- function(save_table = TRUE) {
    drug_targets_polypeptide_external_identifiers <-
      map_df(children, ~get_targets_polypeptide_external_identifiers_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_targets_polypeptide_external_identifiers,
                    table_name = "drug_targets_polypeptides_external_identifiers",
                    save_table_only = TRUE)
    }
    return(drug_targets_polypeptide_external_identifiers)
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
#' parse_drug_carriers_polypeptides_external_identifiers(FALSE)
#' parse_drug_carriers_polypeptides_external_identifiers(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides_external_identifiers <- function(save_table = TRUE) {
    drug_carriers_polypeptide_external_identifiers <-
      map_df(children, ~get_carriers_polypeptide_external_identifiers_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_carriers_polypeptide_external_identifiers,
                    table_name = "drug_carriers_polypeptides_external_identifiers",
                    save_table_only = TRUE)
    }
    return(drug_carriers_polypeptide_external_identifiers)
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
#' parse_drug_transporters_polypeptides_external_identifiers()
#' parse_drug_transporters_polypeptides_external_identifiers(FALSE)
#' parse_drug_transporters_polypeptides_external_identifiers(save_table = FALSE)
#' @export
parse_drug_transporters_polypeptides_external_identifiers <- function(save_table = TRUE) {
    drug_transporters_polypeptide_external_identifiers <-
      map_df(children, ~get_transporters_polypeptide_external_identifiers_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_transporters_polypeptide_external_identifiers,
                    table_name = "drug_transporters_polypeptides_external_identifiers",
                    save_table_only = TRUE)
    }
    return(drug_transporters_polypeptide_external_identifiers)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes polypeptides synonyms node attributs date frame
#'
#' @examples
#' parse_drug_enzymes_polypeptides_synonyms()
#' parse_drug_enzymes_polypeptides_synonyms(FALSE)
#' parse_drug_enzymes_polypeptides_synonyms(save_table = FALSE)
#' @export
parse_drug_enzymes_polypeptides_synonyms <- function(save_table = TRUE) {
    drug_enzymes_polypeptide_synonyms <- map_df(children,
                                                ~get_enzymes_polypeptide_synonyms_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_enzymes_polypeptide_synonyms,
                    table_name = "drug_enzymes_polypeptides_synonyms",
                    save_table_only = TRUE)
    }
    return(drug_enzymes_polypeptide_synonyms)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets polypeptides synonyms node attributs date frame
#'
#' @examples
#' parse_drug_targets_polypeptides_synonyms()
#' parse_drug_targets_polypeptides_synonyms(FALSE)
#' parse_drug_targets_polypeptides_synonyms(save_table = FALSE)
#' @export
parse_drug_targets_polypeptides_synonyms <- function(save_table = TRUE) {
    drug_targets_polypeptide_synonyms <- map_df(children,
                                                ~get_targets_polypeptide_synonyms_df(.x))
    if (save_table) {
      save_drug_sub(con = con,
                    df = drug_targets_polypeptide_synonyms,
                    table_name = "drug_targets_polypeptides_synonyms",
                    save_table_only = TRUE)
    }
    return(drug_targets_polypeptide_synonyms)
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
#' parse_drug_carriers_polypeptides_synonyms(FALSE)
#' parse_drug_carriers_polypeptides_synonyms(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides_synonyms <- function(save_table = TRUE) {
    drug_carriers_polypeptide_synonyms <- map_df(children,
                                                 ~get_carriers_polypeptide_synonyms_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_carriers_polypeptide_synonyms,
                    table_name = "drug_carriers_polypeptides_synonyms",
                    save_table_only = TRUE)
    }
    return(drug_carriers_polypeptide_synonyms)
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
#' parse_drug_transporters_polypeptides_synonyms()
#' parse_drug_transporters_polypeptides_synonyms(FALSE)
#' parse_drug_transporters_polypeptides_synonyms(save_table = FALSE)
#' @export
parse_drug_transporters_polypeptides_synonyms <- function(save_table = TRUE) {
    drug_transporter_polypeptide_synonyms <- map_df(children,
                                                    ~get_transporters_polypeptide_synonyms_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_transporter_polypeptide_synonyms,
                    table_name = "drug_transporters_polypeptides_synonyms",
                    save_table_only = TRUE)
    }
    return(drug_transporter_polypeptide_synonyms)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug groups node attributs date frame
#'
#' @examples
#' parse_drug_enzymes_polypeptides_pfams()
#' parse_drug_enzymes_polypeptides_pfams(FALSE)
#' parse_drug_enzymes_polypeptides_pfams(save_table = FALSE)
#' @export
parse_drug_enzymes_polypeptides_pfams <- function(save_table = TRUE) {
    drug_enzymes_polypeptide_pfams <- map_df(children,
                                             ~get_enzymes_polypeptide_pfams_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_enzymes_polypeptide_pfams,
                    table_name = "drug_enzymes_polypeptides_pfams",
                    save_table_only = TRUE)
    }
    return(drug_enzymes_polypeptide_pfams)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets polypeptides pfams node attributs date frame
#'
#' @examples
#' parse_drug_targets_polypeptides_pfams()
#' parse_drug_targets_polypeptides_pfams(FALSE)
#' parse_drug_targets_polypeptides_pfams(save_table = FALSE)
#' @export
parse_drug_targets_polypeptides_pfams <- function(save_table = TRUE) {
    drug_targets_polypeptide_pfams <- map_df(children,
                                             ~get_targets_polypeptide_pfams_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_targets_polypeptide_pfams,
                    table_name = "drug_targets_polypeptides_pfams",
                    save_table_only = TRUE)
    }
    return(drug_targets_polypeptide_pfams)
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
#' parse_drug_carriers_polypeptides_pfams(FALSE)
#' parse_drug_carriers_polypeptides_pfams(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides_pfams <- function(save_table = TRUE) {
    drug_carriers_polypeptide_pfams <- map_df(children,
                                              ~get_carriers_polypeptide_pfams_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_carriers_polypeptide_pfams,
                    table_name = "drug_carriers_polypeptide_pfams",
                    save_table_only = TRUE)
    }
    return(drug_carriers_polypeptide_pfams)
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
#' parse_drug_transporters_polypeptides_pfams()
#' parse_drug_transporters_polypeptides_pfams(FALSE)
#' parse_drug_transporters_polypeptides_pfams(save_table = FALSE)
#' @export
parse_drug_transporters_polypeptides_pfams <- function(save_table = TRUE) {
    drug_transporters_polypeptides_pfams <- map_df(children,
                                                   ~get_transporters_polypeptide_pfams_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_transporters_polypeptides_pfams,
                    table_name = "drug_transporters_polypeptides_pfams",
                    save_table_only = TRUE)
    }
    return(drug_transporters_polypeptides_pfams)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug enzymes polypeptides go classifiers node attributs date frame
#'
#' @examples
#' parse_drug_enzymes_polypeptides_go_classifiers()
#' parse_drug_enzymes_polypeptides_go_classifiers(FALSE)
#' parse_drug_enzymes_polypeptides_go_classifiers(save_table = FALSE)
#' @export
parse_drug_enzymes_polypeptides_go_classifiers <- function(save_table = TRUE) {
    drug_enzymes_polypeptides_go_classifiers <- map_df(children,
                                                       ~get_enzymes_polypeptide_go_classifiers_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_enzymes_polypeptides_go_classifiers,
                    table_name = "drug_enzymes_polypeptides_go_classifiers",
                    save_table_only = TRUE)
    }
    return(drug_enzymes_polypeptides_go_classifiers)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug targets polypeptides go classifiers node attributs date frame
#'
#' @examples
#' parse_drug_targets_polypeptides_go_classifiers()
#' parse_drug_targets_polypeptides_go_classifiers(FALSE)
#' parse_drug_targets_polypeptides_go_classifiers(save_table = FALSE)
#' @export
parse_drug_targets_polypeptides_go_classifiers <- function(save_table = TRUE) {
    drug_targets_polypeptides_go_classifiers <- map_df(children,
                                                       ~get_targets_polypeptide_go_classifiers_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_targets_polypeptides_go_classifiers,
                    table_name = "drug_targets_polypeptides_go_classifiers",
                    save_table_only = TRUE)
    }
    return(drug_targets_polypeptides_go_classifiers)
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
#' parse_drug_carriers_polypeptides_go_classifiers(FALSE)
#' parse_drug_carriers_polypeptides_go_classifiers(save_table = FALSE)
#' @export
parse_drug_carriers_polypeptides_go_classifiers <- function(save_table = TRUE) {
    drug_carriers_polypeptides_go_classifiers <- map_df(children,
                                                        ~get_carriers_polypeptide_go_classifiers_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_carriers_polypeptides_go_classifiers,
                    table_name = "drug_carriers_polypeptides_go_classifiers",
                    save_table_only = TRUE)
    }
    return(drug_carriers_polypeptides_go_classifiers)
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
#' parse_drug_transporters_polypeptides_go_classifiers()
#' parse_drug_transporters_polypeptides_go_classifiers(FALSE)
#' parse_drug_transporters_polypeptides_go_classifiers(save_table = FALSE)
#' @export
parse_drug_transporters_polypeptides_go_classifiers <- function(save_table = TRUE) {
    drug_transporters_polypeptides_go_classifiers <- map_df(children,
                                                            ~get_transporters_polypeptide_go_classifiers_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_transporters_polypeptides_go_classifiers,
                    table_name = "drug_transporters_polypeptides_go_classifiers",
                    save_table_only = TRUE)
    }
    return(drug_transporters_polypeptides_go_classifiers)
}

#' Extracts the drug reactions element and return data as data frame.
#'
#' \code{parse_drug_reactions} returns data frame of drug reactions elements.
#'
#' This functions extracts the groups element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug reactions node attributs date frame
#'
#' @examples
#' parse_drug_reactions()
#' parse_drug_reactions(FALSE)
#' parse_drug_reactions(save_table = FALSE)
#' @export
parse_drug_reactions <- function(save_table = TRUE) {
    drug_reactions <- map_df(children, ~get_reactions_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_reactions,
                    table_name = "drug_reactions", foreign_key = "drug_key")
    }
    return(drug_reactions)
}

#' Extracts the drug reactions enzymes element and return data as data frame.
#'
#' \code{parse_drug_reactions_enzymes} returns data frame of drug reactions enzymes elements.
#'
#' This functions extracts the reactions enzymes element of drug node in drug bank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned dataframe in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug reactions enzymes node attributs date frame
#'
#' @examples
#' parse_drug_reactions_enzymes()
#' parse_drug_reactions_enzymes(FALSE)
#' parse_drug_reactions_enzymes(save_table = FALSE)
#' @export
parse_drug_reactions_enzymes <- function(save_table = TRUE) {
    drug_reactions_enzymes <- map_df(children, ~get_reactions_enzymes_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_reactions_enzymes,
                    table_name = "drug_reactions_enzymes", save_table_only = TRUE)
    }
    return(drug_reactions_enzymes)
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
#' parse_drug_carriers(FALSE)
#' parse_drug_carriers(save_table = FALSE)
#' @export
parse_drug_carriers <- function(save_table = TRUE) {
    drug_carriers <- map_df(children, ~get_carriers_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_carriers,
                    table_name = "drug_carriers", foreign_key = "drug_key")
    }
    return(drug_carriers)
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
#' parse_drug_transporters()
#' parse_drug_transporters(FALSE)
#' parse_drug_transporters(save_table = FALSE)
#' @export
parse_drug_transporters <- function(save_table = TRUE) {
    drug_transporters <- map_df(children, ~get_transporters_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_transporters,
                    table_name = "drug_transporters", foreign_key = "drug_key")
    }
    return(drug_transporters)
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
#' no need to call it again before calling this funtion.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug target node attributs date frame
#'
#' @examples
#' parse_drug_targets()
#' parse_drug_targets(FALSE)
#' parse_drug_targets(save_table = FALSE)
#' @export
parse_drug_targets <- function(save_table = TRUE) {
    drug_targets <- map_df(children, ~get_targets_df(.x))
    if (save_table) {
      save_drug_sub(con = con, df = drug_targets, table_name = "drug_targets",
                    foreign_key = "drug_key")
    }
    return(drug_targets)
}
