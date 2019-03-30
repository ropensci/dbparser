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
#' @return drug main node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug()
#' parse_drug(TRUE)
#' parse_drug(save_table = FALSE)
#' }
#' @export
parse_drug <- function(save_table = FALSE) {
  # db connection
  drugs <- map_df(pkg.env$children, ~ drug_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drugs,
      table_name = "drug",
      primary_key = "primary_key",
      foreign_key = NULL,
      field.types = list(
        description = "varchar(6349)",
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
        updated = "date"
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
#' @return drug groups node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_groups()
#' parse_drug_groups(TRUE)
#' parse_drug_groups(save_table = FALSE)
#' }
#' @export
parse_drug_groups <- function(save_table = FALSE) {
  drug_groups <- map_df(pkg.env$children, ~ drug_sub_df(.x, "groups"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_groups,
                  table_name = "drug_groups")
  }
  return(tibble::as_tibble(drug_groups))
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
#' @return drug articles node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_articles()
#' parse_drug_articles(TRUE)
#' parse_drug_articles(save_table = FALSE)
#' }
#' @export
parse_drug_articles <- function(save_table = FALSE) {
  drug_articles <-
    map_df(
      pkg.env$children,
      ~ drug_sub_df(.x, "general-references",
                    seconadary_node = "articles")
    )
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_articles,
                  table_name = "drug_articles")
  }
  return(tibble::as_tibble(drug_articles))
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
#' @return drug books node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_books()
#' parse_drug_books(TRUE)
#' parse_drug_books(save_table = FALSE)
#' }
#' @export
parse_drug_books <- function(save_table = FALSE) {
  drug_books <-
    map_df(
      pkg.env$children,
      ~ drug_sub_df(.x, "general-references", seconadary_node = "textbooks")
    )
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_books,
                  table_name = "drug_books")
  }
  return(tibble::as_tibble(drug_books))
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
#' @return drug links node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_links()
#' parse_drug_links(TRUE)
#' parse_drug_links(save_table = FALSE)
#' }
#' @export
parse_drug_links <- function(save_table = FALSE) {
  drug_links <-
    map_df(
      pkg.env$children,
      ~ drug_sub_df(.x, "general-references", seconadary_node = "links")
    )
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_links,
                  table_name = "drug_links")
  }
  return(tibble::as_tibble(drug_links))
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
#' @return drug synonyms node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_synonyms()
#' parse_drug_synonyms(TRUE)
#' parse_drug_synonyms(save_table = FALSE)
#' }
#' @export
parse_drug_synonyms <- function(save_table = FALSE) {
  drug_synonyms <- map_df(pkg.env$children, ~ get_synonyms_df(.x))
  if (save_table) {
    save_drug_sub(
      con = pkg.env$con,
      df = drug_synonyms,
      table_name = "drug_synonyms",
      field.types = list(synonym = "varchar(534)")
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
#' @return drug products node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_products()
#' parse_drug_products(TRUE)
#' parse_drug_products(save_table = FALSE)
#' }
#' @export
parse_drug_products <- function(save_table = FALSE) {
  drug_products <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "products"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_products,
                  table_name = "drug_products")
  }
  return(tibble::as_tibble(drug_products))
}

#' Extracts the drug international brands and return data as tibble.
#'
#' \code{parse_drug_international_brands} returns tibble of drug products elements.
#'
#' This functions extracts the international brands element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug international brands node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_international_brands()
#' parse_drug_international_brands(TRUE)
#' parse_drug_international_brands(save_table = FALSE)
#' }
#' @export
parse_drug_international_brands <- function(save_table = FALSE) {
  drug_international_brands <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "international-brands"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_international_brands,
                  table_name = "international_brands")
  }
  return(tibble::as_tibble(drug_international_brands))
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
#' @return drug mixtures node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_mixtures()
#' parse_drug_mixtures(TRUE)
#' parse_drug_mixtures(save_table = FALSE)
#' }
#' @export
parse_drug_mixtures <- function(save_table = FALSE) {
  drug_mixtures <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "mixtures"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_mixtures,
                  table_name = "drug_mixtures")
  }
  return(tibble::as_tibble(drug_mixtures))
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
#' @return drug packagers node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_packagers()
#' parse_drug_packagers(TRUE)
#' parse_drug_packagers(save_table = FALSE)
#' }
#' @export
parse_drug_packagers <- function(save_table = FALSE) {
  drug_packagers <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "packagers"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_packagers,
                  table_name = "drug_packagers")
  }
  return(tibble::as_tibble(drug_packagers))
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
#' @return drug categories node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_categories()
#' parse_drug_categories(TRUE)
#' parse_drug_categories(save_table = FALSE)
#' }
#' @export
parse_drug_categories <- function(save_table = FALSE) {
  drug_categories <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "categories"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_categories,
                  table_name = "drug_categories")
  }
  return(tibble::as_tibble(drug_categories))
}

#' Extracts the drug affected organisms element and return data as tibble.
#'
#' \code{parse_drug_affected_organisms} returns tibble of drug affected organisms elements.
#'
#' This functions extracts the affected organisms element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug affected organisms node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_affected_organisms()
#' parse_drug_affected_organisms(TRUE)
#' parse_drug_affected_organisms(save_table = FALSE)
#' }
#' @export
parse_drug_affected_organisms <- function(save_table = FALSE) {
  drug_affected_organisms <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "affected-organisms"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_affected_organisms,
                  table_name = "drug_affected_organisms")
  }
  return(tibble::as_tibble(drug_affected_organisms))
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
#' @return drug dosages node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_dosages()
#' parse_drug_dosages(TRUE)
#' parse_drug_dosages(save_table = FALSE)
#' }
#' @export
parse_drug_dosages <- function(save_table = FALSE) {
  drug_dosages <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "dosages"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_dosages,
                  table_name = "drug_dosages")
  }
  return(tibble::as_tibble(drug_dosages))
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
#' @return drug ahfs codes node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_ahfs_codes()
#' parse_drug_ahfs_codes(TRUE)
#' parse_drug_ahfs_codes(save_table = FALSE)
#' }
#' @export
parse_drug_ahfs_codes <- function(save_table = FALSE) {
  drug_ahfs_codes <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "ahfs-codes"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_ahfs_codes,
                  table_name = "drug_ahfs_codes")
  }
  return(tibble::as_tibble(drug_ahfs_codes))
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
#' @return drug pdb entries node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_pdb_entries()
#' parse_drug_pdb_entries(TRUE)
#' parse_drug_pdb_entries(save_table = FALSE)
#' }
#' @export
parse_drug_pdb_entries <- function(save_table = FALSE) {
  drug_pdb_entries <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "pdb-entries"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_pdb_entries,
                  table_name = "drug_pdb_entries")
  }
  return(tibble::as_tibble(drug_pdb_entries))
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
#' @return drug patents node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_patents()
#' parse_drug_patents(TRUE)
#' parse_drug_patents(save_table = FALSE)
#' }
#' @export
parse_drug_patents <- function(save_table = FALSE) {
  drug_patents <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "patents"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_patents,
                  table_name = "drug_patents")
  }
  return(tibble::as_tibble(drug_patents))
}

#' Extracts the drug food interactions element and return data as tibble.
#'
#' \code{parse_drug_food_interactions} returns tibble of drug food
#' interactions elements.
#'
#' This functions extracts the food interactions element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug food interactions node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_food_interactions()
#' parse_drug_food_interactions(TRUE)
#' parse_drug_food_interactions(save_table = FALSE)
#' }
#' @export
parse_drug_food_interactions <- function(save_table = FALSE) {
  drug_food_interactions <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "food-interactions"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_food_interactions,
                  table_name = "drug_food_interactions")
  }
  return(tibble::as_tibble(drug_food_interactions))
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
#' @return drug interactions node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_interactions()
#' parse_drug_interactions(TRUE)
#' parse_drug_interactions(save_table = FALSE)
#' }
#' @export
parse_drug_interactions <- function(save_table = FALSE) {
  drug_drug_interactions <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "drug-interactions"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_drug_interactions,
                  table_name = "drug_drug_interactions")
  }
  return(tibble::as_tibble(drug_drug_interactions))
}

#' Extracts the drug experimental properties element and return data as tibble.
#'
#' \code{parse_drug_experimental_properties} returns tibble of drug experimental
#'  properties elements.
#'
#' This functions extracts the experimental properties element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug experimental properties node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_experimental_properties()
#' parse_drug_experimental_properties(TRUE)
#' parse_drug_experimental_properties(save_table = FALSE)
#' }
#' @export
parse_drug_experimental_properties <- function(save_table = FALSE) {
  drug_experimental_properties <-
    map_df(pkg.env$children,
           ~ drug_sub_df(.x, "experimental-properties"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_experimental_properties,
                  table_name = "drug_experimental_properties")
  }
  return(tibble::as_tibble(drug_experimental_properties))
}

#' Extracts the drug external identifiers element and return data as tibble.
#'
#' \code{parse_drug_external_identifiers} returns tibble of external
#' identifiers groups elements.
#'
#' This functions extracts the external identifiers element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' \code{\link{open_db}} method. It takes one single optional argument to
#' save the returned tibble in the database.
#' It must be called after \code{\link{get_xml_db_rows}} function like
#' any other parser function.
#' If \code{\link{get_xml_db_rows}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @return drug external identifiers node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_external_identifiers()
#' parse_drug_external_identifiers(TRUE)
#' parse_drug_external_identifiers(save_table = FALSE)
#' }
#' @export
parse_drug_external_identifiers <- function(save_table = FALSE) {
  drug_external_identifiers <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "external-identifiers"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_external_identifiers,
                  table_name = "drug_external_identifiers")
  }
  return(drug_external_identifiers)
}

#' Extracts the drug external links element and return data as tibble.
#'
#' \code{parse_drug_external_links} returns tibble of drug external links elements.
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
#' @return drug external links node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_external_links()
#' parse_drug_external_links(TRUE)
#' parse_drug_external_links(save_table = FALSE)
#' }
#' @export
parse_drug_external_links <- function(save_table = FALSE) {
  drug_external_links <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "external-links"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_external_links,
                  table_name = "drug_external_links")
  }
  return(tibble::as_tibble(drug_external_links))
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
#' @return drug snp effects node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_snp_effects()
#' parse_drug_snp_effects(TRUE)
#' parse_drug_snp_effects(save_table = FALSE)
#' }
#' @export
parse_drug_snp_effects <- function(save_table = FALSE) {
  drug_snp_effects <-
    map_df(pkg.env$children, ~ drug_sub_df(.x, "snp-effects"))
  if (save_table) {
    save_drug_sub(con = pkg.env$con,
                  df = drug_snp_effects,
                  table_name = "drug_snp_effects")
  }
  return(tibble::as_tibble(drug_snp_effects))
}

#' Extracts the drug snp adverse drug reactions element and return data as tibble.
#'
#' \code{parse_drug_snp_adverse_drug_reactions} returns tibble of drug
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
#' @return drug snp adverse drug reactions node attributes tibble
#'
#' @examples
#' \donttest{
#' parse_drug_snp_adverse_drug_reactions()
#' parse_drug_snp_adverse_drug_reactions(TRUE)
#' parse_drug_snp_adverse_drug_reactions(save_table = FALSE)
#' }
#' @export
parse_drug_snp_adverse_drug_reactions <-
  function(save_table = FALSE) {
    drug_snp_adverse_drug_reactions <-
      map_df(pkg.env$children,
             ~ drug_sub_df(.x, "snp-adverse-drug-reactions"))
    if (save_table) {
      save_drug_sub(con = pkg.env$con,
                    df = drug_snp_adverse_drug_reactions,
                    table_name = "drug_snp_adverse_drug_reactions")
    }
    return(tibble::as_tibble(drug_snp_adverse_drug_reactions))
  }
