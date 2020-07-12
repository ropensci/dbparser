DrugElementsParser <- R6::R6Class(
  "DrugElementsParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      parsed_tbl <- map_df(drugs, ~ drug_sub_df(.x, private$main_node,
                                                progress = pb)) %>%
        unique()
      if (private$main_node == "groups" & nrow(parsed_tbl) > 0) {
        names(parsed_tbl) <- c("group", "drugbank-id")
      }
      return(parsed_tbl)
    }
  )
)

#' Drug Groups parser
#'
#' Groups that this drug belongs to. May include any of: approved, vet_approved,
#'  nutraceutical, illicit, withdrawn, investigational, and experimental.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{group}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_groups <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_groups",
      main_node = "groups"
    )$parse()
  }

#' Drug Products parser
#'
#' A list of commercially available products in Canada and the United States
#'  that contain the drug.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 32 variables:
#' \describe{
#'  \item{name}{The proprietary name(s) provided by the manufacturer for any
#'  commercially available products containing this drug.}
#'  \item{labeller}{The corporation responsible for labelling this product.}
#'  \item{ndc-id}{The National Drug Code (NDC) identifier of the drug}
#'  \item{ndc-product-code}{The National Drug Code (NDC) product code from the
#'   FDA National Drug Code directory.}
#'  \item{dpd-id}{Drug Product Database (DPD) identification number (a.k.a. DIN)
#'   from the Canadian Drug Product Database. Only present for drugs that are
#'   marketed in Canada}
#'  \item{ema-product-code}{EMA product code from the European Medicines Agency
#'  Database. Only present for products that are authorised by central procedure
#'   for marketing in the European Union.}
#'  \item{ema-ma-number}{EMA marketing authorisation number from the European
#'  Medicines Agency Database. Only present for products that are authorised by
#'   central procedure for marketing in the European Union.}
#'  \item{started-marketing-on}{The starting date for market approval.}
#'  \item{ended-marketing-on}{The ending date for market approval.}
#'  \item{dosage-form	}{The pharmaceutical formulation by which the drug is
#'  introduced into the body.}
#'  \item{strength}{The amount of active drug ingredient provided in the dosage}
#'  \item{route}{The path by which the drug or product is taken into the body}
#'  \item{fda-application-number}{The New Drug Application [NDA] number
#'  assigned to this drug by the FDA.}
#'  \item{over-the-counter}{A list of Over The Counter (OTC) forms of the drug.}
#'  \item{generic}{Whether this product is a generic drug.}
#'  \item{approved}{Indicates whether this drug has been approved by the
#'  regulating government.}
#'  \item{country}{The country where this commercially available drug has been
#'  approved.}
#'  \item{source}{Source of this product information. For example, a value of
#'  DPD indicates this information was retrieved from the Canadian Drug Product
#'   Database.}
#'  \item{standing}{One of good, discordant, or deprecated. Distinguishes
#'  products with up to date ingredient information (good) from products with
#'  conflicting information (discordant) or products that have been removed from
#'   an active label (deprecated).}
#'  \item{standing-updated-on}{The date on which the standing was last updated}
#'  \item{standing-reason}{Explains the non-good standing of the product.
#'  One of: ingredient_change, code_duplication, invalid, or removed.}
#'  \item{jurisdiction-marketing-category	}{The marketing category of this
#'  product in its jurisdiction}
#'  \item{branded}{Whether this product has a named brand}
#'  \item{prescription}{Whether this product is only available with
#'  a prescription}
#'  \item{unapproved}{Whether this product is not approved in its jurisdiction}
#'  \item{vaccine}{Whether this product is a vaccine}
#'  \item{allergenic}{Whether this product is used in allergenic testing}
#'  \item{cosmetic}{Whether this product is a cosmetic, such as sunscreen}
#'  \item{kit}{Whether this product is a kit composed of multiple distinct
#'  parts}
#'  \item{solo}{Whether this product has only a single active ingredient}
#'  \item{available}{Whether this product can be sold in its jurisdiction}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_products <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_products",
      main_node = "products"
    )$parse()
  }

#' Drug Calculated Properties parser
#'
#' Drug properties that have been predicted by ChemAxon or ALOGPS based on the
#' inputed chemical structure. Associated links below will redirect to
#' descriptions of the specific term.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'  \item{kind}{Name of the property.}
#'  \item{value}{Predicted physicochemical properties; obtained by the use of
#'  prediction software such as ALGOPS and ChemAxon.}
#'  \item{source}{Name of the software used to calculate this property,
#'  either ChemAxon or ALOGPS.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_calc_prop <- function(save_table = FALSE,
                           save_csv = FALSE,
                           csv_path = ".",
                           override_csv = FALSE,
                           database_connection = NULL) {
  DrugElementsParser$new(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "drug_calculated_properties",
    main_node = "calculated-properties"
  )$parse()
}

#' Extracts the drug international brands and return data as tibble.
#'
#' \code{drug_intern_brand} returns tibble of drug products
#' elements.
#'
#' This functions extracts the international brands element of drug node in
#' drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in
#'  the new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug international brands node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_intern_brand()
#'
#' # will throw an error, as database_connection is NULL
#' drug_intern_brand(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_intern_brand(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_intern_brand(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' #  current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_intern_brand(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_intern_brand(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_intern_brand(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_intern_brand <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_international_brands", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_international_brands <- readr::read_csv(path)
    } else {
      drug_international_brands <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "international-brands")) %>%
        unique()
      if (nrow(drug_international_brands) > 0) {
        colnames(drug_international_brands) <- c("brand", "company",
                                                 "drugbank-id")
      }
      write_csv(drug_international_brands, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_international_brands,
                    table_name = "international_brands")
    }
    return(drug_international_brands %>% as_tibble())
  }

#' Extracts the drug salts and return data as tibble.
#'
#' \code{drug_salts} returns tibble of drug products elements.
#'
#' This functions extracts the salts element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in t
#' he new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug salts node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_salts()
#'
#' # will throw an error, as database_connection is NULL
#' drug_salts(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_salts(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_salts(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_salts(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_salts(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_salts(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_salts <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
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
      save_drug_sub(con = database_connection,
                    df = drug_salts,
                    table_name = "salts")
    }
    return(drug_salts %>% as_tibble())
  }

#' Extracts the drug mixtures element and return data as tibble.
#'
#' \code{drug_mixtures} returns tibble of drug mixtures elements.
#'
#' This functions extracts the mixtures element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug mixtures node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_mixtures()
#'
#' # will throw an error, as database_connection is NULL
#' drug_mixtures(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_mixtures(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_mixtures(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_mixtures(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_mixtures(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_mixtures(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_mixtures <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_mixtures", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_mixtures <- readr::read_csv(path)
    } else {
      drug_mixtures <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "mixtures")) %>% unique()

      write_csv(drug_mixtures, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_mixtures,
                    table_name = "drug_mixtures")
    }
    return(drug_mixtures %>% as_tibble())
  }

#' Extracts the drug packagers element and return data as tibble.
#'
#' \code{drug_packagers} returns tibble of drug packagers elements.
#'
#' This functions extracts the packagers element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug packagers node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_packagers()
#'
#' # will throw an error, as database_connection is NULL
#' drug_packagers(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_packagers(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_packagers(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not
#' # exist in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_packagers(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_packagers(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_packagers(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_packagers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_packagers", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_packagers <- readr::read_csv(path)
    } else {
      drug_packagers <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "packagers")) %>% unique()
      write_csv(drug_packagers, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_packagers,
                    table_name = "drug_packagers")
    }
    return(drug_packagers %>% as_tibble())
  }


#' Extracts the drug categories element and return data as tibble.
#'
#' \code{drug_categories} returns tibble of drug categories elements.
#'
#' This functions extracts the categories element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug categories node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_categories()
#'
#' # will throw an error, as database_connection is NULL
#' drug_categories(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_categories(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_categories(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' #  in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_categories(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_categories(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_categories(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_categories <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_categories", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_categories <- readr::read_csv(path)
    } else {
      drug_categories <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "categories")) %>% unique()
      write_csv(drug_categories, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_categories,
                    table_name = "drug_categories")
    }
    return(drug_categories %>% as_tibble())
  }

#' Extracts the drug affected organisms element and return data as tibble.
#'
#' \code{drug_affected_organisms} returns tibble of drug affected
#' organisms elements.
#'
#' This functions extracts the affected organisms element of drug node in
#' drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug affected organisms node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_affected_organisms()
#'
#' # will throw an error, as database_connection is NULL
#' drug_affected_organisms(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_affected_organisms(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_affected_organisms(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_affected_organisms(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_affected_organisms(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_affected_organisms(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_affected_organisms <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_affected_organisms", csv_path)
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
      save_drug_sub(con = database_connection,
                    df = drug_affected_organisms,
                    table_name = "drug_affected_organisms")
    }
    return(drug_affected_organisms %>% as_tibble())
  }

#' Extracts the drug dosages element and return data as tibble.
#'
#' \code{drug_dosages} returns tibble of drug dosages elements.
#'
#' This functions extracts the dosages element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug dosages node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_dosages()
#'
#' # will throw an error, as database_connection is NULL
#' drug_dosages(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_dosages(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_dosages(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_dosages(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' #  and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_dosages(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_dosages(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_dosages <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_dosages", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_dosages <- readr::read_csv(path)
    } else {
      drug_dosages <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "dosages")) %>% unique()

      write_csv(drug_dosages, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_dosages,
                    table_name = "drug_dosages")
    }
    return(drug_dosages %>% as_tibble())
  }


#' Extracts the drug ahfs codes element and return data as tibble.
#'
#' \code{drug_ahfs_codes} returns tibble of drug ahfs codes elements.
#'
#' This functions extracts the ahfs codes element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug ahfs codes node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_ahfs_codes()
#'
#' # will throw an error, as database_connection is NULL
#' drug_ahfs_codes(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_ahfs_codes(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ahfs_codes(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ahfs_codes(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ahfs_codes(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_ahfs_codes(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_ahfs_codes <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
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
      save_drug_sub(con = database_connection,
                    df = drug_ahfs_codes,
                    table_name = "drug_ahfs_codes")
    }
    return(drug_ahfs_codes %>% as_tibble())
  }

#' Extracts the drug pdb entries element and return data as tibble.
#'
#' \code{drug_pdb_entries} returns tibble of drug pdb entries elements.
#'
#' This functions extracts the pdb entries element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#'  \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug pdb entries node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_pdb_entries()
#'
#' # will throw an error, as database_connection is NULL
#' drug_pdb_entries(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_pdb_entries(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pdb_entries(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pdb_entries(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pdb_entries(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_pdb_entries(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_pdb_entries <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
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
      save_drug_sub(con = database_connection,
                    df = drug_pdb_entries,
                    table_name = "drug_pdb_entries")
    }
    return(drug_pdb_entries %>% as_tibble())
  }

#' Extracts the drug patents element and return data as tibble.
#'
#' \code{drug_patents} returns tibble of drug patents elements.
#'
#' This functions extracts the patents element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#'  \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug patents node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_patents()
#'
#' # will throw an error, as database_connection is NULL
#' drug_patents(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_patents(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_patents(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # 'in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_patents(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_patents(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_patents(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_patents <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_patents", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_patents <- readr::read_csv(path)
    } else {
      drug_patents <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "patents")) %>% unique()

      write_csv(drug_patents, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_patents,
                    table_name = "drug_patents")
    }
    return(drug_patents %>% as_tibble())
  }

#' Extracts the drug food interactions element and return data as tibble.
#'
#' \code{drug_food_interactions} returns tibble of drug food
#' interactions elements.
#'
#' This functions extracts the food interactions element of drug node in
#' drugbank xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug food interactions node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_food_interactions()
#'
#' # will throw an error, as database_connection is NULL
#' drug_food_interactions(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_food_interactions(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_food_interactions(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_food_interactions(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_food_interactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' #  and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_food_interactions(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_food_interactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_food_interactions", csv_path)
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
      save_drug_sub(con = database_connection,
                    df = drug_food_interactions,
                    table_name = "drug_food_interactions")
    }
    return(drug_food_interactions %>% as_tibble())
  }

#' Extracts the drug interactions element and return data as tibble.
#'
#' \code{drug_interactions} returns tibble of drug interactions elements.
#'
#' This functions extracts the interactions element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table}
#' and \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug interactions node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_interactions()
#'
#' # will throw an error, as database_connection is NULL
#' drug_interactions(save_table = TRUE)
#'
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_interactions(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_interactions(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_interactions(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_interactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_interactions(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_interactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_drug_interactions", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_drug_interactions <- readr::read_csv(path)
    } else {
      drug_drug_interactions <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "drug-interactions")) %>%
        unique()

      write_csv(drug_drug_interactions, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_drug_interactions,
                    table_name = "drug_drug_interactions")
    }
    return(drug_drug_interactions %>% as_tibble())
  }

#' Extracts the drug experimental properties element and return data as tibble.
#'
#' \code{drug_exp_prop} returns tibble of drug
#'  experimental
#'  properties elements.
#'
#' This functions extracts the experimental properties element of drug node in
#'  drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug experimental properties node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_exp_prop()
#'
#' # will throw an error, as database_connection is NULL
#' drug_exp_prop(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_exp_prop(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_exp_prop(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_exp_prop(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_exp_prop(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_exp_prop(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_exp_prop <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
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
      save_drug_sub(con = database_connection,
                    df = drug_experimental_properties,
                    table_name = "drug_experimental_properties")
    }
    return(drug_experimental_properties %>% as_tibble())
  }

#' Extracts the drug external identifiers element and return data as tibble.
#'
#' \code{drug_ex_identity} returns tibble of external
#' identifiers groups elements.
#'
#' This functions extracts the external identifiers element of drug node in
#' drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug external identifiers node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_ex_identity()
#'
#' # will throw an error, as database_connection is NULL
#' drug_ex_identity(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_ex_identity(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_ex_identity(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_ex_identity <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_external_identifiers", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_external_identifiers <- readr::read_csv(path)
    } else {
      drug_external_identifiers <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "external-identifiers"))

      write_csv(drug_external_identifiers, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_external_identifiers,
                    table_name = "drug_external_identifiers")
    }
    return(drug_external_identifiers %>% as_tibble())
  }

#' Extracts the drug external links element and return data as tibble.
#'
#' \code{drug_external_links} returns tibble of drug external links
#' elements.
#'
#' This functions extracts the external links element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug external links node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_external_links()
#'
#' # will throw an error, as database_connection is NULL
#' drug_external_links(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_external_links(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_external_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_external_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
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
      save_drug_sub(con = database_connection,
                    df = drug_external_links,
                    table_name = "drug_external_links")
    }
    return(drug_external_links %>% as_tibble())
  }

#' Extracts the drug snp effects element and return data as tibble.
#'
#' \code{drug_snp_effects} returns tibble of snp effects groups elements.
#'
#' This functions extracts the snp effects element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug snp effects node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_snp_effects()
#'
#' # will throw an error, as database_connection is NULL
#' drug_snp_effects(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_snp_effects(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_snp_effects(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_snp_effects <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_snp_effects", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_snp_effects <- readr::read_csv(path)
    } else {
      drug_snp_effects <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "snp-effects")) %>% unique()

      write_csv(drug_snp_effects, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_snp_effects,
                    table_name = "drug_snp_effects")
    }
    return(drug_snp_effects %>% as_tibble())
  }

#' Extracts the drug snp adverse drug reactions element and return data as
#' tibble.
#'
#' \code{drug_snp_adverse_reactions } returns tibble of drug
#'  snp adverse drug reactions elements.
#'
#' This functions extracts the groups element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#'  \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug snp adverse drug reactions node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_snp_adverse_reactions()
#'
#' # will throw an error, as database_connection is NULL
#' drug_snp_adverse_reactions(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_snp_adverse_reactions(save_table = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_snp_adverse_reactions(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_snp_adverse_reactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
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
      save_drug_sub(con = database_connection,
                    df = snp_adverse_reactions,
                    table_name = "snp_adverse_reactions")
    }
    return(snp_adverse_reactions %>% as_tibble())
  }

# Extract drug Pharmacology
drug_pharmacology_rec <- function(drug) {
  c(
    drugbank_id = xmlValue(drug[["drugbank-id"]]),
    indication = xmlValue(drug[["indication"]]),
    pharmacodynamics = xmlValue(drug[["pharmacodynamics"]]),
    mechanism_of_action = xmlValue(drug[["mechanism-of-action"]]),
    toxicity = xmlValue(drug[["toxicity"]]),
    metabolism = xmlValue(drug[["metabolism"]]),
    absorption = xmlValue(drug[["absorption"]]),
    half_life = xmlValue(drug[["half-life"]]),
    protein_binding = xmlValue(drug[["protein-binding"]]),
    route_of_elimination = xmlValue(drug[["route-of-elimination"]]),
    volume_of_distribution = xmlValue(drug[["volume-of-distribution"]]),
    clearance = xmlValue(drug[["clearance"]])
  )
}


#' Extracts the drug_pharmacology elements and return data as tibble.
#'
#' \code{drug_pharmacology} returns tibble of drug_pharmacologys main elements.
#'
#' This functions extracts the main element of drug_pharmacology node in
#' drugbank
#' xml database with the option to save it in a user defined database.
#' It takes two optional arguments to save the returned tibble in the database
#' \code{save_table} and \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug_pharmacology attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_pharmacology()
#'
#' # will throw an error, as database_connection is NULL
#' drug_pharmacology(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_pharmacology(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pharmacology(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pharmacology(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' #  and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pharmacology(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_pharmacology(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_pharmacology <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drugs_pharmacology", csv_path)
    if (!override_csv & file.exists(path)) {
      drugs_pharmacology <- readr::read_csv(path)
    } else {
      drugs_pharmacology <- xmlSApply(xmlRoot(pkg_env$root),
                                      drug_pharmacology_rec)
      drugs_pharmacology <- as_tibble(t(drugs_pharmacology))
      write_csv(drugs_pharmacology, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drugs_pharmacology,
        table_name = "drug_pharmacology",
        primary_key = "drugbank_id",
        foreign_key = NULL,
        field_types = list(
          drugbank_id = paste0("varchar(", max(nchar(
            drugs_pharmacology$drugbank_id
          )), ")"),
          mechanism_of_action = "varchar(MAX)",
          pharmacodynamics = "varchar(MAX)",
          indication = paste0("varchar(", max(nchar(
            drugs_pharmacology$indication
          ), na.rm = TRUE) + 10, ")"),
          absorption = paste0("varchar(", max(nchar(
            drugs_pharmacology$absorption
          ), na.rm = TRUE) + 10, ")"),
          route_of_elimination = paste0("varchar(", max(
            nchar(drugs_pharmacology$route_of_elimination),
            na.rm = TRUE
          ) + 10, ")"),
          metabolism = paste0("varchar(", max(nchar(
            drugs_pharmacology$metabolism
          ), na.rm = TRUE) + 10, ")"),
          clearance = paste0("varchar(", max(nchar(
            drugs_pharmacology$clearance
          ), na.rm = TRUE) + 10, ")"),
          half_life = paste0("varchar(", max(nchar(
            drugs_pharmacology$half_life
          ), na.rm = TRUE) + 10, ")"),
          volume_of_distribution = paste0("varchar(", max(
            nchar(drugs_pharmacology$volume_of_distribution),
            na.rm = TRUE
          ) + 10, ")"),
          protein_binding = paste0("varchar(", max(
            nchar(drugs_pharmacology$protein_binding),
            na.rm = TRUE
          ) + 10, ")"),
          toxicity = "varchar(MAX)"
        )
      )
    }

    return(drugs_pharmacology %>% as_tibble())
  }
