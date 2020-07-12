PathwayParser <- R6::R6Class(
  "PathwayParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      map_df(drugs, ~ private$get_pathways_df(.x, pb)) %>%
        unique()
    },
    get_pathways_df = function(rec, pb) {
      pb$tick()
      return(map_df(
        xmlChildren(rec[["pathways"]]),
        ~ private$get_pathway_rec(.x, xmlValue(rec["drugbank-id"][[1]]))
      ))
    },
    get_pathway_rec = function(r, drug_key) {
      tibble(
        smpdb_id = xmlValue(r[["smpdb-id"]]),
        name = xmlValue(r[["name"]]),
        category = xmlValue(r[["category"]]),
        parent_key = drug_key
      )
    }
  )
)

# Extract drug pathways drugs df
get_pathways_drugs_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["pathways"]]),
    ~ drug_sub_df(.x, "drugs", id = "smpdb-id")
  ))
}

# Extract drug pathways enzymes df
get_pathways_enzymes_df <- function(rec) {
  return(map_df(
    xmlChildren(rec[["pathways"]]),
    ~ drug_sub_df(.x, "enzymes", id = "smpdb-id")
  ))
}

#' Extracts the drug pathway enzyme element and return data as tibble.
#'
#' \code{drug_pathway_enzyme} returns tibble of drug pathway enzyme
#'  elements.
#'
#' This functions extracts the pathway enzyme element of drug node in drugbank
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
#' the new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug pathway enzyme node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_pathway_enzyme()
#'
#' # will throw an error, as database_connection is NULL
#' drug_pathway_enzyme(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_pathway_enzyme(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pathway_enzyme(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pathway_enzyme(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pathway_enzyme(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_pathway_enzyme(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_pathway_enzyme <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    path <-
      get_dataset_full_path("drug_pathway_enzymes", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_pathway_enzymes <- readr::read_csv(path)
    } else {
      drug_pathway_enzymes <-
        map_df(pkg_env$children, ~ get_pathways_enzymes_df(.x)) %>%
        unique()
      write_csv(drug_pathway_enzymes, save_csv, csv_path)
    }


    if (nrow(drug_pathway_enzymes) > 0) {
      colnames(drug_pathway_enzymes) <- c("enzyme", "pathway_id")
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_pathway_enzymes,
        table_name = "drug_pathway_enzyme",
        save_table_only = TRUE
      )
    }
    return(drug_pathway_enzymes %>% as_tibble())
  }

#' Extracts the drug pathway drugs element and return data as tibble.
#'
#' \code{drug_pathway_drugs} returns tibble of drug pathway drugs
#'  elements.
#'
#' This functions extracts the pathway drugs element of drug node in
#' \strong{DrugBank}
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
#' the new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#'
#' @return drug pathway drugs node attributes date frame
#'
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_pathway_drugs()
#'
#' # will throw an error, as database_connection is NULL
#' drug_pathway_drugs(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_pathway_drugs(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pathway_drugs(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pathway_drugs(save_table = TRUE, save_csv = TRUE,
#' database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pathway_drugs(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_pathway_drugs(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_pathway_drugs <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_pathway_drugs", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_pathway_drugs <- readr::read_csv(path)
    } else {
      drug_pathway_drugs <-
        map_df(pkg_env$children, ~ get_pathways_drugs_df(.x)) %>%
        unique()

      write_csv(drug_pathway_drugs, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_pathway_drugs,
        table_name = "drug_pathway_drugs",
        save_table_only = TRUE
      )
    }
    return(drug_pathway_drugs %>% as_tibble())
  }

#' Drug Pathway parser
#'
#' Metabolic, disease, and biological pathways that the drug is involved in, as
#' identified by the Small Molecule Protein Database (SMPDB).
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{smpdb-id}{Small Molecule Pathway Database identifier for this
#'  pathway.}
#'  \item{name}{Pathway name}
#'  \item{category}{Pathway category}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family pathway
#'
#' @inherit drug_all examples
#' @export
drug_pathway <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    PathwayParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_pathway"
    )$parse()
  }
