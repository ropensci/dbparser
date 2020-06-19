textbooks <- function(rec, parent_node) {
  return(map_df(
    xmlChildren(rec[[parent_node]]),
    ~ drug_sub_df(.x,
                  "references",
                  seconadary_node = "textbooks",
                  id = "id")
  ))
}


#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters books element parser
#'
#' A list of text books that were used as references for drugs, carriers,
#'  enzymes, targets or transporters
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'   \item{ref-id}{Identifier for the article being referenced.
#'   This is unique across all reference types (books, links, article).}
#'   \item{isbn}{ISBN identifying the textbook.}
#'   \item{citation}{A Textbook citation in a standard format.}
#'   \item{\emph{parent_id}}{drug/ carrier/ target/ enzyme/ transporter id}
#' }
#' @family references
#'
#' @inherit drug_all examples
#' @name books
NULL

#' @rdname books
#' @export
drugs_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drugs_textbooks", csv_path)
    if (!override_csv & file.exists(path)) {
      drugs_textbooks <- readr::read_csv(path)
    } else {
      drugs_textbooks <- map_df(
        xmlChildren(pkg_env$root),
        ~ drug_sub_df(
          .x,
          "general-references",
          seconadary_node = "textbooks",
          id = "drugbank-id"
        )
      )
      write_csv(drugs_textbooks, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drugs_textbooks,
                    table_name = "drugs_textbooks")
    }
    return(drugs_textbooks %>% as_tibble())
  }

#' @rdname books
#' @export
carriers_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_carriers_textbooks", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_carriers_textbooks <- readr::read_csv(path)
    } else {
      drug_carriers_textbooks <-
        map_df(pkg_env$children, ~ textbooks(., "carriers")) %>% unique()

      write_csv(drug_carriers_textbooks, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_carriers_textbooks,
        table_name = "drug_carriers_textbooks",
        save_table_only = TRUE
      )
    }
    return(drug_carriers_textbooks %>% as_tibble())
  }

#' @rdname books
#' @export
enzymes_textbooks <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  check_parameters_validation(save_table, database_connection)
  path <-
    get_dataset_full_path("drug_enzymes_textbooks", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_enzymes_textbooks <- readr::read_csv(path)
  } else {
    drug_enzymes_textbooks <-
      map_df(pkg_env$children, ~ textbooks(., "enzymes")) %>%
      unique()

    write_csv(drug_enzymes_textbooks, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(
      con = database_connection,
      df = drug_enzymes_textbooks,
      table_name = "drug_enzymes_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_enzymes_textbooks %>% as_tibble())
}

#' @rdname books
#' @export
targets_textbooks <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  check_parameters_validation(save_table, database_connection)
  path <-
    get_dataset_full_path("drug_targ_textbooks", csv_path)
  if (!override_csv & file.exists(path)) {
    drug_targ_textbooks <- readr::read_csv(path)
  } else {
    drug_targ_textbooks <-
      map_df(pkg_env$children, ~ textbooks(., "targets")) %>% unique()

    write_csv(drug_targ_textbooks, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = database_connection,
      df = drug_targ_textbooks,
      table_name = "drug_targ_textbooks",
      save_table_only = TRUE
    )
  }
  return(drug_targ_textbooks %>% as_tibble())
}

#' @rdname books
#' @export
transporters_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_trans_textbooks", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_trans_textbooks <- readr::read_csv(path)
    } else {
      drug_trans_textbooks <-
        map_df(pkg_env$children, ~ textbooks(., "targets")) %>%
        unique()

      write_csv(drug_trans_textbooks, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drug_trans_textbooks,
        table_name = "drug_trans_textbooks",
        save_table_only = TRUE
      )
    }
    return(drug_trans_textbooks %>% as_tibble())
  }
