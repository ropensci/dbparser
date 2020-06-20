references <- function(children,
                      ref_title = "references",
                      ref_type = "textbooks",
                      id = "id") {
  return(map_df(
    children,
    ~ drug_sub_df(.,
                  ref_title,
                  seconadary_node = ref_type,
                  id = id)
  ))
}

reference_parser <- function(save_table = FALSE,
                             save_csv = FALSE,
                             csv_path = ".",
                             override_csv = FALSE,
                             database_connection = NULL,
                             tibble_name,
                             child_node = NULL,
                             ref_title = "references",
                             ref_type = "textbooks",
                             id = "id") {
  check_parameters_validation(save_table, database_connection)
  path <- get_dataset_full_path(tibble_name, csv_path)
  drugs <-  xmlChildren(pkg_env$root)
  references_tbl <- NULL
  if (!override_csv & file.exists(path)) {
    references_tbl <- readr::read_csv(path)
  } else {
    if (is.null(child_node)) {
      references_tbl <- map_df(drugs,
                              ~ drug_sub_df(.,
                                            ref_title,
                                            seconadary_node = ref_type,
                                            id = id))
    } else {
      references_tbl <-  map_df(drugs, ~ references(
        xmlChildren(.[[child_node]]),
        ref_title = ref_title,
        ref_type = ref_type))
    }
    references_tbl <- references_tbl %>% unique()
    write_csv(references_tbl, save_csv, csv_path)
  }

  if (save_table) {
    save_drug_sub(con = database_connection,
                  df = references_tbl,
                  table_name = tibble_name)
  }
  return(references_tbl %>% as_tibble())
}

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters books element parser
#'
#' Return a list of text books that were used as references for drugs, carriers,
#'  enzymes, targets or transporters
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'   \item{ref-id}{Identifier for the article being referenced.
#'   This is unique across all reference types (books, links, article,
#'   attachments).}
#'   \item{isbn}{ISBN identifying the textbook.}
#'   \item{citation}{A Textbook citation in a standard format.}
#'   \item{\emph{parent_id}}{drug/ carrier/ target/ enzyme/ transporter id}
#' }
#' @family references
#'
#' @inherit drug_all examples
#' @name books
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters links element parser
#'
#' Return a list of websites that were used as references for
#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'   \item{ref-id}{Name of the source website}
#'   \item{title}{Identifier for this drug in the given resource}
#'   \item{url}{The url of the website}
#'   \item{\emph{parent_id}}{drug/ carrier/ target/ enzyme/ transporter id}
#' }
#' @family references
#'
#' @inherit drug_all examples
#' @name links
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters articles element parser
#'
#' Return a list of articles that were used as references for drugs carriers
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'   \item{ref-id}{Identifier for the article being referenced.
#'   This is unique across all reference types (books, links, article,
#'   attachments).}
#'   \item{pubmed-id}{The PubMed identifier for the article.}
#'   \item{citation}{Article citation in a standard format.}
#'   \item{\emph{parent_id}}{drug/carrier/target/enzyme/transporter id}
#' }
#' @family references
#'
#' @inherit drug_all examples
#' @name articles
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters attachments element parser
#'
#' Return a list of attachment that were used as references for drugs carriers
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'   \item{ref-id}{Identifier for the article being referenced.
#'   This is unique across all reference types (books, links, article,
#'   attachments).}
#'   \item{title}{The title of the attachment.}
#'   \item{url}{The url to download the attachment from.}
#'   \item{\emph{parent_id}}{drug/carrier/target/enzyme/transporter id}
#' }
#' @family references
#'
#' @inherit drug_all examples
#' @name attachments
NULL

#' @rdname books
#' @export
drugs_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drugs_textbooks",
      ref_title = "general-references",
      ref_type = "textbooks",
      id = "drugbank-id"
    )
  }

#' @rdname books
#' @export
carriers_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_carriers_textbooks",
      child_node = "carriers",
      ref_type = "textbooks"
    )
  }

#' @rdname books
#' @export
enzymes_textbooks <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  reference_parser(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_enzymes_textbooks",
    child_node = "enzymes",
    ref_type = "textbooks"
  )
}

#' @rdname books
#' @export
targets_textbooks <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  reference_parser(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_targ_textbooks",
    child_node = "targets",
    ref_type = "textbooks"
  )
}

#' @rdname books
#' @export
transporters_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_trans_textbooks",
      child_node = "transporters",
      ref_type = "textbooks"
    )
  }

#' @rdname links
#' @export
drugs_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drugs_links",
      ref_title = "general-references",
      ref_type = "links",
      id = "drugbank-id"
    )
  }

#' @rdname links
#' @export
carriers_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_carriers_links",
      child_node = "carriers",
      ref_type = "links"
    )
  }

#' @rdname links
#' @export
enzymes_links <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  reference_parser(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_enzymes_links",
    child_node = "enzymes",
    ref_type = "links"
  )
}

#' @rdname links
#' @export
targets_links <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  reference_parser(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_targ_links",
    child_node = "targets",
    ref_type = "links"
  )
}

#' @rdname links
#' @export
transporters_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_trans_links",
      child_node = "transporters",
      ref_type = "links"
    )
  }

#' @rdname articles
#' @export
drugs_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drugs_articles",
      ref_title = "general-references",
      ref_type = "articles",
      id = "drugbank-id"
    )
  }

#' @rdname articles
#' @export
carriers_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_carriers_articles",
      child_node = "carriers",
      ref_type = "articles"
    )
  }

#' @rdname articles
#' @export
enzymes_articles <- function(save_table = FALSE,
                          save_csv = FALSE,
                          csv_path = ".",
                          override_csv = FALSE,
                          database_connection = NULL) {
  reference_parser(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_enzymes_articles",
    child_node = "enzymes",
    ref_type = "articles"
  )
}

#' @rdname articles
#' @export
targets_articles <- function(save_table = FALSE,
                          save_csv = FALSE,
                          csv_path = ".",
                          override_csv = FALSE,
                          database_connection = NULL) {
  reference_parser(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_targ_articles",
    child_node = "targets",
    ref_type = "articles"
  )
}

#' @rdname articles
#' @export
transporters_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_trans_articles",
      child_node = "transporters",
      ref_type = "articles"
    )
  }
#' @rdname attachments
#' @export
drugs_attachments <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drugs_attachments",
      ref_title = "general-references",
      ref_type = "attachments",
      id = "drugbank-id"
    )
  }

#' @rdname attachments
#' @export
carriers_attachments <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_carriers_attachments",
      child_node = "carriers",
      ref_type = "attachments"
    )
  }

#' @rdname attachments
#' @export
enzymes_attachments <- function(save_table = FALSE,
                             save_csv = FALSE,
                             csv_path = ".",
                             override_csv = FALSE,
                             database_connection = NULL) {
  reference_parser(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_enzymes_attachments",
    child_node = "enzymes",
    ref_type = "attachments"
  )
}

#' @rdname attachments
#' @export
targets_attachments <- function(save_table = FALSE,
                             save_csv = FALSE,
                             csv_path = ".",
                             override_csv = FALSE,
                             database_connection = NULL) {
  reference_parser(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_targ_attachments",
    child_node = "targets",
    ref_type = "attachments"
  )
}

#' @rdname attachments
#' @export
transporters_attachments <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    reference_parser(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_trans_attachments",
      child_node = "transporters",
      ref_type = "attachments"
    )
  }
