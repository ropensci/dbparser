ReferenceParser <-
  R6::R6Class(
    "ReferenceParser",
    inherit = AbstractParser,
    private = list(
      parse_record = function() {
        drugs <-  xmlChildren(pkg_env$root)
        pb <- progress_bar$new(total = xmlSize(drugs))
        if (is.null(private$object_node)) {
          return(map_df(
            drugs,
            ~ drug_sub_df(
              .,
              private$main_node,
              private$secondary_node,
              private$id,
              pb
            )
          ))
        }
        return(map_df(drugs,
                      ~ private$parse_ref_elem(
                        xmlChildren(.[[private$object_node]]),
                        pb)))
      },
      parse_ref_elem = function(children, pb) {
        pb$tick()
        return(map_df(
          children,
          ~ drug_sub_df(.,
                        private$main_node,
                        private$secondary_node,
                        "id")
        ))
      }
    )
  )

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters books element parser
#'
#' Return a list of text books that were used as references for drugs, carriers,
#'  enzymes, targets or transporters
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
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
#' @inherit run_all_parsers examples
#' @name books
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters links element parser
#'
#' Return a list of websites that were used as references for
#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
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
#' @inherit run_all_parsers examples
#' @name links
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters articles element parser
#'
#' Return a list of articles that were used as references for drugs carriers
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
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
#' @inherit run_all_parsers examples
#' @name articles
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters attachments element parser
#'
#' Return a list of attachment that were used as references for drugs carriers
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
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
#' @inherit run_all_parsers examples
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
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drugs_textbooks",
      main_node = "general-references",
      secondary_node = "textbooks",
      id = "drugbank-id"
    )$parse()
  }

#' @rdname books
#' @export
carriers_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_carriers_textbooks",
      object_node = "carriers",
      main_node = "references",
      secondary_node = "textbooks",
      id = "id"
    )$parse()
  }

#' @rdname books
#' @export
enzymes_textbooks <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  ReferenceParser$new(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_enzymes_textbooks",
    object_node = "enzymes",
    main_node = "references",
    secondary_node = "textbooks",
    id = "id"
  )$parse()
}

#' @rdname books
#' @export
targets_textbooks <- function(save_table = FALSE,
                              save_csv = FALSE,
                              csv_path = ".",
                              override_csv = FALSE,
                              database_connection = NULL) {
  ReferenceParser$new(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_targ_textbooks",
    object_node = "targets",
    main_node = "references",
    secondary_node = "textbooks",
    id = "id"
  )$parse()
}

#' @rdname books
#' @export
transporters_textbooks <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_trans_textbooks",
      object_node = "transporters",
      main_node = "references",
      secondary_node = "textbooks",
      id = "id"
    )$parse()
  }

#' @rdname links
#' @export
drugs_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drugs_links",
      main_node = "general-references",
      secondary_node = "links",
      id = "drugbank-id"
    )$parse()
  }

#' @rdname links
#' @export
carriers_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_carriers_links",
      object_node = "carriers",
      main_node = "references",
      secondary_node = "links",
      id = "id"
    )$parse()
  }

#' @rdname links
#' @export
enzymes_links <- function(save_table = FALSE,
                          save_csv = FALSE,
                          csv_path = ".",
                          override_csv = FALSE,
                          database_connection = NULL) {
  ReferenceParser$new(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_enzymes_links",
    object_node = "enzymes",
    main_node = "references",
    secondary_node = "links",
    id = "id"
  )$parse()
}

#' @rdname links
#' @export
targets_links <- function(save_table = FALSE,
                          save_csv = FALSE,
                          csv_path = ".",
                          override_csv = FALSE,
                          database_connection = NULL) {
  ReferenceParser$new(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_targ_links",
    object_node = "targets",
    main_node = "references",
    secondary_node = "links",
    id = "id"
  )$parse()
}

#' @rdname links
#' @export
transporters_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_trans_links",
      object_node = "transporters",
      main_node = "references",
      secondary_node = "links",
      id = "id"
    )$parse()
  }

#' @rdname articles
#' @export
drugs_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drugs_articles",
      main_node = "general-references",
      secondary_node = "articles",
      id = "drugbank-id"
    )$parse()
  }

#' @rdname articles
#' @export
carriers_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_carriers_articles",
      object_node = "carriers",
      main_node = "references",
      secondary_node = "articles",
      id = "id"
    )$parse()
  }

#' @rdname articles
#' @export
enzymes_articles <- function(save_table = FALSE,
                             save_csv = FALSE,
                             csv_path = ".",
                             override_csv = FALSE,
                             database_connection = NULL) {
  ReferenceParser$new(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_enzymes_articles",
    object_node = "enzymes",
    main_node = "references",
    secondary_node = "articles",
    id = "id"
  )$parse()
}

#' @rdname articles
#' @export
targets_articles <- function(save_table = FALSE,
                             save_csv = FALSE,
                             csv_path = ".",
                             override_csv = FALSE,
                             database_connection = NULL) {
  ReferenceParser$new(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_targ_articles",
    object_node = "targets",
    main_node = "references",
    secondary_node = "articles",
    id = "id"
  )$parse()
}

#' @rdname articles
#' @export
transporters_articles <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_trans_articles",
      object_node = "transporters",
      main_node = "references",
      secondary_node = "articles",
      id = "id"
    )$parse()
  }
#' @rdname attachments
#' @export
drugs_attachments <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drugs_attachments",
      main_node = "general-references",
      secondary_node = "attachments",
      id = "drugbank-id"
    )$parse()
  }

#' @rdname attachments
#' @export
carriers_attachments <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_carriers_attachments",
      object_node = "carriers",
      main_node = "references",
      secondary_node = "attachments",
      id = "id"
    )$parse()
  }

#' @rdname attachments
#' @export
enzymes_attachments <- function(save_table = FALSE,
                                save_csv = FALSE,
                                csv_path = ".",
                                override_csv = FALSE,
                                database_connection = NULL) {
  ReferenceParser$new(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_enzymes_attachments",
    object_node = "enzymes",
    main_node = "references",
    secondary_node = "attachments",
    id = "id"
  )$parse()
}

#' @rdname attachments
#' @export
targets_attachments <- function(save_table = FALSE,
                                save_csv = FALSE,
                                csv_path = ".",
                                override_csv = FALSE,
                                database_connection = NULL) {
  ReferenceParser$new(
    save_table = save_table,
    save_csv = save_csv,
    csv_path = csv_path,
    override_csv = override_csv,
    database_connection = database_connection,
    tibble_name = "drug_targ_attachments",
    object_node = "targets",
    main_node = "references",
    secondary_node = "attachments",
    id = "id"
  )$parse()
}

#' @rdname attachments
#' @export
transporters_attachments <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    ReferenceParser$new(
      save_table = save_table,
      save_csv = save_csv,
      csv_path = csv_path,
      override_csv = override_csv,
      database_connection = database_connection,
      tibble_name = "drug_trans_attachments",
      object_node = "transporters",
      main_node = "references",
      secondary_node = "attachments",
      id = "id"
    )$parse()
  }
