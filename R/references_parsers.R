ReferenceParser <-
  R6::R6Class(
    "ReferenceParser",
    inherit = AbstractParser,
    private = list(
      parse_record = function() {
        drugs     <-  xmlChildren(pkg_env$root)
        pb        <- progress_bar$new(total = xmlSize(drugs))
        ref_table <- NULL
        id_name   <- NULL

        if (is.null(private$object_node)) {
          id_name   <- "drugbank_id"
          ref_table <- map_df(
            drugs,
            ~ drug_sub_df(
              .,
              private$main_node,
              private$secondary_node,
              private$id,
              pb
            )
          )
        } else {
          id_name <- paste0(substr(x     = private$object_node,
                                   start = 1,
                                   stop  = nchar(private$object_node)-1),
                            "_id")
          ref_table <- map_df(drugs,
                 ~ private$parse_ref_elem(
                   xmlChildren(.[[private$object_node]]),
                   pb))
        }

        if ("parent_key" %in% names(ref_table)) {
          ref_table <- ref_table %>%
            rename(!!id_name := parent_key)
        }

        ref_table
      },
      parse_ref_elem = function(children, pb) {
        pb$tick()
        map_df(
          children,
          ~ drug_sub_df(.,
                        private$main_node,
                        private$secondary_node,
                        "id")
        )
      }
    )
  )

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters books element parser
#'
#' Return a list of text books that were used as references for drugs, carriers,
#'  enzymes, targets or transporters
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
#' @keywords internal
#'
#' @name books
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters links element parser
#'
#' Return a list of websites that were used as references for
#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'   \item{ref-id}{Name of the source website}
#'   \item{title}{Identifier for this drug in the given resource}
#'   \item{url}{The url of the website}
#'   \item{\emph{parent_id}}{drug/ carrier/ target/ enzyme/ transporter id}
#' }
#' @keywords internal
#'
#' @name links
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters articles element parser
#'
#' Return a list of articles that were used as references for drugs carriers
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
#' @keywords internal
#'
#' @name articles
NULL

#' Drugs/ Carriers/ Enzymes/ Targets/ Transporters attachments element parser
#'
#' Return a list of attachment that were used as references for drugs carriers
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
#' @keywords internal
#'
#' @name attachments
NULL


#' @rdname books
drugs_textbooks <- function() {
    ReferenceParser$new(
      tibble_name = "drugs_textbooks",
      main_node = "general-references",
      secondary_node = "textbooks",
      id = "drugbank-id"
    )$parse()
  }

#' @rdname books
carriers_textbooks <- function() {
    ReferenceParser$new(
      tibble_name = "drug_carriers_textbooks",
      object_node = "carriers",
      main_node = "references",
      secondary_node = "textbooks",
      id = "id"
    )$parse()
  }

#' @rdname books
enzymes_textbooks <- function() {
  ReferenceParser$new(
    tibble_name = "drug_enzymes_textbooks",
    object_node = "enzymes",
    main_node = "references",
    secondary_node = "textbooks",
    id = "id"
  )$parse()
}

#' @rdname books
targets_textbooks <- function() {
  ReferenceParser$new(
    tibble_name = "drug_targ_textbooks",
    object_node = "targets",
    main_node = "references",
    secondary_node = "textbooks",
    id = "id"
  )$parse()
}

#' @rdname books
transporters_textbooks <- function() {
    ReferenceParser$new(
      tibble_name = "drug_trans_textbooks",
      object_node = "transporters",
      main_node = "references",
      secondary_node = "textbooks",
      id = "id"
    )$parse()
  }

#' @rdname links
drugs_links <- function() {
    ReferenceParser$new(
      tibble_name = "drugs_links",
      main_node = "general-references",
      secondary_node = "links",
      id = "drugbank-id"
    )$parse()
  }


#' @rdname links
carriers_links <- function() {
    ReferenceParser$new(
      tibble_name = "drug_carriers_links",
      object_node = "carriers",
      main_node = "references",
      secondary_node = "links",
      id = "id"
    )$parse()
  }


#' @rdname links
enzymes_links <- function() {
  ReferenceParser$new(
    tibble_name = "drug_enzymes_links",
    object_node = "enzymes",
    main_node = "references",
    secondary_node = "links",
    id = "id"
  )$parse()
}


#' @rdname links
targets_links <- function() {
  ReferenceParser$new(
    tibble_name = "drug_targ_links",
    object_node = "targets",
    main_node = "references",
    secondary_node = "links",
    id = "id"
  )$parse()
}


#' @rdname links
transporters_links <- function() {
    ReferenceParser$new(
      tibble_name = "drug_trans_links",
      object_node = "transporters",
      main_node = "references",
      secondary_node = "links",
      id = "id"
    )$parse()
  }


#' @rdname articles
drugs_articles <- function() {
    ReferenceParser$new(
      tibble_name = "drugs_articles",
      main_node = "general-references",
      secondary_node = "articles",
      id = "drugbank-id"
    )$parse()
  }


#' @rdname articles
carriers_articles <- function() {
    ReferenceParser$new(
      tibble_name = "drug_carriers_articles",
      object_node = "carriers",
      main_node = "references",
      secondary_node = "articles",
      id = "id"
    )$parse()
  }


#' @rdname articles
enzymes_articles <- function() {
  ReferenceParser$new(
    tibble_name = "drug_enzymes_articles",
    object_node = "enzymes",
    main_node = "references",
    secondary_node = "articles",
    id = "id"
  )$parse()
}


#' @rdname articles
targets_articles <- function() {
  ReferenceParser$new(
    tibble_name = "drug_targ_articles",
    object_node = "targets",
    main_node = "references",
    secondary_node = "articles",
    id = "id"
  )$parse()
}


#' @rdname articles
transporters_articles <- function() {
    ReferenceParser$new(
      tibble_name = "drug_trans_articles",
      object_node = "transporters",
      main_node = "references",
      secondary_node = "articles",
      id = "id"
    )$parse()
}


#' @rdname attachments
drugs_attachments <- function() {
    ReferenceParser$new(
      tibble_name = "drugs_attachments",
      main_node = "general-references",
      secondary_node = "attachments",
      id = "drugbank-id"
    )$parse()
  }


#' @rdname attachments
carriers_attachments <- function() {
    ReferenceParser$new(
      tibble_name = "drug_carriers_attachments",
      object_node = "carriers",
      main_node = "references",
      secondary_node = "attachments",
      id = "id"
    )$parse()
  }


#' @rdname attachments
enzymes_attachments <- function() {
  ReferenceParser$new(
    tibble_name = "drug_enzymes_attachments",
    object_node = "enzymes",
    main_node = "references",
    secondary_node = "attachments",
    id = "id"
  )$parse()
}


#' @rdname attachments
targets_attachments <- function() {
  ReferenceParser$new(
    tibble_name = "drug_targ_attachments",
    object_node = "targets",
    main_node = "references",
    secondary_node = "attachments",
    id = "id"
  )$parse()
}


#' @rdname attachments
transporters_attachments <- function() {
    ReferenceParser$new(
      tibble_name    = "drug_trans_attachments",
      object_node    = "transporters",
      main_node      = "references",
      secondary_node = "attachments",
      id             = "id"
    )$parse()
}
