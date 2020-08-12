CETTPolyGeneralInfoParser <-
  R6::R6Class(
    "CETTPolyGeneralInfoParser",
    inherit = AbstractParser,
    private = list(
      parse_record = function() {
        cett_type <- strsplit(private$tibble_name, "_")[[1]][1]
        drugs <-  xmlChildren(pkg_env$root)
        pb <- progress_bar$new(total = xmlSize(drugs))
        polypeptides_tbl <-
          map_df(drugs,
                 ~ private$polypeptides_parser(., cett_type, pb)) %>% unique()
      },
      polypeptides_parser = function(rec, cett_type, pb) {
        pb$tick()
        return(map_df(xmlChildren(rec[[cett_type]]),
                      ~ private$polypeptide_rec(.)))
      },
      polypeptide_rec = function(r) {
        parent_id <- xmlValue(r[["id"]])
        p <- r[["polypeptide"]]
        if (!is.null(p)) {
          tibble(
            id = ifelse(is.null(xmlGetAttr(p, name = "id")), NA,
                        xmlGetAttr(p, name = "id")),
            source = ifelse(is.null(xmlGetAttr(p,
                                               name = "source")), NA,
                            xmlGetAttr(p, name = "source")),
            name = xmlValue(p[["name"]]),
            general_function = xmlValue(p[["general-function"]]),
            specific_function = xmlValue(p[["specific-function"]]),
            gene_name = xmlValue(p[["gene-name"]]),
            locus = xmlValue(p[["locus"]]),
            cellular_location = xmlValue(p[["cellular-location"]]),
            transmembrane_regions = xmlValue(p[["transmembrane-regions"]]),
            signal_regions = xmlValue(p[["signal-regions"]]),
            theoretical_pi = xmlValue(p[["theoretical-pi"]]),
            molecular_weight = xmlValue(p[["molecular-weight"]]),
            chromosome_location = xmlValue(p[["chromosome_location"]]),
            organism = xmlValue(p[["organism"]]),
            organism_ncbi_taxonomy_id = xmlGetAttr(p[["organism"]],
                                                   name = "ncbi-taxonomy-id"),
            amino_acid_sequence = xmlValue(p[["amino-acid-sequence"]]),
            amino_acid_format = xmlGetAttr(p[["amino-acid-sequence"]],
                                           name = "format"),
            gene_sequence = xmlValue(p[["gene-sequence"]]),
            gene_format = xmlGetAttr(p[["gene-sequence"]],
                                     name = "format"),
            parent_id = parent_id
          )
        }
      },
      save_db_table = function(parsed_tbl) {
        if (private$save_table) {
          save_drug_sub(
            con = private$database_connection,
            df = parsed_tbl,
            table_name = private$tibble_name,
            save_table_only = TRUE,
            field_types = list(
              general_function =
                paste("varchar(",
                      max(
                        nchar(parsed_tbl$general_function),
                        na.rm = TRUE
                      ), ")",
                      sep = ""),
              specific_function = "varchar(MAX)",
              amino_acid_sequence = "varchar(MAX)",
              gene_sequence = "varchar(MAX)"
            )
          )
        }
      }
    )
  )

#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide parsers
#'
#' Extract descriptions of identified polypeptide targets, enzymes, carriers,
#'  or transporters.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return a tibble with 20 variables:
#' \describe{
#'   \item{id}{\href{https://www.uniprot.org/}{Universal Protein Resource
#'   (UniProt) identifier}}
#'   \item{source}{Specifies whether the identified polypeptide ID is
#'   associated with any of the following UniProt knowledge bases:
#'   Swiss-Prot, which is manually annotated and reviewed, or TrEMBL,
#'   which is automatically annotated and not reviewed.}
#'   \item{name}{}
#'   \item{general_function}{General summary of the physiological function of
#'   the polypeptide}
#'   \item{specific_function}{A more specific description of the polypeptide’s
#'    physiological function within the cell.}
#'   \item{gene_name}{The short name commonly associated with the associated
#'    gene. Eg. PTGS1.}
#'   \item{locus}{The specific chromosomal location or position of the gene’s
#'    sequence on a chromosome.}
#'   \item{cellular_location}{The cellular location of the polypeptide.}
#'   \item{transmembrane_regions}{Areas of the polypeptide sequence that span
#'    a biological membrane.}
#'   \item{signal_regions}{Location of any signal peptides within the
#'    polypeptide sequence.}
#'   \item{theoretical_pi}{Theoretical isoelectric point.}
#'   \item{molecular_weight}{The molecular weight of the polypeptide.}
#'   \item{chromosome_location}{The chromosomal location of the polypeptide
#'   gene}
#'   \item{organism}{The organism in which this polypeptide functions.}
#'   \item{organism_ncbi_taxonomy_id}{}
#'   \item{amino_acid_sequence}{The amino acid sequence of the polypeptide}
#'   \item{amino_acid_format}{}
#'   \item{gene_sequence}{The sequence of the associated gene.}
#'   \item{gene_format}{}
#'   \item{parent_key}{carrier/ target/ enzyme/ transporter id}
#' }
#' @family cett
#' @inherit run_all_parsers examples
#' @name cett_poly_doc
NULL

#' @rdname cett_poly_doc
#' @export
carriers_polypeptides <- function(save_table = FALSE,
                                  save_csv = FALSE,
                                  csv_path = ".",
                                  override_csv = FALSE,
                                  database_connection = NULL) {
  CETTPolyGeneralInfoParser$new(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "carriers_polypeptides"
  )$parse()
}

#' @rdname cett_poly_doc
#' @export
enzymes_polypeptides <- function(save_table = FALSE,
                                 save_csv = FALSE,
                                 csv_path = ".",
                                 override_csv = FALSE,
                                 database_connection = NULL) {
  CETTPolyGeneralInfoParser$new(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "enzymes_polypeptides"
  )$parse()
}

#' @rdname cett_poly_doc
#' @export
targets_polypeptides <- function(save_table = FALSE,
                                 save_csv = FALSE,
                                 csv_path = ".",
                                 override_csv = FALSE,
                                 database_connection = NULL) {
  CETTPolyGeneralInfoParser$new(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "targets_polypeptides"
  )$parse()
}

#' @rdname cett_poly_doc
#' @export
transporters_polypeptides <- function(save_table = FALSE,
                                      save_csv = FALSE,
                                      csv_path = ".",
                                      override_csv = FALSE,
                                      database_connection = NULL) {
  CETTPolyGeneralInfoParser$new(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "transporters_polypeptides"
  )$parse()
}
