#' Carriers/ Enzymes/ Targets/ Transporters Polypeptide parsers
#'
#' Extract descriptions of identified polypeptide targets, enzymes, carriers,
#'  or transporters.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with 20 variables:
#' \describe{
#'   \item{id}{\href{http://www.uniprot.org/}{Universal Protein Resource
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
#' @inherit drug_all examples
#' @name cett_poly_doc
NULL

polypeptide_rec <- function(r) {
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
}

polypeptides_parser <- function(rec, cett_type) {
  return(map_df(xmlChildren(rec[[cett_type]]), ~ polypeptide_rec(.)))
}

polypeptides <- function(save_table = FALSE,
                         save_csv = FALSE,
                         csv_path = ".",
                         override_csv = FALSE,
                         database_connection = NULL,
                         tibble_name) {
  check_parameters_validation(save_table, database_connection)
  path <-
    get_dataset_full_path(tibble_name, csv_path)
  if (!override_csv & file.exists(path)) {
    polypeptides_tbl <- readr::read_csv(path)
  } else {
    cett_type <- strsplit(tibble_name, "_")[[1]][1]
    polypeptides_tbl <-
      map_df(pkg_env$children,
             ~ polypeptides_parser(., cett_type)) %>% unique()

    write_csv(polypeptides_tbl, save_csv, csv_path)
  }


  if (save_table) {
    save_drug_sub(
      con = database_connection,
      df = polypeptides_tbl,
      table_name = tibble_name,
      save_table_only = TRUE,
      field_types = list(
        general_function =
          paste("varchar(",
                max(
                  nchar(polypeptides_tbl$general_function),
                  na.rm = TRUE
                ), ")",
                sep = ""),
        specific_function =
          paste("varchar(",
                max(
                  nchar(polypeptides_tbl$specific_function),
                  na.rm = TRUE
                ), ")",
                sep = ""),
        amino_acid_sequence =
          paste("varchar(",
                max(
                  nchar(polypeptides_tbl$amino_acid_sequence),
                  na.rm = TRUE
                ), ")",
                sep = ""),
        gene_sequence = paste("varchar(max)", sep = "")
      )
    )
  }
  return(polypeptides_tbl %>% as_tibble())
}

#' @rdname cett_poly_doc
#' @export
carriers_polypeptides <- function(save_table = FALSE,
                                  save_csv = FALSE,
                                  csv_path = ".",
                                  override_csv = FALSE,
                                  database_connection = NULL) {
  polypeptides(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "carriers_polypeptides"
  )
}

#' @rdname cett_poly_doc
#' @export
enzymes_polypeptides <- function(save_table = FALSE,
                                 save_csv = FALSE,
                                 csv_path = ".",
                                 override_csv = FALSE,
                                 database_connection = NULL) {
  polypeptides(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "enzymes_polypeptides"
  )
}

#' @rdname cett_poly_doc
#' @export
targets_polypeptides <- function(save_table = FALSE,
                                 save_csv = FALSE,
                                 csv_path = ".",
                                 override_csv = FALSE,
                                 database_connection = NULL) {
  polypeptides(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "targets_polypeptides"
  )
}

#' @rdname cett_poly_doc
#' @export
transporters_polypeptides <- function(save_table = FALSE,
                                      save_csv = FALSE,
                                      csv_path = ".",
                                      override_csv = FALSE,
                                      database_connection = NULL) {
  polypeptides(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
    "transporters_polypeptides"
  )
}
