#' Parse the TWOSIDES Drug-Drug Interaction Database
#'
#' Reads the \href{https://tatonettilab-resources.s3.amazonaws.com/nsides/TWOSIDES.csv.gz}{TWOSIDES} data file, which contains adverse event data for pairs of
#' drugs taken concurrently (N=2 interactions).
#'
#' \href{https://tatonettilab-resources.s3.amazonaws.com/nsides/TWOSIDES.csv.gz}{TWOSIDES} is a database of drug-drug interaction safety signals
#' mined from the FDA's Adverse Event Reporting System using the same
#' approach as is used to generate OffSIDES.
#'
#' Database fields as follow:
#' \describe{
#' \item{drug_1_rxnorn_id}{RxNORM identifier for drug 1}
#' \item{drug_1_concept_name}{RxNORM name string for drug 1}
#' \item{drug_2_rxnorm_id}{RxNORM identifier for drug 2}
#' \item{drug_2_concept_name}{RxNORM name string for drug 3}
#' \item{condition_meddra_id}{MedDRA identifier for the side effect}
#' \item{condition_concpet_name}{MedDRA name string for the side effect}
#' \item{A}{The number of reports for the pair of drugs that report the side effect}
#' \item{B}{The number of reports for the pair of drugs that do not report the side effect}
#' \item{C}{The number of reports for other PSM matched drugs (including perhaps the single versions of drug 1 or drug 2) that report the side effect}
#' \item{D}{The number of reports for other PSM matched drugs and other side effects}
#' \item{PRR}{Proportional reporting ratio, PRR=(A/(A+B))/(C/(C+D))}
#' \item{PRR_error}{Error estimate of the PRR}
#' \item{mean_reporting_frequency}{Proportion of reports for the drug that report the side effect, A/(A+B)}
#' }
#'
#' @param twosides_file_path Path to the TWOSIDES data file (e.g., 'TWOSIDES.csv.gz').
#' @param db_version used twoside version (default = NULL)
#' @param db_exported_date used twoside release date (default = NULL)
#'
#' @return A dvobject of class `TWOSIDESDB` containing the `drug_drug_interactions`
#'   data frame and associated metadata.
#'
#' @export
#' @family parsers
#' @importFrom data.table fread
parseTWOSIDES <- function(twosides_file_path,
                          db_version       = NULL,
                          db_exported_date = NULL) {
  if (!file.exists(twosides_file_path)) {
    stop("TWOSIDES file not found at path: ", twosides_file_path)
  }

  message("Parsing TWOSIDES drug-drug interaction data...")
  twosides_data <- data.table::fread(twosides_file_path)
  message("Parsing complete.")

  twosides_db <- init_dvobject()
  twosides_db$drug_drug_interactions <- twosides_data

  twosides_db <- add_database_info(
    dvobject         = twosides_db,
    db_type          = "TWOSIDESDB",
    db_version       = db_version,
    db_exported_date = db_exported_date)

  twosides_db
}
