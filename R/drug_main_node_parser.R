DrugParser <- R6::R6Class(
  "DrugParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      pb <-  progress_bar$new(total = xmlSize(pkg_env$root))
      drugs <-
        xmlSApply(xmlRoot(pkg_env$root), private$drug_row, pb)
      as_tibble(t(drugs))
    },
    drug_row = function(drug, pb) {
      pb$tick()
      d_elements <- names(drug)
      ids <- drug[d_elements == "drugbank-id"]
      ids_length <- length(ids)
      ids <- map_chr(ids, xmlValue)
      c(
        primary_key = ids[[1]],
        other_keys = ifelse(ids_length > 1,
                            paste(unlist(ids[2:ids_length]), collapse = ";"),
                            NA),
        type = xmlGetAttr(node = drug, name = "type"),
        created = xmlGetAttr(node = drug, name = "created"),
        updated = xmlGetAttr(node = drug, name = "updated"),
        name = xmlValue(drug[["name"]]),
        description = xmlValue(drug[["description"]]),
        cas_number = xmlValue(drug[["cas-number"]]),
        unii = xmlValue(drug[["unii"]]),
        average_mass = xmlValue(drug[["average-mass"]]),
        monoisotopic_mass = xmlValue(drug[["monoisotopic-mass"]]),
        state = xmlValue(drug[["state"]]),
        synthesis_reference = xmlValue(drug[["synthesis-reference"]]),
        fda_label = xmlValue(drug[["fda-label"]]),
        msds = xmlValue(drug[["msds"]])
      )
    }
  )
)

#' Drugs General Information parser
#'
#' A description of the hierarchical chemical classification of the drug;
#' imported from ClassyFire.
#'
#'
#' @return  a tibble with 15 variables:
#' \describe{
#'   \item{primary_key}{DrugBank id}
#'   \item{other_keys}{Other identifiers that may be associated with the drug}
#'   \item{type}{	Either small molecule, or biotech. Biotech is used for any
#'   drug that is derived from living systems or organisms, usually composed of
#'    high molecular weight mixtures of protein, while small molecule describes
#'     a low molecular weight organic compound.}
#'   \item{name}{}
#'   \item{created}{Date that this drug was first added to DrugBank.}
#'   \item{updated}{Denotes when this drug was last updated in DrugBank.}
#'   \item{description}{Descriptions of drug chemical properties,
#'    history and regulatory status.}
#'   \item{cas_number}{The Chemical Abstracts Service (CAS) registry number
#'    assigned to the drug.}
#'   \item{\emph{unii}}{Unique Ingredient Identifier (UNII) of this drug.}
#'   \item{average_mass}{The weighted average of the isotopic masses of the
#'   drug}
#'   \item{state}{One of solid, liquid, or gas}
#'   \item{monoisotopic_mass}{The mass of the most abundant isotope of the drug}
#'   \item{synthesis_reference}{Citation for synthesis of the drug molecule.}
#'   \item{fda_label}{Contains a URL for accessing the uploaded United States Food
#'   and Drug Administration (FDA) Monograph for this drug.}
#'   \item{msds}{Contains a URL for accessing the Material Safety Data Sheet
#'   (MSDS) for this drug.}
#' }
#' @keywords internal
drug_general_information <- function() {
    DrugParser$new("drug")$parse()
  }
