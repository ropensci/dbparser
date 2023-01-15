PharmacologyParser <- R6::R6Class(
  "PharmacologyParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      pb <-  progress_bar$new(total = xmlSize(pkg_env$root))
      drugs_pharmacology <- xmlSApply(xmlRoot(pkg_env$root),
                                      private$drug_pharmacology_rec, pb)
      return(as_tibble(t(drugs_pharmacology)))
    },
    drug_pharmacology_rec = function(drug, pb) {
      pb$tick()
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
  )
)

#' Drug Pharmacology parser
#'
#' Describes the use, mechanism of action, pharmacokinetics, pharmacodynamics,
#'  and physiological or biochemical effects in the body.
#'
#' @inheritSection run_all_parsers read_drugbank_xml_db
#' @inheritParams run_all_parsers
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{indication}{The approved conditions, diseases, or states for which a
#'  drug can safely and effectively be used. An indication is considered to be
#'  FDA-approved when it has any of the following designations: NDA, ANDA, BLA,
#'   or OTC. May also include indications in other countries, such as Canada
#'   (through Health Canada) or in Europe
#'   (through the European Medicines Agency).}
#'  \item{pharmacodynamics}{A description of how the drug modifies or affects
#'  the organism it is being used in. May include effects in the body that are
#'   desired (enzyme or protein targets for example) and undesired
#'   (also known as “side effects”). This is in contrast to pharmacokinetics,
#'    which describes how the body modifies the drug being used.}
#'  \item{mechanism_of_action}{A component of pharmacodynamics that describes
#'  the biochemical interaction through which a drug produces its intended
#'  effect. May include the exact molecular protein or enzyme targets and/or
#'  a description of the physiological effects produced.}
#'  \item{toxicity}{Any adverse reaction, or side effect, that may or may not
#'  occur with use of the drug. May be attributed to a number of effects
#'  including: an enhanced therapeutic effect, rare anaphylactic reactions,
#'   interactions with other medications, or unanticipated binding of the
#'   molecule at different sites within the body.}
#'  \item{metabolism}{A description of the chemical degradation of the drug
#'  molecule within the body; most commonly by enzymes from the
#'  Cytochrome P450 (CYP) system in the liver.}
#'  \item{absorption}{A description of the movement of the drug from the site
#'   of administration into the bloodstream or target tissue. Common
#'   pharmacokinetic metrics used to evaluate absorption include Area Under
#'   the Curve (AUC), bioavailability (F), maximum concentration (Cmax), and
#'   time to maximum concentration (Tmax).}
#'  \item{half-life}{The period of time it takes for the amount of drug in the
#'  body to be reduced by one half. Provides a description of how quickly the
#'  drug is being eliminated and how much is available in the bloodstream.}
#'  \item{protein-binding	}{A description of the drug’s affinity for plama
#'  proteins and the proportion of the drug that is bound to them when in
#'  circulation within the body.}
#'  \item{route_of_elimination}{A description of the pathway that is used to
#'  excrete the drug from the body. Common pharmacokinetic parameters used to
#'  evaluate excretion include elemination half life, renal clearance, and
#'  tracking of radiolabelled compounds through the renal and GI system.}
#'  \item{volume_of_distribution}{The Vd of a drug represents the degree to
#'  which it is distributed into body tissue compared to the plasma.}
#'  \item{clearance}{A pharmacokinetic measurement of the rate of removal of the
#'   drug from plasma, expressed as mL/min; reflects the rate of elimination of
#'    the drug.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit run_all_parsers examples
#' @export

drug_pharmacology <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE) {
    PharmacologyParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      "drugs_pharmacology"
    )$parse()
  }
