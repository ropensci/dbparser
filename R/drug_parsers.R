DrugElementsParser <- R6::R6Class(
  "DrugElementsParser",
  inherit = AbstractParser,
  private = list(
    parse_record = function() {
      drugs <-  xmlChildren(pkg_env$root)
      pb <- progress_bar$new(total = xmlSize(drugs))
      parsed_tbl <- map_df(drugs, ~ drug_sub_df(.x, private$main_node,
                                                progress = pb)) %>%
        unique()
      if (NROW(parsed_tbl) > 0) {
        switch(
          private$main_node,
          "groups" = names(parsed_tbl) <- c("group", "drugbank-id"),
          "international-brands" = names(parsed_tbl) <-
            c("brand", "company","drugbank-id"),
          "affected-organisms" = names(parsed_tbl) <-
            c("affected_organism", "drugbank_id"),
          "ahfs-codes" = names(parsed_tbl) <- c("ahfs_code", "drugbank_id"),
          "pdb-entries" = names(parsed_tbl) <- c("pdb_entry", "drugbank_id"),
          "food-interactions" = names(parsed_tbl) <-
            c("food_interaction", "drugbank_id")
        )
      }
      return(parsed_tbl)
    }
  )
)

#' Drug Groups parser
#'
#' Groups that this drug belongs to. May include any of: approved, vet_approved,
#'  nutraceutical, illicit, withdrawn, investigational, and experimental.
#'
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{group}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_groups <- function() {
    DrugElementsParser$new(
      "drug_groups",
      main_node = "groups"
    )$parse()
  }

#' Drug Products parser
#'
#' A list of commercially available products in Canada and the United States
#'  that contain the drug.
#'
#'
#' @return  a tibble with 32 variables:
#' \describe{
#'  \item{name}{The proprietary name(s) provided by the manufacturer for any
#'  commercially available products containing this drug.}
#'  \item{labeller}{The corporation responsible for labelling this product.}
#'  \item{ndc-id}{The National Drug Code (NDC) identifier of the drug}
#'  \item{ndc-product-code}{The National Drug Code (NDC) product code from the
#'   FDA National Drug Code directory.}
#'  \item{dpd-id}{Drug Product Database (DPD) identification number (a.k.a. DIN)
#'   from the Canadian Drug Product Database. Only present for drugs that are
#'   marketed in Canada}
#'  \item{ema-product-code}{EMA product code from the European Medicines Agency
#'  Database. Only present for products that are authorised by central procedure
#'   for marketing in the European Union.}
#'  \item{ema-ma-number}{EMA marketing authorisation number from the European
#'  Medicines Agency Database. Only present for products that are authorised by
#'   central procedure for marketing in the European Union.}
#'  \item{started-marketing-on}{The starting date for market approval.}
#'  \item{ended-marketing-on}{The ending date for market approval.}
#'  \item{dosage-form	}{The pharmaceutical formulation by which the drug is
#'  introduced into the body.}
#'  \item{strength}{The amount of active drug ingredient provided in the dosage}
#'  \item{route}{The path by which the drug or product is taken into the body}
#'  \item{fda-application-number}{The New Drug Application [NDA] number
#'  assigned to this drug by the FDA.}
#'  \item{over-the-counter}{A list of Over The Counter (OTC) forms of the drug.}
#'  \item{generic}{Whether this product is a generic drug.}
#'  \item{approved}{Indicates whether this drug has been approved by the
#'  regulating government.}
#'  \item{country}{The country where this commercially available drug has been
#'  approved.}
#'  \item{source}{Source of this product information. For example, a value of
#'  DPD indicates this information was retrieved from the Canadian Drug Product
#'   Database.}
#'  \item{standing}{One of good, discordant, or deprecated. Distinguishes
#'  products with up to date ingredient information (good) from products with
#'  conflicting information (discordant) or products that have been removed from
#'   an active label (deprecated).}
#'  \item{standing-updated-on}{The date on which the standing was last updated}
#'  \item{standing-reason}{Explains the non-good standing of the product.
#'  One of: ingredient_change, code_duplication, invalid, or removed.}
#'  \item{jurisdiction-marketing-category	}{The marketing category of this
#'  product in its jurisdiction}
#'  \item{branded}{Whether this product has a named brand}
#'  \item{prescription}{Whether this product is only available with
#'  a prescription}
#'  \item{unapproved}{Whether this product is not approved in its jurisdiction}
#'  \item{vaccine}{Whether this product is a vaccine}
#'  \item{allergenic}{Whether this product is used in allergenic testing}
#'  \item{cosmetic}{Whether this product is a cosmetic, such as sunscreen}
#'  \item{kit}{Whether this product is a kit composed of multiple distinct
#'  parts}
#'  \item{solo}{Whether this product has only a single active ingredient}
#'  \item{available}{Whether this product can be sold in its jurisdiction}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_products <- function() {
    DrugElementsParser$new(
      "drug_products",
      main_node = "products"
    )$parse()
  }

#' Drug Calculated Properties parser
#'
#' Drug properties that have been predicted by ChemAxon or ALOGPS based on the
#' inputed chemical structure. Associated links below will redirect to
#' descriptions of the specific term.
#'
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'  \item{kind}{Name of the property.}
#'  \item{value}{Predicted physicochemical properties; obtained by the use of
#'  prediction software such as ALGOPS and ChemAxon.}
#'  \item{source}{Name of the software used to calculate this property,
#'  either ChemAxon or ALOGPS.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_calc_prop <- function() {
  DrugElementsParser$new(
    "drug_calculated_properties",
    main_node = "calculated-properties"
  )$parse()
}

#' Drug International Brands parser
#'
#' The proprietary names used by the manufacturers for commercially available
#' forms of the drug, focusing on brand names for products that are available
#' in countries other than Canada and the Unites States.
#'
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'  \item{brand}{The proprietary, well-known name for given to this drug by a
#'  manufacturer.}
#'  \item{company}{The company or manufacturer that uses this name.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_intern_brand <- function() {
    DrugElementsParser$new(
      "drug_international_brands",
      main_node = "international-brands"
    )$parse()
  }

#' Drug Salts parser
#'
#' Available salt forms of the drug. Ions such as hydrochloride, sodium,
#'  and sulfate are often added to the drug molecule to increase solubility,
#'  dissolution, or absorption.
#'
#'
#' @return  a tibble with 1 variables:
#' \describe{
#'  \item{drugbank-id}{DrugBank identfiers of the available salt form(s).}
#'  \item{name}{Name of the available salt form(s)}
#'  \item{unii}{Unique Ingredient Identifier (UNII) of the available salt
#'  form(s).}
#'  \item{cas-number}{Chemical Abstracts Service (CAS) registry number assigned
#'   to the salt form(s) of the drug.}
#'  \item{inchikey}{IUPAC International Chemical Identifier (InChi) key
#'  identfier for the available salt form(s).}
#'  \item{average-mass}{Average molecular mass: the weighted average of the
#'   isotopic masses of the salt.}
#'  \item{monoisotopic-mass}{The mass of the most abundant isotope of the salt}
#'  \item{smiles}{The simplified molecular-input line-entry system (SMILES) is
#'  a line notation used for describing the structure of chemical species using
#'   short ASCII strings; calculated by ChemAxon.}
#'  \item{inchi}{A prediction of the IUPAC
#'  International Chemical Identifier (InChI); imported by ChemAxon.}
#'  \item{formula}{Indicates the simple numbers of each type of atom within the
#'   molecule; calculated by ChemAxon.}
#'  \item{\emph{drugbank_id}}{parent drugbank id}
#' }
#' @keywords internal
drug_salts <- function() {
    DrugElementsParser$new(
      "drug_salts",
      main_node = "salts"
    )$parse()
  }

#' Drug Mixtures parser
#'
#' All commercially available products in which this drug is available in
#' combination with other drug molecules
#'
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'  \item{name}{The proprietary name provided by the manufacturer for this
#'  combination product.}
#'  \item{ingredients}{A list of ingredients, separated by addition symbols}
#'  \item{supplemental-ingredients}{List of additional active ingredients which
#'   are not clinically relevant to the main indication of the product,
#'   separated by addition symbols.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_mixtures <- function() {
    DrugElementsParser$new(
      "drug_mixtures",
      main_node = "mixtures"
    )$parse()
  }

#' Drug Packagers parser
#'
#' A list of companies that are packaging the drug for re-distribution.
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{name}{}
#'  \item{url}{A link to any companies that are packaging the drug for
#'  re-distribution.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_packagers <- function() {
    DrugElementsParser$new(
      "drug_packagers",
      main_node = "packagers"
    )$parse()
  }


#' Drug Categories parser
#'
#' General categorizations of the drug.
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{category}{category name}
#'  \item{mesh-id}{The Medical Subjects Headings (MeSH) identifier for the
#'  category.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_categories <- function() {
    DrugElementsParser$new(
      "drug_categories",
      main_node = "categories"
    )$parse()
  }

#' Drug Affected Organism parser
#'
#' Organisms in which the drug may display activity; activity may depend on
#' local susceptibility patterns and resistance.
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{affected-organism}{affected-organism name}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_affected_organisms <- function() {
    DrugElementsParser$new(
      "drug_affected_organisms",
      main_node = "affected-organisms"
    )$parse()
  }

#' Drug Dosages parser
#'
#' A list of the commercially available dosages of the drug.
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{form}{The pharmaceutical formulation by which the drug is introduced
#'  into the body}
#'  \item{route}{The path by which the drug or product is taken into the body.}
#'  \item{strength}{The amount of active drug ingredient provided in the dosage}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_dosages <- function() {
    DrugElementsParser$new(
      "drug_dosages",
      main_node = "dosages"
    )$parse()
  }


#' Drug ahfs-codes parser
#'
#' The American Hospital Formulary Service (AHFS) identifier for this drug.
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{ahfs-code}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_ahfs_codes <- function() {
    DrugElementsParser$new(
      "drug_ahfs_codes",
      main_node = "ahfs-codes"
    )$parse()
  }

#' Drug pdb-entries parser
#'
#' Protein Data Bank (PDB) identifiers for this drug.
#'
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{pdb-entry}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_pdb_entries <- function() {
    DrugElementsParser$new(
      "drug_pdb_entries",
      main_node = "pdb-entries"
    )$parse()
  }

#' Drug Patents parser
#' A property right issued by the United States Patent and Trademark
#' Office (USPTO) to an inventor for a limited time, in exchange for public
#' disclosure of the invention when the patent is granted. Drugs may be issued
#' multiple patents.
#'
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{number}{The patent number(s) associated with the drug.}
#'  \item{country}{The country that issued the patent rights.}
#'  \item{approved}{The date that the patent request was filed.}
#'  \item{expires}{The date that the patent rights expire.}
#'  \item{pediatric-extension}{Indicates whether or not a pediatric extension has been approved for
#'   the patent. Granted pediatric extensions provide an additional 6 months of
#'   market protection.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_patents <- function() {
    DrugElementsParser$new(
      "drug_patents",
      main_node = "patents"
    )$parse()
  }

#' Drug Groups parser
#'
#' Food that may interact with this drug.
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{food-interaction}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_food_interactions <- function() {
    DrugElementsParser$new(
      "drug_food_interactions",
      main_node = "food-interactions"
    )$parse()
  }

#' Drug Interactions parser
#'
#' Drug-drug interactions detailing drugs that, when administered concomitantly
#' with the drug of interest, will affect its activity or result in adverse
#' effects. These interactions may be synergistic or antagonistic depending on
#' the physiological effects and mechanism of action of each drug.
#'
#' @return a tibble with the following variables:
#' \describe{
#'  \item{drugbank-id	}{Drugbank ID of the interacting drug.}
#'  \item{name}{Name of the interacting drug.}
#'  \item{description}{Textual description of the physiological consequences
#'  of the drug interaction}
#'  \item{\emph{drugbank_id}}{parent drugbank id}
#' }
#' @keywords internal
drug_interactions <- function() {
    DrugElementsParser$new(
      "drug_drug_interactions",
      main_node = "drug-interactions"
    )$parse()
  }

#' Drug Experimental Properties parser
#'
#' Drug properties that have been experimentally proven
#'
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{kind}{Name of the property.}
#'  \item{value}{Drug properties that have been experimentally proven.}
#'  \item{source}{Reference to the source of this experimental data.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#'
#' The following experimental properties are provided:
#' \describe{
#'  \item{Water Solubility}{The experimentally determined aqueous solubility
#'  of the molecule.}
#'  \item{Molecular Formula}{Protein formula of Biotech drugs}
#'  \item{Molecular Weight}{Protein weight of Biotech drugs.}
#'  \item{Melting Point}{The experimentally determined temperature at which the
#'   drug molecule changes from solid to liquid at atmospheric temperature.}
#'  \item{Boiling Point}{The experimentally determined temperature at which the
#'   drug molecule changes from liquid to gas at atmospheric temperature.}
#'  \item{Hydrophobicity}{The ability of a molecule to repel water rather than
#'  absorb or dissolve water.}
#'  \item{Isoelectric Point}{The pH value at which the net electric charge of a
#'  molecule is zero.}
#'  \item{caco2 Permeability}{A continuous line of heterogenous human epithelial
#'   colorectal adenocarcinoma cells, CAC02 cells are employed as a model of
#'   human intestinal absorption of various drugs and compounds. CAC02 cell
#'   permeability is ultimately an assay to measure drug absorption.}
#'  \item{pKa}{The experimentally determined pka value of the molecule}
#'  \item{logP}{The experimentally determined partition coefficient (LogP)
#'  based on the ratio of solubility of the molecule in 1-octanol compared to
#'  water.}
#'  \item{logS}{The intrinsic solubility of a given compound is the
#'  concentration in equilibrium with its solid phase that dissolves into
#'   solution, given as the natural logarithm (LogS) of the concentration.}
#'  \item{Radioactivity}{The property to spontaneously emit particles
#'  (alpha, beta, neutron) or radiation (gamma, K capture), or both at the same
#'  time, from the decay of certain nuclides.}
#' }
#'
#' @keywords internal
drug_exp_prop <- function() {
    DrugElementsParser$new(
      "drug_experimental_properties",
      main_node = "experimental-properties"
    )$parse()
  }

#' Drug External Identifiers parser
#'
#' Identifiers used in other websites or databases providing information about
#' this drug.
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{resource}{Name of the source database.}
#'  \item{identifier}{Identifier for this drug in the given resource.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_ex_identity <- function() {
    DrugElementsParser$new(
      "drug_external_identifiers",
      main_node = "external-identifiers"
    )$parse()
  }

#' Drug External Links parser
#'
#' Links to other websites or databases providing information about this drug.
#'
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{resource}{Name of the source website.}
#'  \item{identifier}{Identifier for this drug in the given resource}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_external_links <- function() {
    DrugElementsParser$new(
      "drug_external_links",
      main_node = "external-links"
    )$parse()
  }

#' Drug SNP Effects parser
#'
#' A list of single nucleotide polymorphisms (SNPs) relevent to drug activity or
#'  metabolism, and the effects these may have on pharmacological activity.
#'  SNP effects in the patient may require close monitoring, an increase or
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{protein-name}{Proteins involved in this SNP.}
#'  \item{gene-symbol}{Genes involved in this SNP.}
#'  \item{uniprot-id}{Universal Protein Resource (UniProt) identifiers for
#'  proteins involved in this pathway.}
#'  \item{rs-id}{	The SNP Database identifier for this single nucleotide
#'  polymorphism.}
#'  \item{allele}{The alleles associated with the identified SNP.}
#'  \item{defining-change}{}
#'  \item{description}{A written description of the SNP effects.}
#'  \item{pubmed-id	}{Reference to PubMed article.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_snp_effects <- function() {
    DrugElementsParser$new(
      "drug_snp_effects",
      main_node = "snp-effects"
    )$parse()
  }

#' Drug SNP Adverse Drug Reactions parser
#'
#' The adverse drug reactions that may occur as a result of the listed single
#' nucleotide polymorphisms (SNPs)
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{protein-name}{Proteins involved in this SNP.}
#'  \item{gene-symbol}{Genes involved in this SNP.}
#'  \item{uniprot-id}{Universal Protein Resource (UniProt) identifiers for
#'  proteins involved in this pathway.}
#'  \item{rs-id}{The SNP Database identifier for this single nucleotide
#'   polymorphism.}
#'  \item{allele}{The alleles associated with the identified SNP.}
#'  \item{adverse-reaction}{}
#'  \item{description}{}
#'  \item{pubmed-id	}{Reference to PubMed article.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @keywords internal
drug_snp_adverse_reactions <- function() {
    DrugElementsParser$new(
      "snp_adverse_reactions",
      main_node = "snp-adverse-drug-reactions"
    )$parse()
  }
