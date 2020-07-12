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
      if (nrow(parsed_tbl) > 0) {
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
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{group}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_groups <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_groups",
      main_node = "groups"
    )$parse()
  }

#' Drug Products parser
#'
#' A list of commercially available products in Canada and the United States
#'  that contain the drug.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
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
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_products <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
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
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
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
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_calc_prop <- function(save_table = FALSE,
                           save_csv = FALSE,
                           csv_path = ".",
                           override_csv = FALSE,
                           database_connection = NULL) {
  DrugElementsParser$new(
    save_table,
    save_csv,
    csv_path,
    override_csv,
    database_connection,
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
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 4 variables:
#' \describe{
#'  \item{brand}{The proprietary, well-known name for given to this drug by a
#'  manufacturer.}
#'  \item{company}{The company or manufacturer that uses this name.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_intern_brand <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
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
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
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
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_salts <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_salts",
      main_node = "salts"
    )$parse()
  }

#' Drug Mixtures parser
#'
#' All commercially available products in which this drug is available in
#' combination with other drug molecules
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
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
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_mixtures <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_mixtures",
      main_node = "mixtures"
    )$parse()
  }

#' Drug Packagers parser
#'
#' A list of companies that are packaging the drug for re-distribution.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{name}{}
#'  \item{url}{A link to any companies that are packaging the drug for
#'  re-distribution.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_packagers <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_packagers",
      main_node = "packagers"
    )$parse()
  }


#' Drug Categories parser
#'
#' General categorizations of the drug.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{category}{category name}
#'  \item{mesh-id}{The Medical Subjects Headings (MeSH) identifier for the
#'  category.}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_categories <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_categories",
      main_node = "categories"
    )$parse()
  }

#' Drug Affected Organism parser
#'
#' Organisms in which the drug may display activity; activity may depend on
#' local susceptibility patterns and resistance.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with 2 variables:
#' \describe{
#'  \item{affected-organism}{affected-organism name}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_affected_organisms <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_affected_organisms",
      main_node = "affected-organisms"
    )$parse()
  }

#' Drug Dosages parser
#'
#' A list of the commercially available dosages of the drug.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{form}{The pharmaceutical formulation by which the drug is introduced
#'  into the body}
#'  \item{route}{The path by which the drug or product is taken into the body.}
#'  \item{strength}{The amount of active drug ingredient provided in the dosage}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_dosages <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_dosages",
      main_node = "dosages"
    )$parse()
  }


#' Drug ahfs-codes parser
#'
#' The American Hospital Formulary Service (AHFS) identifier for this drug.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{ahfs-code}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_ahfs_codes <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_ahfs_codes",
      main_node = "ahfs-codes"
    )$parse()
  }

#' Drug pdb-entries parser
#'
#' Protein Data Bank (PDB) identifiers for this drug.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{pdb-entry}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_pdb_entries <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
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
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
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
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_patents <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_patents",
      main_node = "patents"
    )$parse()
  }

#' Drug Groups parser
#'
#' Food that may interact with this drug.
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return  a tibble with the following variables:
#' \describe{
#'  \item{food-interaction}{}
#'  \item{\emph{drugbank_id}}{drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_food_interactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
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
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
#'
#' @return a tibble with the following variables:
#' \describe{
#'  \item{drugbank-id	}{Drugbank ID of the interacting drug.}
#'  \item{name}{Name of the interacting drug.}
#'  \item{description}{Textual description of the physiological consequences
#'  of the drug interaction}
#'  \item{\emph{drugbank_id}}{parent drugbank id}
#' }
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_interactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_drug_interactions",
      main_node = "drug-interactions"
    )$parse()
  }

#' Drug Experimental Properties parser
#'
#' Drug properties that have been experimentally proven
#'
#' @inheritSection drug_all read_drugbank_xml_db
#' @inheritParams drug_all
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
#' @family drugs
#'
#' @inherit drug_all examples
#' @export
drug_exp_prop <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    DrugElementsParser$new(
      save_table,
      save_csv,
      csv_path,
      override_csv,
      database_connection,
      "drug_experimental_properties",
      main_node = "experimental-properties"
    )$parse()
  }

#' Extracts the drug external identifiers element and return data as tibble.
#'
#' \code{drug_ex_identity} returns tibble of external
#' identifiers groups elements.
#'
#' This functions extracts the external identifiers element of drug node in
#' drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug external identifiers node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_ex_identity()
#'
#' # will throw an error, as database_connection is NULL
#' drug_ex_identity(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_ex_identity(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_ex_identity(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_ex_identity(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_ex_identity <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("drug_external_identifiers", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_external_identifiers <- readr::read_csv(path)
    } else {
      drug_external_identifiers <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "external-identifiers"))

      write_csv(drug_external_identifiers, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_external_identifiers,
                    table_name = "drug_external_identifiers")
    }
    return(drug_external_identifiers %>% as_tibble())
  }

#' Extracts the drug external links element and return data as tibble.
#'
#' \code{drug_external_links} returns tibble of drug external links
#' elements.
#'
#' This functions extracts the external links element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#'  location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug external links node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_external_links()
#'
#' # will throw an error, as database_connection is NULL
#' drug_external_links(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_external_links(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location and
#' # return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_external_links(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_external_links(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_external_links <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_external_links", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_external_links <- readr::read_csv(path)
    } else {
      drug_external_links <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "external-links")) %>%
        unique()

      write_csv(drug_external_links, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_external_links,
                    table_name = "drug_external_links")
    }
    return(drug_external_links %>% as_tibble())
  }

#' Extracts the drug snp effects element and return data as tibble.
#'
#' \code{drug_snp_effects} returns tibble of snp effects groups elements.
#'
#' This functions extracts the snp effects element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#' \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug snp effects node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_snp_effects()
#'
#' # will throw an error, as database_connection is NULL
#' drug_snp_effects(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_snp_effects(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_effects(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_snp_effects(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_snp_effects <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drug_snp_effects", csv_path)
    if (!override_csv & file.exists(path)) {
      drug_snp_effects <- readr::read_csv(path)
    } else {
      drug_snp_effects <-
        map_df(pkg_env$children, ~ drug_sub_df(.x, "snp-effects")) %>% unique()

      write_csv(drug_snp_effects, save_csv, csv_path)
    }


    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = drug_snp_effects,
                    table_name = "drug_snp_effects")
    }
    return(drug_snp_effects %>% as_tibble())
  }

#' Extracts the drug snp adverse drug reactions element and return data as
#' tibble.
#'
#' \code{drug_snp_adverse_reactions } returns tibble of drug
#'  snp adverse drug reactions elements.
#'
#' This functions extracts the groups element of drug node in drugbank
#' xml database with the option to save it in a predefined database via
#' passed database connection. It takes two optional arguments to
#' save the returned tibble in the database \code{save_table} and
#'  \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug snp adverse drug reactions node attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_snp_adverse_reactions()
#'
#' # will throw an error, as database_connection is NULL
#' drug_snp_adverse_reactions(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_snp_adverse_reactions(save_table = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist in
#' # current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' # and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_snp_adverse_reactions(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current location
#' # and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_snp_adverse_reactions(
#'   save_csv = TRUE, csv_path = TRUE,
#'   override = TRUE
#' )
#' }
#' @export
drug_snp_adverse_reactions <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <-
      get_dataset_full_path("snp_adverse_reactions", csv_path)
    if (!override_csv & file.exists(path)) {
      snp_adverse_reactions <- readr::read_csv(path)
    } else {
      snp_adverse_reactions <-
        map_df(pkg_env$children,
               ~ drug_sub_df(.x, "snp-adverse-drug-reactions")) %>% unique()

      write_csv(snp_adverse_reactions, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(con = database_connection,
                    df = snp_adverse_reactions,
                    table_name = "snp_adverse_reactions")
    }
    return(snp_adverse_reactions %>% as_tibble())
  }

# Extract drug Pharmacology
drug_pharmacology_rec <- function(drug) {
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


#' Extracts the drug_pharmacology elements and return data as tibble.
#'
#' \code{drug_pharmacology} returns tibble of drug_pharmacologys main elements.
#'
#' This functions extracts the main element of drug_pharmacology node in
#' drugbank
#' xml database with the option to save it in a user defined database.
#' It takes two optional arguments to save the returned tibble in the database
#' \code{save_table} and \code{database_connection}.
#' It must be called after \code{\link{read_drugbank_xml_db}} function like
#' any other parser function.
#' If \code{\link{read_drugbank_xml_db}} is called before for any reason, so
#' no need to call it again before calling this function.
#'
#' @param save_table boolean, save table in database if true.
#' @param save_csv boolean, save csv version of parsed tibble if true
#' @param csv_path location to save csv files into it, default is current
#' location, save_csv must be true
#' @param override_csv override existing csv, if any, in case it is true in the
#'  new parse operation
#' @param database_connection DBI connection object that holds a connection to
#' user defined database. If \code{save_table} is enabled without providing
#' value for this function an error will be thrown.
#' @return drug_pharmacology attributes tibble
#' @family drugs
#' @examples
#' \dontrun{
#' # return only the parsed tibble
#' drug_pharmacology()
#'
#' # will throw an error, as database_connection is NULL
#' drug_pharmacology(save_table = TRUE)
#'
#' # save in database in SQLite in memory database and return parsed tibble
#' sqlite_con <- DBI::dbConnect(RSQLite::SQLite())
#' drug_pharmacology(save_table = TRUE, database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pharmacology(save_csv = TRUE)
#'
#' # save in database, save parsed tibble as csv if it does not exist
#' # in current location and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pharmacology(save_table = TRUE, save_csv = TRUE,
#'  database_connection = sqlite_con)
#'
#' # save parsed tibble as csv if it does not exist in given location
#' #  and return parsed tibble.
#' # If the csv exist before read it and return its data.
#' drug_pharmacology(save_csv = TRUE, csv_path = TRUE)
#'
#' # save parsed tibble as csv if it does not exist in current
#' # location and return parsed tibble.
#' # If the csv exist override it and return it.
#' drug_pharmacology(save_csv = TRUE, csv_path = TRUE, override = TRUE)
#' }
#' @export
drug_pharmacology <-
  function(save_table = FALSE,
           save_csv = FALSE,
           csv_path = ".",
           override_csv = FALSE,
           database_connection = NULL) {
    check_parameters_validation(save_table, database_connection)
    path <- get_dataset_full_path("drugs_pharmacology", csv_path)
    if (!override_csv & file.exists(path)) {
      drugs_pharmacology <- readr::read_csv(path)
    } else {
      drugs_pharmacology <- xmlSApply(xmlRoot(pkg_env$root),
                                      drug_pharmacology_rec)
      drugs_pharmacology <- as_tibble(t(drugs_pharmacology))
      write_csv(drugs_pharmacology, save_csv, csv_path)
    }

    if (save_table) {
      save_drug_sub(
        con = database_connection,
        df = drugs_pharmacology,
        table_name = "drug_pharmacology",
        primary_key = "drugbank_id",
        foreign_key = NULL,
        field_types = list(
          drugbank_id = paste0("varchar(", max(nchar(
            drugs_pharmacology$drugbank_id
          )), ")"),
          mechanism_of_action = "varchar(MAX)",
          pharmacodynamics = "varchar(MAX)",
          indication = paste0("varchar(", max(nchar(
            drugs_pharmacology$indication
          ), na.rm = TRUE) + 10, ")"),
          absorption = paste0("varchar(", max(nchar(
            drugs_pharmacology$absorption
          ), na.rm = TRUE) + 10, ")"),
          route_of_elimination = paste0("varchar(", max(
            nchar(drugs_pharmacology$route_of_elimination),
            na.rm = TRUE
          ) + 10, ")"),
          metabolism = paste0("varchar(", max(nchar(
            drugs_pharmacology$metabolism
          ), na.rm = TRUE) + 10, ")"),
          clearance = paste0("varchar(", max(nchar(
            drugs_pharmacology$clearance
          ), na.rm = TRUE) + 10, ")"),
          half_life = paste0("varchar(", max(nchar(
            drugs_pharmacology$half_life
          ), na.rm = TRUE) + 10, ")"),
          volume_of_distribution = paste0("varchar(", max(
            nchar(drugs_pharmacology$volume_of_distribution),
            na.rm = TRUE
          ) + 10, ")"),
          protein_binding = paste0("varchar(", max(
            nchar(drugs_pharmacology$protein_binding),
            na.rm = TRUE
          ) + 10, ")"),
          toxicity = "varchar(MAX)"
        )
      )
    }

    return(drugs_pharmacology %>% as_tibble())
  }
