context("test small molecule drug main attributes parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)


small_molecule <- "drugbank_record_small_molecule.xml"
test_that(desc = "Read database",
          code = {
            expect_true(get_xml_db_rows(
              system.file("extdata", small_molecule, package = "dbparser")
            ))
          })

test_that(desc = "Read drug primary key attribute",
          code = {
            expect_match(parse_drug()[1][["primary_key"]], "DB00006")
            expect_error(parse_drug(TRUE))
          })

test_that(desc = "Read drug other keys attribute",
          code = {
            expect_match(parse_drug()$other_keys, "BTD00076,EXPT03302,BIOD00076,DB02351")
            expect_error(parse_drug(TRUE))
          })

test_that(desc = "Read drug some small attributes",
          code = {
            expect_match(parse_drug()$average_mass, "2180.2853")
            expect_match(parse_drug()$monoisotopic_mass, "2178.985813062")
          })

test_that(desc = "Read drug groups attributes",
          code = {
            expect_match(as.character(parse_drug_groups()[1][["group"]][1]), "approved")
            expect_error(parse_drug_groups(TRUE))
          })

test_that(desc = "Read drug articles attributes",
          code = {
            expect_match(as.character(parse_drug_articles()[["pubmed-id"]][1]), "16466327")
            expect_error(parse_drug_articles(TRUE))
          })

test_that(desc = "Read drug books attributes",
          code = {
            expect_equal(nrow(parse_drug_books()), 0)
            expect_error(parse_drug_books(TRUE))
          })

test_that(desc = "Read drug links attributes",
          code = {
            expect_equal(nrow(parse_drug_links()), 0)
            expect_error(parse_drug_links(TRUE))
          })

test_that(desc = "Read drug classification attributes",
          code = {
            expect_match(parse_drug_classification()[1][["parent_key"]], "DB00006")
            expect_error(parse_drug_classification(TRUE))
          })

test_that(desc = "Read drug synonyms attributes",
          code = {
            expect_match(parse_drug_synonyms()[["synonym"]][[1]], "Bivalirudina")
            expect_error(parse_drug_synonyms(TRUE))
          })

test_that(desc = "Read drug articles attributes",
          code = {
            expect_match(as.character(parse_drug_articles()[["pubmed-id"]][1]), "16466327")
            expect_error(parse_drug_articles(TRUE))
          })

test_that(desc = "Read drug products attributes",
          code = {
            expect_match(as.character(parse_drug_products()[["name"]][1]), "Angiomax")
            expect_error(parse_drug_products(TRUE))
          })

test_that(desc = "Read drug calculated properties attributes",
          code = {
            expect_match(as.character(parse_drug_calculated_properties()[1][["kind"]][[1]]), "logP")
            expect_error(parse_drug_calculated_properties(TRUE))
          })

test_that(desc = "Read drug mixtures attributes",
          code = {
            expect_match(as.character(parse_drug_mixtures()[["name"]][1]), "Angiomax")
            expect_error(parse_drug_mixtures(TRUE))
          })

test_that(desc = "Read drug packagers attributes",
          code = {
            expect_match(as.character(parse_drug_packagers()[["name"]][1]), "Ben Venue Laboratories Inc.")
            expect_error(parse_drug_packagers(TRUE))
          })

test_that(desc = "Read drug manufacturers attributes",
          code = {
            expect_match(
              as.character(parse_drug_manufacturers()[["manufacturer"]][[1]]),
              "The medicines co"
            )
            expect_error(parse_drug_manufacturers(TRUE))
          })

test_that(desc = "Read drug prices attributes",
          code = {
            expect_match(as.character(parse_drug_prices()[["currency"]][[1]]),
                         "USD")
            expect_error(parse_drug_prices(TRUE))
          })

test_that(desc = "Read drug categories attributes",
          code = {
            expect_match(as.character(parse_drug_categories()[["mesh-id"]][[1]]),
                         "D000602")
            expect_error(parse_drug_categories(TRUE))
          })

test_that(desc = "Read drug affected organisms attributes",
          code = {
            expect_match(as.character(parse_drug_affected_organisms()[["affected_organism"]][[1]]),
                         "Humans and other mammals")
            expect_error(parse_drug_affected_organisms(TRUE))
          })

test_that(desc = "Read drug dosages attributes",
          code = {
            expect_match(as.character(parse_drug_dosages()[["route"]][[1]]),
                         "Intravenous")
            expect_error(parse_drug_dosages(TRUE))
          })

test_that(desc = "Read drug atc codes attributes",
          code = {
            expect_match(as.character(parse_drug_atc_codes()[["atc_code"]][[1]]),
                         "B01AE06")
            expect_error(parse_drug_atc_codes(TRUE))
          })

test_that(desc = "Read drug ahfs codes attributes",
          code = {
            expect_equal(nrow(parse_drug_ahfs_codes()),
                         1)
            expect_error(parse_drug_ahfs_codes(TRUE))
          })

test_that(desc = "Read drug pdb entries attributes",
          code = {
            expect_equal(nrow(parse_drug_pdb_entries()),
                         0)
            expect_error(parse_drug_pdb_entries(TRUE))
          })

test_that(desc = "Read drug patents attributes",
          code = {
            expect_match(as.character(parse_drug_patents()[["country"]][[1]]),
                         "United States")
            expect_error(parse_drug_patents(TRUE))
          })

test_that(desc = "Read drug interactions attributes",
          code = {
            expect_match(as.character(parse_drug_interactions()[["name"]][[1]]),
                         "St. John's Wort")
            expect_error(parse_drug_interactions(TRUE))
          })

test_that(desc = "Read drug food interactions attributes",
          code = {
            expect_equal(nrow(parse_drug_food_interactions()),
                         2)
            expect_error(parse_drug_food_interactions(TRUE))
          })

test_that(desc = "Read drug sequences attributes",
          code = {
            expect_equal(nrow(parse_drug_sequences()),
                         0)
            expect_error(parse_drug_sequences(TRUE))
          })

test_that(desc = "Read drug experimental properties attributes",
          code = {
            expect_equal(nrow(parse_drug_experimental_properties()),
                         0)
            expect_error(parse_drug_experimental_properties(TRUE))
          })

test_that(desc = "Read drug external identifiers attributes",
          code = {
            expect_match(
              as.character(parse_drug_external_identifiers()[["resource"]][[1]]),
              "Drugs Product Database \\(DPD\\)"
            )
            expect_error(parse_drug_external_identifiers(TRUE))
          })

test_that(desc = "Read drug external links attributes",
          code = {
            expect_match(as.character(parse_drug_external_links()[["resource"]][[1]]),
                         "RxList")
            expect_error(parse_drug_external_links(TRUE))
          })


test_that(desc = "Read drug snp effects attributes",
          code = {
            expect_equal(nrow(parse_drug_snp_effects()),
                         0)
            expect_error(parse_drug_snp_effects(TRUE))
          })

test_that(desc = "Read drug snp adverse drug reactions attributes",
          code = {
            expect_equal(nrow(parse_drug_snp_adverse_drug_reactions()),
                         0)
            expect_error(parse_drug_snp_adverse_drug_reactions(TRUE))
          })
