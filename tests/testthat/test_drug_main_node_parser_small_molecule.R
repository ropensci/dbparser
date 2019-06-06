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

test_that(desc = "Read darug main attributes",
          code = {
            expect_match(parse_drug()[1][["primary_key"]], "DB00006")
            expect_error(parse_drug(TRUE))
          })

test_that(desc = "Read darug groups attributes",
          code = {
            expect_match(as.character(parse_drug_groups()[1][["group"]][1]), "approved")
            expect_error(parse_drug_groups(TRUE))
          })

test_that(desc = "Read darug articles attributes",
          code = {
            expect_match(as.character(parse_drug_articles()[["pubmed-id"]][1]), "16466327")
            expect_error(parse_drug_articles(TRUE))
          })

test_that(desc = "Read darug books attributes",
          code = {
            expect_equal(nrow(parse_drug_books()), 0)
            expect_error(parse_drug_books(TRUE))
          })

test_that(desc = "Read darug links attributes",
          code = {
            expect_equal(nrow(parse_drug_links()), 0)
            expect_error(parse_drug_links(TRUE))
          })

test_that(desc = "Read darug classification attributes",
          code = {
            expect_match(parse_drug_classification()[1][["parent_key"]], "DB00006")
            expect_error(parse_drug_classification(TRUE))
          })

test_that(desc = "Read darug synonyms attributes",
          code = {
            expect_match(parse_drug_synonyms()[["synonym"]][[1]], "Bivalirudina")
            expect_error(parse_drug_synonyms(TRUE))
          })

test_that(desc = "Read darug articles attributes",
          code = {
            expect_match(as.character(parse_drug_articles()[["pubmed-id"]][1]), "16466327")
            expect_error(parse_drug_articles(TRUE))
          })

test_that(desc = "Read darug products attributes",
          code = {
            expect_match(as.character(parse_drug_products()[["name"]][1]), "Angiomax")
            expect_error(parse_drug_products(TRUE))
          })

test_that(desc = "Read darug mixtures attributes",
          code = {
            expect_match(as.character(parse_drug_mixtures()[["name"]][1]), "Angiomax")
            expect_error(parse_drug_mixtures(TRUE))
          })

test_that(desc = "Read darug packagers attributes",
          code = {
            expect_match(as.character(parse_drug_packagers()[["name"]][1]), "Ben Venue Laboratories Inc.")
            expect_error(parse_drug_packagers(TRUE))
          })

test_that(desc = "Read darug manufacturers attributes",
          code = {
            expect_match(
              as.character(parse_drug_manufacturers()[["manufacturer"]][[1]]),
              "The medicines co"
            )
            expect_error(parse_drug_manufacturers(TRUE))
          })

test_that(desc = "Read darug prices attributes",
          code = {
            expect_match(as.character(parse_drug_prices()[["currency"]][[1]]),
                         "USD")
            expect_error(parse_drug_prices(TRUE))
          })

test_that(desc = "Read darug categories attributes",
          code = {
            expect_match(as.character(parse_drug_categories()[["mesh-id"]][[1]]),
                         "D000602")
            expect_error(parse_drug_categories(TRUE))
          })

test_that(desc = "Read darug affected organisms attributes",
          code = {
            expect_match(as.character(parse_drug_affected_organisms()[["affected_organism"]][[1]]),
                         "Humans and other mammals")
            expect_error(parse_drug_affected_organisms(TRUE))
          })

test_that(desc = "Read darug dosages attributes",
          code = {
            expect_match(as.character(parse_drug_dosages()[["route"]][[1]]),
                         "Intravenous")
            expect_error(parse_drug_dosages(TRUE))
          })

test_that(desc = "Read darug atc codes attributes",
          code = {
            expect_match(as.character(parse_drug_atc_codes()[["atc_code"]][[1]]),
                         "B01AE06")
            expect_error(parse_drug_atc_codes(TRUE))
          })

test_that(desc = "Read darug ahfs codes attributes",
          code = {
            expect_equal(nrow(parse_drug_ahfs_codes()),
                         1)
            expect_error(parse_drug_ahfs_codes(TRUE))
          })

test_that(desc = "Read darug pdb entries attributes",
          code = {
            expect_equal(nrow(parse_drug_pdb_entries()),
                         0)
            expect_error(parse_drug_pdb_entries(TRUE))
          })

test_that(desc = "Read darug patents attributes",
          code = {
            expect_match(as.character(parse_drug_patents()[["country"]][[1]]),
                         "United States")
            expect_error(parse_drug_patents(TRUE))
          })

test_that(desc = "Read darug interactions attributes",
          code = {
            expect_match(as.character(parse_drug_interactions()[["name"]][[1]]),
                         "St. John's Wort")
            expect_error(parse_drug_interactions(TRUE))
          })

test_that(desc = "Read darug food interactions attributes",
          code = {
            expect_equal(nrow(parse_drug_food_interactions()),
                         2)
            expect_error(parse_drug_food_interactions(TRUE))
          })

test_that(desc = "Read darug sequences attributes",
          code = {
            expect_equal(nrow(parse_drug_sequences()),
                         0)
            expect_error(parse_drug_sequences(TRUE))
          })

test_that(desc = "Read darug experimental properties attributes",
          code = {
            expect_equal(nrow(parse_drug_experimental_properties()),
                         0)
            expect_error(parse_drug_experimental_properties(TRUE))
          })

test_that(desc = "Read darug external identifiers attributes",
          code = {
            expect_match(
              as.character(parse_drug_external_identifiers()[["resource"]][[1]]),
              "Drugs Product Database \\(DPD\\)"
            )
            expect_error(parse_drug_external_identifiers(TRUE))
          })

test_that(desc = "Read darug external links attributes",
          code = {
            expect_match(as.character(parse_drug_external_links()[["resource"]][[1]]),
                         "RxList")
            expect_error(parse_drug_external_links(TRUE))
          })


test_that(desc = "Read darug snp effects attributes",
          code = {
            expect_equal(nrow(parse_drug_snp_effects()),
                         0)
            expect_error(parse_drug_snp_effects(TRUE))
          })

test_that(desc = "Read darug snp adverse drug reactions attributes",
          code = {
            expect_equal(nrow(parse_drug_snp_adverse_drug_reactions()),
                         0)
            expect_error(parse_drug_snp_adverse_drug_reactions(TRUE))
          })
