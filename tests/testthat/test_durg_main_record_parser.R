context("test drug main attributes parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)



test_that(desc = "Read database",
          code = {
            expect_true(get_xml_db_rows(
              system.file("extdata", "drugbank_record.xml", package = "dbparser")
            ))
          })

test_that(desc = "Read darug main attributes",
          code = {
            expect_match(parse_drug(FALSE)[1][["primary_key"]], "DB00001")
            expect_error(parse_drug())
          })

test_that(desc = "Read darug groups attributes",
          code = {
            expect_match(as.character(parse_drug_groups(FALSE)[1][["text"]]), "approved")
            expect_error(parse_drug_groups())
          })

test_that(desc = "Read darug articles attributes",
          code = {
            expect_match(as.character(parse_drug_articles(FALSE)[["pubmed-id"]][1]), "16244762")
            expect_error(parse_drug_articles())
          })

test_that(desc = "Read darug books attributes",
          code = {
            expect_equal(nrow(parse_drug_books(FALSE)), 0)
            expect_error(parse_drug_books())
          })

test_that(desc = "Read darug links attributes",
          code = {
            expect_match(as.character(parse_drug_links(FALSE)[1][["title"]]), "Google books")
            expect_error(parse_drug_links())
          })

test_that(desc = "Read darug classifications attributes",
          code = {
            expect_match(parse_drug_classfications(FALSE)[1][["parent_key"]], "DB00001")
            expect_error(parse_drug_classfications())
          })

test_that(desc = "Read darug synonyms attributes",
          code = {
            expect_match(parse_drug_synonyms(FALSE)[["synonym"]][[1]], "Hirudin variant-1")
            expect_error(parse_drug_synonyms())
          })

test_that(desc = "Read darug articles attributes",
          code = {
            expect_match(as.character(parse_drug_articles(FALSE)[["pubmed-id"]][1]), "16244762")
            expect_error(parse_drug_articles())
          })

test_that(desc = "Read darug products attributes",
          code = {
            expect_match(as.character(parse_drug_products(FALSE)[["name"]][1]), "Refludan")
            expect_error(parse_drug_products())
          })

test_that(desc = "Read darug mixtures attributes",
          code = {
            expect_match(as.character(parse_drug_mixtures(FALSE)[["name"]][1]), "Refludan")
            expect_error(parse_drug_mixtures())
          })

test_that(desc = "Read darug packagers attributes",
          code = {
            expect_match(as.character(parse_drug_packagers(FALSE)[["name"]][1]), "Bayer Healthcare")
            expect_error(parse_drug_packagers())
          })

test_that(desc = "Read darug manufacturers attributes",
          code = {
            expect_match(
              as.character(parse_drug_manufacturers(FALSE)[["text"]][[1]]),
              "Bayer healthcare pharmaceuticals inc"
            )
            expect_error(parse_drug_manufacturers())
          })

test_that(desc = "Read darug prices attributes",
          code = {
            expect_match(as.character(parse_drug_prices(FALSE)[["currency"]][[1]]),
                         "USD")
            expect_error(parse_drug_prices())
          })

test_that(desc = "Read darug categories attributes",
          code = {
            expect_match(as.character(parse_drug_categories(FALSE)[["mesh-id"]][[1]]),
                         "D000602")
            expect_error(parse_drug_categories())
          })

test_that(desc = "Read darug affected organisms attributes",
          code = {
            expect_match(as.character(parse_drug_affected_organisms(FALSE)[["text"]][[1]]),
                         "Humans and other mammals")
            expect_error(parse_drug_affected_organisms())
          })

test_that(desc = "Read darug dosages attributes",
          code = {
            expect_match(as.character(parse_drug_dosages(FALSE)[["route"]][[1]]),
                         "Intravenous")
            expect_error(parse_drug_dosages())
          })

test_that(desc = "Read darug atc codes attributes",
          code = {
            expect_match(as.character(parse_drug_atc_codes(FALSE)[["atc_code"]][[1]]),
                         "B01AE02")
            expect_error(parse_drug_atc_codes())
          })

test_that(desc = "Read darug ahfs codes attributes",
          code = {
            expect_equal(nrow(parse_drug_ahfs_codes(FALSE)),
                         0)
            expect_error(parse_drug_ahfs_codes())
          })

test_that(desc = "Read darug pdb entries attributes",
          code = {
            expect_equal(nrow(parse_drug_pdb_entries(FALSE)),
                         0)
            expect_error(parse_drug_pdb_entries())
          })

test_that(desc = "Read darug patents attributes",
          code = {
            expect_match(as.character(parse_drug_patents(FALSE)[["country"]][[1]]),
                         "United States")
            expect_error(parse_drug_patents())
          })

test_that(desc = "Read darug interactions attributes",
          code = {
            expect_match(as.character(parse_drug_interactions(FALSE)[["name"]][[1]]),
                         "St. John's Wort")
            expect_error(parse_drug_interactions())
          })

test_that(desc = "Read darug food interactions attributes",
          code = {
            expect_equal(nrow(parse_drug_food_interactions(FALSE)),
                         0)
            expect_error(parse_drug_food_interactions())
          })

test_that(desc = "Read darug sequences attributes",
          code = {
            expect_match(as.character(parse_drug_sequences(FALSE)[["format"]][[1]]),
                         "FASTA")
            expect_error(parse_drug_sequences())
          })

test_that(desc = "Read darug experimental properties attributes",
          code = {
            expect_match(as.character(parse_drug_experimental_properties(FALSE)[["kind"]][[1]]),
                         "Melting Point")
            expect_error(parse_drug_experimental_properties())
          })

test_that(desc = "Read darug external identifiers attributes",
          code = {
            expect_match(
              as.character(parse_drug_external_identifiers(FALSE)[["resource"]][[1]]),
              "Drugs Product Database \\(DPD\\)"
            )
            expect_error(parse_drug_external_identifiers())
          })

test_that(desc = "Read darug external links attributes",
          code = {
            expect_match(as.character(parse_drug_external_links(FALSE)[["resource"]][[1]]),
                         "RxList")
            expect_error(parse_drug_external_links())
          })


test_that(desc = "Read darug snp effects attributes",
          code = {
            expect_equal(nrow(parse_drug_snp_effects(FALSE)),
                         0)
            expect_error(parse_drug_snp_effects())
          })

test_that(desc = "Read darug snp adverse drug reactions attributes",
          code = {
            expect_equal(nrow(parse_drug_snp_adverse_drug_reactions(FALSE)),
                         0)
            expect_error(parse_drug_snp_adverse_drug_reactions())
          })
