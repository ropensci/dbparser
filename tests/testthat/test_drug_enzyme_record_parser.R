context("test drug enzymes attributes parsers")

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

test_that(desc = "Read darug enzymes attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes(FALSE)),
                         0)
            expect_error(parse_drug_enzymes())
          })

test_that(desc = "Read darug enzymes actions attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_actions(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_actions())
          })

test_that(desc = "Read darug enzymes articles attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_articles(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_articles())
          })


test_that(desc = "Read darug enzymes textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_textbooks(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_textbooks())
          })

test_that(desc = "Read darug enzymes links attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_links(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_links())
          })

test_that(desc = "Read darug enzymes polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_polypeptides())
          })

test_that(desc = "Read darug enzymes polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_enzymes_polypeptides_external_identifiers(FALSE)
            ),
            0)
            expect_error(parse_drug_enzymes_polypeptides_external_identifiers())
          })


test_that(desc = "Read darug enzymes polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_synonyms(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_synonyms())
          })

test_that(desc = "Read darug enzymes polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_pfams(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_pfams())
          })

test_that(desc = "Read darug enzymes polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_go_classifiers(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_go_classifiers())
          })
