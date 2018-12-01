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
            expect_equal(nrow(parse_drug_enzymes()),
                         0)
            expect_error(parse_drug_enzymes(TRUE))
          })

test_that(desc = "Read darug enzymes actions attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_actions()),
                         0)
            expect_error(parse_drug_enzymes_actions(TRUE))
          })

test_that(desc = "Read darug enzymes articles attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_articles()),
                         0)
            expect_error(parse_drug_enzymes_articles(TRUE))
          })


test_that(desc = "Read darug enzymes textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_textbooks()),
                         0)
            expect_error(parse_drug_enzymes_textbooks(TRUE))
          })

test_that(desc = "Read darug enzymes links attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_links()),
                         0)
            expect_error(parse_drug_enzymes_links(TRUE))
          })

test_that(desc = "Read darug enzymes polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides()),
                         0)
            expect_error(parse_drug_enzymes_polypeptides(TRUE))
          })

test_that(desc = "Read darug enzymes polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_enzymes_polypeptides_external_identifiers()
            ),
            0)
            expect_error(parse_drug_enzymes_polypeptides_external_identifiers(TRUE))
          })


test_that(desc = "Read darug enzymes polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_synonyms()),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_synonyms(TRUE))
          })

test_that(desc = "Read darug enzymes polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_pfams()),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_pfams(TRUE))
          })

test_that(desc = "Read darug enzymes polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_go_classifiers()),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_go_classifiers(TRUE))
          })
