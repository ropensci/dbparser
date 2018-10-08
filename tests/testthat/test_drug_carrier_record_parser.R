context("test drug carrier attributes parsers")

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

test_that(desc = "Read darug carriers actions attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_actions (FALSE)),
                         0)
            expect_error(parse_drug_carriers_actions ())
          })

test_that(desc = "Read darug carriers articles attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_articles(FALSE)),
                         0)
            expect_error(parse_drug_carriers_articles())
          })

test_that(desc = "Read darug carriers_textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_textbooks(FALSE)),
                         0)
            expect_error(parse_drug_carriers_textbooks())
          })

test_that(desc = "Read darug carriers polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides(FALSE)),
                         0)
            expect_error(parse_drug_carriers_polypeptides())
          })

test_that(desc = "Read darug carriers polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_carriers_polypeptides_external_identifiers(FALSE)
            ),
            0)
            expect_error(parse_drug_carriers_polypeptides_external_identifiers())
          })

test_that(desc = "Read darug carriers polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_synonyms(FALSE)),
                         0)
            expect_error(parse_drug_carriers_polypeptides_synonyms())
          })

test_that(desc = "Read darug carriers polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_go_classifiers(FALSE)),
                         0)
            expect_error(parse_drug_carriers_polypeptides_go_classifiers())
          })

test_that(desc = "Read darug carriers attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers(FALSE)),
                         0)
            expect_error(parse_drug_carriers())
          })

test_that(desc = "Read darug carriers polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_pfams(FALSE)),
                         0)
            expect_error(parse_drug_carriers_polypeptides_pfams())
          })
