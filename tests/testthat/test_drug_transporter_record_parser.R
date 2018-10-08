context("test drug transporter element parsers")

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

test_that(desc = "Read darug transporters actions attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_actions(FALSE)),
                         0)
            expect_error(parse_drug_transporters_actions())
          })

test_that(desc = "Read darug transporters_articles attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_articles(FALSE)),
                         0)
            expect_error(parse_drug_transporters_articlese())
          })

test_that(desc = "Read darug transporters textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_textbooks(FALSE)),
                         0)
            expect_error(parse_drug_transporters_textbooks())
          })

test_that(desc = "Read darug transporters links attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_links(FALSE)),
                         0)
            expect_error(parse_drug_transporters_links())
          })

test_that(desc = "Read darug transporters polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides(FALSE)),
                         0)
            expect_error(parse_drug_transporters_polypeptides())
          })

test_that(desc = "Read darug transporters polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_transporters_polypeptides_external_identifiers(FALSE)
            ),
            0)
            expect_error(parse_drug_transporters_polypeptides_external_identifiers())
          })

test_that(desc = "Read darug transporters polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides_synonyms(FALSE)),
                         0)
            expect_error(parse_drug_transporters_polypeptides_synonyms())
          })

test_that(desc = "Read darug transporters polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides_pfams(FALSE)),
                         0)
            expect_error(parse_drug_transporters_polypeptides_pfams())
          })

test_that(desc = "Read darug transporters polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_transporters_polypeptides_go_classifiers(FALSE)
            ),
            0)
            expect_error(parse_drug_transporters_polypeptides_go_classifiers())
          })

test_that(desc = "Read darug transporters attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters(FALSE)),
                         0)
            expect_error(parse_drug_transporters())
          })
