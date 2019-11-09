context("test drug transporter element parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

biotech <- "drugbank_record_biotech.xml"
test_that(desc = "Read database",
          code = {
            expect_true(
              get_xml_db_rows(
                system.file("extdata", biotech, package = "dbparser")))
          })

test_that(desc = "Read darug transporters actions attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_actions()),
                         0)
            expect_error(parse_drug_transporters_actions(TRUE))
          })

test_that(desc = "Read darug transporters_articles attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_articles()),
                         0)
            expect_error(parse_drug_transporters_articlese(TRUE))
          })

test_that(desc = "Read darug transporters textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_textbooks()),
                         0)
            expect_error(parse_drug_transporters_textbooks(TRUE))
          })

test_that(desc = "Read darug transporters links attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_links()),
                         0)
            expect_error(parse_drug_transporters_links(TRUE))
          })

test_that(desc = "Read darug transporters polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides()),
                         0)
            expect_error(parse_drug_transporters_polypeptides(TRUE))
          })

test_that(
  desc = "Read darug transporters polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(parse_drug_transporters_polypeptides_external_identifiers()),
            0)
    expect_error(
      parse_drug_transporters_polypeptides_external_identifiers(TRUE))
          })

test_that(desc = "Read darug transporters polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides_synonyms()),
                         0)
            expect_error(parse_drug_transporters_polypeptides_synonyms(TRUE))
          })

test_that(desc = "Read darug transporters polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides_pfams()),
                         0)
            expect_error(parse_drug_transporters_polypeptides_pfams(TRUE))
          })

test_that(
  desc = "Read darug transporters polypeptides go classifiers attributes",
  code = {
    expect_equal(nrow(parse_drug_transporters_polypeptides_go_classifiers()),
                         0)
    expect_error(parse_drug_transporters_polypeptides_go_classifiers(TRUE))})

test_that(desc = "Read darug transporters attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters()),
                         0)
            expect_error(parse_drug_transporters(TRUE))
          })
