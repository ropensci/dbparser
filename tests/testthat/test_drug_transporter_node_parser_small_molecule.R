context("test small molecule drug transporter element parsers")

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

test_that(desc = "Read darug transporters actions attributes",
          code = {
            expect_equal(nrow(parse_drug_trans_actions()),
                         0)
            expect_error(parse_drug_trans_actions(TRUE))
          })

test_that(desc = "Read darug transporters_articles attributes",
          code = {
            expect_equal(nrow(parse_drug_trans_articles()),
                         0)
            expect_error(parse_drug_trans_articlese(TRUE))
          })

test_that(desc = "Read darug transporters textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_trans_textbooks()),
                         0)
            expect_error(parse_drug_trans_textbooks(TRUE))
          })

test_that(desc = "Read darug transporters links attributes",
          code = {
            expect_equal(nrow(parse_drug_trans_links()),
                         0)
            expect_error(parse_drug_trans_links(TRUE))
          })

test_that(desc = "Read darug transporters polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_trans_polys()),
                         0)
            expect_error(parse_drug_trans_polys(TRUE))
          })

test_that(
  desc = "Read darug transporters polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(parse_trans_poly_ex_identity()),
            0)
    expect_error(
      parse_trans_poly_ex_identity(TRUE))
          })

test_that(desc = "Read darug transporters polypeptides syn attributes",
          code = {
            expect_equal(nrow(parse_trans_poly_syn()),
                         0)
            expect_error(parse_trans_poly_syn(TRUE))
          })

test_that(desc = "Read darug transporters polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_trans_poly_pfams()),
                         0)
            expect_error(parse_trans_poly_pfams(TRUE))
          })

test_that(
  desc = "Read darug transporters polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(parse_trans_poly_go()), 0)
    expect_error(parse_trans_poly_go(TRUE))})

test_that(desc = "Read darug transporters attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters()),
                         0)
            expect_error(parse_drug_transporters(TRUE))
          })
