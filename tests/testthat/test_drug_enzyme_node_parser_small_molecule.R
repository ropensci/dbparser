context("test small molecule drug enzymes attributes parsers")

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

test_that(desc = "Read darug enzymes attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes()),
                         1)
            expect_error(parse_drug_enzymes(TRUE))
          })

test_that(desc = "Read darug enzymes actions attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_actions()),
                         1)
            expect_error(parse_drug_enzymes_actions(TRUE))
          })

test_that(desc = "Read darug enzymes articles attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_articles()),
                         1)
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
            expect_equal(nrow(parse_enzy_poly()),
                         1)
            expect_error(parse_enzy_poly(TRUE))
          })

test_that(desc =
            "Read darug enzymes polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_enzy_poly_ext_identitys()
            ),
            7)
            expect_error(
              parse_enzy_poly_ext_identitys(TRUE))
          })


test_that(desc = "Read darug enzymes polypeptides syn attributes",
          code = {
            expect_equal(nrow(parse_enzy_poly_syn()),
                         1)
            expect_error(parse_enzy_poly_syn(TRUE))
          })

test_that(desc = "Read darug enzymes polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_enzy_poly_pfams()),
                         1)
            expect_error(parse_enzy_poly_pfams(TRUE))
          })

test_that(desc = "Read darug enzymes polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(parse_enzy_poly_go()),
                         24)
            expect_error(parse_enzy_poly_go(TRUE))
          })
