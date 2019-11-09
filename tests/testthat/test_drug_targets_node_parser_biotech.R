context("test drug targets element parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

biotech <- "drugbank_record_biotech.xml"
test_that(desc = "Read database",
          code = {
            expect_true(get_xml_db_rows(
              system.file("extdata", biotech, package = "dbparser")
            ))
          })

test_that(desc = "Read darug targets actions attributes",
          code = {
            expect_match(as.character(
              parse_drug_targets_actions()[["action"]][[1]]),
                         "inhibitor")
            expect_error(parse_drug_targets_actions(TRUE))
          })

test_that(desc = "Read darug targets_articles attributes",
          code = {
            expect_match(
              as.character(parse_drug_targets_articles()[["citation"]][[1]]),
              paste0("coronary syndromes\\. Am J Cardiol\\. 1999 ",
              "Sep 2;84\\(5A\\):2M-6M\\.")
            )
            expect_error(parse_drug_targets_articles(TRUE))
          })

test_that(desc = "Read darug targets textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_targets_textbooks()),
                         0)
            expect_error(parse_drug_targets_textbooks(TRUE))
          })

test_that(desc = "Read darug targets links attributes",
          code = {
            expect_equal(nrow(parse_drug_targets_links()),
                         0)
            expect_error(parse_drug_targets_links(TRUE))
          })

test_that(desc = "Read darug targets polypeptides attributes",
          code = {
            expect_match(as.character(
              parse_drug_targets_polypeptides()[["name"]][[1]]),
                         "Prothrombin")
            expect_error(parse_drug_targets_polypeptides(TRUE))
          })

test_that(desc =
            "Read darug targets polypeptides external identifiers attributes",
          code = {
            expect_match(
              as.character(
                parse_drug_targets_polypeptides_external_identifiers()
                [["identifier"]][1]
              ),
              "HGNC:3535"
            )
            expect_error(
              parse_drug_targets_polypeptides_external_identifiers(TRUE))
          })

test_that(desc = "Read darug targets polypeptides synonyms attributes",
          code = {
            expect_match(
              as.character(parse_drug_targets_polypeptides_synonyms()
                           [["synonyms"]][1]),
              "3.4.21.5,Coagulation factor II"
            )
            expect_error(parse_drug_targets_polypeptides_synonyms(TRUE))
          })

test_that(desc = "Read darug targets polypeptides pfams attributes",
          code = {
            expect_match(as.character(parse_drug_targets_polypeptides_pfams()
                                      [["name"]][1]),
                         "Gla")
            expect_error(parse_drug_targets_polypeptides_pfams(TRUE))
          })

test_that(desc = "Read darug targets polypeptides go classifiers attributes",
          code = {
            expect_match(
              as.character(parse_drug_targets_polypeptides_go_classifiers()
                           [["description"]][1]),
              "blood microparticle"
            )
            expect_error(parse_drug_targets_polypeptides_go_classifiers(TRUE))
          })

test_that(desc = "Read darug targets attributes",
          code = {
            expect_match(as.character(parse_drug_targets()[["name"]][1]),
                         "Prothrombin")
            expect_error(parse_drug_targets(TRUE))
          })
