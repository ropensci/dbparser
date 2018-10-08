context("test drug targets element parsers")

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

test_that(desc = "Read darug targets actions attributes",
          code = {
            expect_match(as.character(parse_drug_targets_actions(FALSE)[["text"]][[1]]),
                         "inhibitor")
            expect_error(parse_drug_targets_actions())
          })

test_that(desc = "Read darug targets_articles attributes",
          code = {
            expect_match(
              as.character(parse_drug_targets_articles(FALSE)[["citation"]][[1]]),
              "coronary syndromes\\. Am J Cardiol\\. 1999 Sep 2;84\\(5A\\):2M-6M\\."
            )
            expect_error(parse_drug_targets_articles())
          })

test_that(desc = "Read darug targets textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_targets_textbooks(FALSE)),
                         0)
            expect_error(parse_drug_targets_textbooks())
          })

test_that(desc = "Read darug targets links attributes",
          code = {
            expect_equal(nrow(parse_drug_targets_links(FALSE)),
                         0)
            expect_error(parse_drug_targets_links())
          })

test_that(desc = "Read darug targets polypeptides attributes",
          code = {
            expect_match(as.character(parse_drug_targets_polypeptides(FALSE)[["name"]][[1]]),
                         "Prothrombin")
            expect_error(parse_drug_targets_polypeptides())
          })

test_that(desc = "Read darug targets polypeptides external identifiers attributes",
          code = {
            expect_match(
              as.character(
                parse_drug_targets_polypeptides_external_identifiers(FALSE)[["identifier"]][1]
              ),
              "HGNC:3535"
            )
            expect_error(parse_drug_targets_polypeptides_external_identifiers())
          })

test_that(desc = "Read darug targets polypeptides synonyms attributes",
          code = {
            expect_match(
              as.character(parse_drug_targets_polypeptides_synonyms(FALSE)[["synonyms"]][1]),
              "3.4.21.5,Coagulation factor II"
            )
            expect_error(parse_drug_targets_polypeptides_synonyms())
          })

test_that(desc = "Read darug targets polypeptides pfams attributes",
          code = {
            expect_match(as.character(parse_drug_targets_polypeptides_pfams(FALSE)[["name"]][1]),
                         "Gla")
            expect_error(parse_drug_targets_polypeptides_pfams())
          })

test_that(desc = "Read darug targets polypeptides go classifiers attributes",
          code = {
            expect_match(
              as.character(parse_drug_targets_polypeptides_go_classifiers(FALSE)[["description"]][1]),
              "blood microparticle"
            )
            expect_error(parse_drug_targets_polypeptides_go_classifiers())
          })

test_that(desc = "Read darug targets attributes",
          code = {
            expect_match(as.character(parse_drug_targets(FALSE)[["name"]][1]),
                         "Prothrombin")
            expect_error(parse_drug_targets())
          })
