context("test drug pathway attributes parsers")

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

test_that(desc = "Read darug pathway attributes",
          code = {
            expect_match(as.character(parse_drug_pathway(FALSE)[["name"]][[1]]),
                         "Lepirudin Action Pathway")
            expect_error(parse_drug_pathway())
          })

test_that(desc = "Read darug pathway drugs attributes",
          code = {
            expect_match(as.character(parse_drug_pathway_drugs(FALSE)[["name"]][[1]]),
                         "Lepirudin")
            expect_error(parse_drug_pathway_drugs())
          })

test_that(desc = "Read darug pathway enzyme attributes",
          code = {
            expect_match(as.character(parse_drug_pathway_enzyme(FALSE)[["text"]][[1]]),
                         "P00734")
            expect_error(parse_drug_pathway_enzyme())
          })
