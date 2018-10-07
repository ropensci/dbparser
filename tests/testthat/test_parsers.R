context("test parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)



test_that(desc = "Read database",
                    code = {
                      expect_true(get_xml_db_rows(system.file(
                        "extdata","drugbank_record.xml", package = "dbparser")))
                    })
test_that(desc = "Read darug main attributes",
          code = {
            expect_match(parse_drug(FALSE)[1][["primary_key"]], "DB00001")
            expect_error(parse_drug())
          })

test_that(desc = "Read darug groups attributes",
          code = {
            expect_match(as.character(
              parse_drug_groups(FALSE)[1][["text"]]), "approved")
            expect_error(parse_drug_groups())
          })
