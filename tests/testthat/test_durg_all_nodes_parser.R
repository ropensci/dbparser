context("test parse all drug nodes")

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

test_that(desc = "Read all drug nodes",
          code = {
            expect_equal(length(parse_drug_all()), 72)
            expect_error(parse_drug_all(TRUE))
          })
