context("test drugbank database metadata")

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

test_that(desc = "Read drugbank database metadata",
          code = {
            expect_match(get_drugbank_version(),
                         "5.1")
            expect_match(get_drugbank_exported_date(),
                         "2018-07-03")
            expect_equal(length(get_drugbank_metadata()),
                         2)
          })
