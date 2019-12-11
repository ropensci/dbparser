context("test small molecule drug reaction attributes parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

biotech <- "drugbank_record_small_molecule.xml"
test_that(
  desc = "Read database",
  code = {
    expect_true(read_drugbank_xml_db(
      system.file("extdata", biotech, package = "dbparser")
    ))
  }
)

test_that(
  desc = "Read darug reactions attributes",
  code = {
    expect_equal(
      nrow(parse_drug_reactions()),
      0
    )
    expect_error(parse_drug_reactions(TRUE))
  }
)

test_that(
  desc = "Read darug reactions enzymes attributes",
  code = {
    expect_equal(
      nrow(parse_drug_reactions_enzymes()),
      0
    )
    expect_error(parse_drug_reactions_enzymes(TRUE))
  }
)
