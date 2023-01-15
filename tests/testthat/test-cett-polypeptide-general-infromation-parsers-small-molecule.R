context("test cett poly general info parsers")
library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

small_molecule <- "drugbank_record_small_molecule.xml"

test_that(
  desc = "Read database",
  code = {
    expect_true(read_drugbank_xml_db(
      system.file("extdata", small_molecule, package = "dbparser")
    ))
  }
)

test_that(
  desc = "Read drug carriers polypeptides",
  code = {
    expect_equal(
      nrow(carriers_polypeptides()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides()))
  }
)

test_that(
  desc = "Read drug enzymes polypeptides attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides()),
      1
    )
    expect_true(is_tibble(enzymes_polypeptides()))
  }
)

test_that(
  desc = "Read drug targets polypeptides attributes",
  code = {
    expect_match(
      as.character(targets_polypeptides()
                   [["name"]][[1]]),
      "Prothrombin"
    )
    expect_true(is_tibble(targets_polypeptides()))
  }
)

test_that(
  desc = "Read drug transporters polypeptides attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptides()),
      0
    )
    expect_true(is_tibble(transporters_polypeptides()))
  }
)
