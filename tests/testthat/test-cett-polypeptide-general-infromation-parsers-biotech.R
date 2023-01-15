context("test cett poly general info parsers")
library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

biotech <- "drugbank_record_biotech.xml"

test_that(
  desc = "Read database",
  code = {
    expect_true(read_drugbank_xml_db(
      system.file("extdata", biotech, package = "dbparser")
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
      0
    )
    expect_true(is_tibble(enzymes_polypeptides()))
  }
)

test_that(
  desc = "Read drug targ polypeptides attributes",
  code = {
    expect_match(
      as.character(
        targets_polypeptides()[["name"]][[1]]
      ),
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
