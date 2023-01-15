context("test cett general information parsers")
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
  desc = "Read drug carriers",
  code = {
    expect_equal(
      nrow(carriers()),
      0
    )
    expect_true(is_tibble(carriers()))
  }
)


test_that(
  desc = "Read drug enzymes attributes",
  code = {
    expect_equal(
      nrow(enzymes()),
      1
    )
    expect_true(is_tibble(enzymes()))
  }
)


test_that(
  desc = "Read drug targ attributes",
  code = {
    expect_match(
      as.character(targets()[["name"]][1]),
      "Prothrombin"
    )
    expect_true(is_tibble(targets()))
  }
)

test_that(
  desc = "Read drug transporters attributes",
  code = {
    expect_equal(
      nrow(transporters()),
      0
    )
    expect_true(is_tibble(transporters()))
  }
)
