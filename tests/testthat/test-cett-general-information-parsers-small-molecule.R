context("test cett general information parsers")
library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

small_molecule <- "drugbank_record_small_molecule.xml"
database_connection <- dbConnect(RSQLite::SQLite(), ":memory:")

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
    expect_error(carriers(TRUE))
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
    expect_error(enzymes(TRUE))
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
    expect_error(targets(TRUE))
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
    expect_error(transporters(TRUE))
  }
)
dbDisconnect(database_connection)
