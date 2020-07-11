context("test cett actions parsers")
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
  desc = "Read drug carriers actions",
  code = {
    expect_equal(
      nrow(carriers_actions()),
      0
    )
    expect_error(carriers_actions(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes actions attributes",
  code = {
    expect_equal(
      nrow(enzymes_actions()),
      1
    )
    expect_true(is_tibble(enzymes_actions()))
    expect_error(enzymes_actions(TRUE))
  }
)

test_that(
  desc = "Read drug targets actions attributes",
  code = {
    expect_match(
      as.character(targets_actions()
                   [["action"]][[1]]),
      "inhibitor"
    )
    expect_true(is_tibble(targets_actions()))
    expect_error(targets_actions(TRUE))
  }
)

test_that(
  desc = "Read drug transporters actions attributes",
  code = {
    expect_equal(
      nrow(transporters_actions()),
      0
    )
    expect_true(is_tibble(transporters_actions()))
    expect_error(transporters_actions(TRUE))
  }
)
dbDisconnect(database_connection)
