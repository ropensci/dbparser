context("test cett actions parsers")
library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

biotech <- "drugbank_record_biotech.xml"
database_connection <- dbConnect(RSQLite::SQLite(), ":memory:")

test_that(
  desc = "Read database",
  code = {
    expect_true(read_drugbank_xml_db(
      system.file("extdata", biotech, package = "dbparser")
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
    expect_true(is_tibble(carriers_actions()))
    expect_error(
      nrow(carriers_actions(TRUE))
    )
    expect_error(carriers_actions(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes actions attributes",
  code = {
    expect_equal(
      nrow(enzymes_actions()),
      0
    )
    expect_true(is_tibble(enzymes_actions()))
    expect_error(enzymes_actions(TRUE))
  }
)

test_that(
  desc = "Read drug targ actions attributes",
  code = {
    expect_match(
      as.character(
        targets_actions()[["action"]][[1]]
      ),
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
