context("test cett actions parsers")
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
  desc = "Read drug carriers actions",
  code = {
    expect_equal(
      nrow(carriers_actions()),
      0
    )
    expect_true(is_tibble(carriers_actions()))
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
  }
)
