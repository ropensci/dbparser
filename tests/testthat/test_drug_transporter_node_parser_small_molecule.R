context("test small molecule drug transporter element parsers")

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

test_that(
  desc = "Read drug transporters polypeptides attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptides()),
      0
    )
    expect_true(is_tibble(transporters_polypeptides()))
    expect_error(transporters_polypeptides(TRUE))
  }
)

test_that(
  desc = "Read drug transporters polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(transporters_polypep_ex_ident()),
      0
    )
    expect_true(is_tibble(transporters_polypep_ex_ident()))
    expect_error(
      transporters_polypep_ex_ident(TRUE)
    )
  }
)

test_that(
  desc = "Read drug transporters polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptide_syn()),
      0
    )
    expect_true(is_tibble(transporters_polypeptide_syn()))
    expect_error(transporters_polypeptide_syn(TRUE))
  }
)

test_that(
  desc = "Read drug transporters polypeptides pfams attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptide_pfams()),
      0
    )
    expect_true(is_tibble(transporters_polypeptide_pfams()))
    expect_error(transporters_polypeptide_pfams(TRUE))
  }
)

test_that(
  desc = "Read drug transporters polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptide_go()), 0
    )
    expect_true(is_tibble(transporters_polypeptide_go()))
    expect_error(transporters_polypeptide_go(TRUE))
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
