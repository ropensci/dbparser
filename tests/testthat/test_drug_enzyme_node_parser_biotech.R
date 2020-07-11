context("test biotech drug enzymes attributes parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

biotech <- "drugbank_record_biotech.xml"
test_that(
  desc = "Read database",
  code = {
    expect_true(
      read_drugbank_xml_db(
        system.file("extdata", biotech, package = "dbparser")
      )
    )
  }
)

test_that(
  desc = "Read drug enzymes attributes",
  code = {
    expect_equal(
      nrow(enzymes()),
      0
    )
    expect_true(is_tibble(enzymes()))
    expect_error(enzymes(TRUE))
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
    expect_error(enzymes_polypeptides(TRUE))
  }
)

test_that(
  desc =
    "Read drug enzymes polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypep_ex_ident()),
      0
    )
    expect_true(is_tibble(enzymes_polypep_ex_ident()))
    expect_error(
      enzymes_polypep_ex_ident(TRUE)
    )
  }
)


test_that(
  desc = "Read drug enzymes polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_syn()),
      0
    )
    expect_true(is_tibble(enzymes_polypeptides_syn()))
    expect_error(enzymes_polypeptides_syn(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes polypeptides pfams attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_pfams()),
      0
    )
    expect_true(is_tibble(enzymes_polypeptides_pfams()))
    expect_error(enzymes_polypeptides_pfams(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_go()),
      0
    )
    expect_true(is_tibble(enzymes_polypeptides_go()))
    expect_error(enzymes_polypeptides_go(TRUE))
  }
)
