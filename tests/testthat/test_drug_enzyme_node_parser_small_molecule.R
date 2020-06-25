context("test small molecule drug enzymes attributes parsers")

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
  desc = "Read darug enzymes attributes",
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
  desc = "Read darug enzymes actions attributes",
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
  desc = "Read darug enzymes polypeptides attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides()),
      1
    )
    expect_true(is_tibble(enzymes_polypeptides()))
    expect_error(enzymes_polypeptides(TRUE))
  }
)

test_that(
  desc =
    "Read darug enzymes polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(
        enzymes_polypep_ex_ident()
      ),
      7
    )
    expect_true(is_tibble(enzymes_polypep_ex_ident()))
    expect_error(
      enzymes_polypep_ex_ident(TRUE)
    )
  }
)


test_that(
  desc = "Read darug enzymes polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptide_syn()),
      1
    )
    expect_true(is_tibble(enzymes_polypeptide_syn()))
    expect_error(enzymes_polypeptide_syn(TRUE))
  }
)

test_that(
  desc = "Read darug enzymes polypeptides pfams attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_pfams()),
      1
    )
    expect_true(is_tibble(enzymes_polypeptides_pfams()))
    expect_error(enzymes_polypeptides_pfams(TRUE))
  }
)

test_that(
  desc = "Read darug enzymes polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_go()),
      24
    )
    expect_true(is_tibble(enzymes_polypeptides_go()))
    expect_error(enzymes_polypeptides_go(TRUE))
  }
)
