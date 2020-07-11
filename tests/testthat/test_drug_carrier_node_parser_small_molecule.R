context("test small molecule drug carrier that parsers")

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
  desc = "Read drug carriers polypeptides external identifiers",
  code = {
    expect_equal(
      nrow(carriers_polypep_ex_ident()),
      0
    )
    expect_true(is_tibble(carriers_polypep_ex_ident()))
    expect_error(
      carriers_polypep_ex_ident(TRUE)
    )
  }
)

test_that(
  desc = "Read drug carriers polypeptides syn",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_syn()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_syn()))
    expect_error(carriers_polypeptides_syn(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides go classifiers",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_go()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_go()))
    expect_error(carriers_polypeptides_go(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides pfams",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_pfams()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_pfams()))
    expect_error(carriers_polypeptides_pfams(TRUE))
  }
)
