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
  desc = "Read drug carriers polypeptides",
  code = {
    expect_equal(
      nrow(carriers_polypeptide()),
      0
    )
    expect_true(is_tibble(carriers_polypeptide()))
    expect_error(carriers_polypeptide(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides external identifiers",
  code = {
    expect_equal(
      nrow(carriers_polypeptide_ext_id()),
      0
    )
    expect_true(is_tibble(carriers_polypeptide_ext_id()))
    expect_error(
      carriers_polypeptide_ext_id(TRUE)
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
