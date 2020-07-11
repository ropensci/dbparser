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
  desc =
    "Read drug enzymes polypeptides external identifiers attributes",
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
  desc = "Read drug enzymes polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_syn()),
      2
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
      1
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
      24
    )
    expect_true(is_tibble(enzymes_polypeptides_go()))
    expect_error(enzymes_polypeptides_go(TRUE))
  }
)
