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
  desc = "Read drug carriers actions that",
  code = {
    expect_equal(
      nrow(parse_drug_carriers_actions()),
      0
    )
    expect_error(parse_drug_carriers_actions(TRUE))
  }
)

test_that(
  desc = "Read drug carriers articles that",
  code = {
    expect_equal(
      nrow(parse_drug_carriers_articles()),
      0
    )
    expect_error(parse_drug_carriers_articles(TRUE))
  }
)

test_that(
  desc = "Read drug carriers_textbooks that",
  code = {
    expect_equal(
      nrow(parse_drug_carriers_textbooks()),
      0
    )
    expect_error(parse_drug_carriers_textbooks(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides that",
  code = {
    expect_equal(
      nrow(parse_carr_poly()),
      0
    )
    expect_error(parse_carr_poly(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides external identifiers that",
  code = {
    expect_equal(
      nrow(parse_carr_poly_ext_identity()),
      0
    )
    expect_error(
      parse_carr_poly_ext_identity(TRUE)
    )
  }
)

test_that(
  desc = "Read drug carriers polypeptides syn that",
  code = {
    expect_equal(
      nrow(parse_carr_polypeptides_syn()),
      0
    )
    expect_error(parse_carr_polypeptides_syn(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides go classifiers that",
  code = {
    expect_equal(
      nrow(parse_carr_polypeptides_go()),
      0
    )
    expect_error(parse_carr_polypeptides_go(TRUE))
  }
)

test_that(
  desc = "Read drug carriers that",
  code = {
    expect_equal(
      nrow(parse_drug_carriers()),
      0
    )
    expect_error(parse_drug_carriers(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides pfams that",
  code = {
    expect_equal(
      nrow(parse_carr_polypeptides_pfams()),
      0
    )
    expect_error(parse_carr_polypeptides_pfams(TRUE))
  }
)
