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
  desc = "Read darug enzymes attributes",
  code = {
    expect_equal(
      nrow(enzymes()),
      0
    )
    expect_error(enzymes(TRUE))
  }
)

test_that(
  desc = "Read darug enzymes actions attributes",
  code = {
    expect_equal(
      nrow(enzymes_actions()),
      0
    )
    expect_error(enzymes_actions(TRUE))
  }
)

test_that(
  desc = "Read darug enzymes articles attributes",
  code = {
    expect_equal(
      nrow(enzymes_articles()),
      0
    )
    expect_error(enzymes_articles(TRUE))
  }
)


test_that(
  desc = "Read darug enzymes textbooks attributes",
  code = {
    expect_equal(
      nrow(enzymes_textbooks()),
      0
    )
    expect_error(enzymes_textbooks(TRUE))
  }
)

test_that(
  desc = "Read darug enzymes links attributes",
  code = {
    expect_equal(
      nrow(enzymes_links()),
      0
    )
    expect_error(enzymes_links(TRUE))
  }
)

test_that(
  desc = "Read darug enzymes polypeptides attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptide()),
      0
    )
    expect_error(enzymes_polypeptide(TRUE))
  }
)

test_that(
  desc =
    "Read darug enzymes polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptide_ext_ident()),
      0
    )
    expect_error(
      enzymes_polypeptide_ext_ident(TRUE)
    )
  }
)


test_that(
  desc = "Read darug enzymes polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptide_syn()),
      0
    )
    expect_error(enzymes_polypeptide_syn(TRUE))
  }
)

test_that(
  desc = "Read darug enzymes polypeptides pfams attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptide_pfams()),
      0
    )
    expect_error(enzymes_polypeptide_pfams(TRUE))
  }
)

test_that(
  desc = "Read darug enzymes polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptide_go()),
      0
    )
    expect_error(enzymes_polypeptide_go(TRUE))
  }
)
