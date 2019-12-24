context("test drug transporter element parsers")

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
  desc = "Read darug transporters actions attributes",
  code = {
    expect_equal(
      nrow(transporters_actions()),
      0
    )
    expect_error(transporters_actions(TRUE))
  }
)

test_that(
  desc = "Read darug transporters_articles attributes",
  code = {
    expect_equal(
      nrow(transporters_articles()),
      0
    )
    expect_error(transporters_articlese(TRUE))
  }
)

test_that(
  desc = "Read darug transporters textbooks attributes",
  code = {
    expect_equal(
      nrow(transporters_textbooks()),
      0
    )
    expect_error(transporters_textbooks(TRUE))
  }
)

test_that(
  desc = "Read darug transporters links attributes",
  code = {
    expect_equal(
      nrow(transporters_links()),
      0
    )
    expect_error(transporters_links(TRUE))
  }
)

test_that(
  desc = "Read darug transporters polypeptides attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptide()),
      0
    )
    expect_error(transporters_polypeptide(TRUE))
  }
)

test_that(
  desc = "Read darug transporters polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(transporters_polypep_ex_ident()),
      0
    )
    expect_error(
      transporters_polypep_ex_ident(TRUE)
    )
  }
)

test_that(
  desc = "Read darug transporters polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptide_syn()),
      0
    )
    expect_error(transporters_polypeptide_syn(TRUE))
  }
)

test_that(
  desc = "Read darug transporters polypeptides pfams attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptide_pfams()),
      0
    )
    expect_error(transporters_polypeptide_pfams(TRUE))
  }
)

test_that(
  desc = "Read darug transporters polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptide_go()),
      0
    )
    expect_error(transporters_polypeptide_go(TRUE))
  }
)

test_that(
  desc = "Read darug transporters attributes",
  code = {
    expect_equal(
      nrow(transporters()),
      0
    )
    expect_error(transporters(TRUE))
  }
)
