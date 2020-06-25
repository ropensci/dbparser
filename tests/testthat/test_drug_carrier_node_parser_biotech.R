context("test biotech drug carrier that parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

biotech <- "drugbank_record_biotech.xml"
database_connection <- dbConnect(RSQLite::SQLite(), ":memory:")

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
    expect_error(
      nrow(carriers_actions(TRUE))
    )
    expect_error(carriers_actions(TRUE))
  }
)


test_that(
  desc = "Read drug carriers polypeptides that",
  code = {
    expect_equal(
      nrow(carriers_polypeptides()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides()))
    expect_error(carriers_polypeptides(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides external identifiers that",
  code = {
    expect_equal(
      nrow(
        carriers_polypep_ex_ident()
      ),
      0
    )
    expect_true(is_tibble(carriers_polypep_ex_ident()))
    expect_error(
      carriers_polypep_ex_ident(TRUE)
    )
  }
)

test_that(
  desc = "Read drug carriers polypeptides syn that",
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
  desc = "Read drug carriers polypeptides go classifiers that",
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
  desc = "Read drug carriers that",
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
  desc = "Read drug carriers polypeptides pfams that",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_pfams()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_pfams()))
    expect_error(carriers_polypeptides_pfams(TRUE))
  }
)
dbDisconnect(database_connection)
