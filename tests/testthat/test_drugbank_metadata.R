context("test drugbank database metadata")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

biotech <- "drugbank_record_biotech.xml"
test_that(
  desc = "Read database",
  code = {
    expect_true(read_drugbank_xml_db(
      system.file("extdata", biotech, package = "dbparser")
    ))
  }
)

test_that(
  desc = "Read drugbank database metadata",
  code = {
    expect_match(
      get_drugbank_version(),
      "5.1"
    )
    expect_match(
      get_drugbank_exported_date(),
      "2018-07-03"
    )
    expect_equal(
      length(get_drugbank_metadata()),
      2
    )
  }
)

test_that(
  desc = "Open DBI database",
  code = {
    expect_error(
      open_db(driver = "SQL Server", server = "dummy_server",
              output_database = "dummy_database")
    )
  }
)

test_that(
  desc = "Open MDB database",
  code = {
    expect_error(
      open_mdb(username = "user", password = "pass", server,
               output_database,
               host = "localhost", port = 3306)
    )
  }
)
