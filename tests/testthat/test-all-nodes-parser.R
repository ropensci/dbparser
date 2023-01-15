context("test parse drug all nodes")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

classlist <- function(x) {
  map_df(x, class)
}

test_that(
  desc = "Read database incorrectly",
  code = {
    expect_error(read_drugbank_xml_db("I_do_not_exist_file.xml"))
    expect_error(read_drugbank_xml_db("drugbank_record"))
  }
)

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


drugs <- run_all_parsers()
drugs_types <- classlist(drugs)
test_that(
  desc = "Read all drug nodes",
  code = {
    expect_equal(length(drugs), 81)
    expect_equal(dim(drugs_types), c(3, 81))
  }
)

test_that(
  desc = "Read selected drug nodes",
  code = {
    expect_equal(length(drug_element()), 81)
    expect_equal(length(drug_element(c("all"))), 81)
    expect_error(drug_element(c("notvalid")))
    expect_error(drug_element(c("drug_ahfs_codes", "notvalid")))
    expect_equal(length(drug_element_options()), 83)
    expect_equal(length(drug_element(
      c(
        "ahfs_codes_drug",
        "affected_organisms_drug",
        "textbooks_transporter_drug"
      )
    )), 3)
  }
)
