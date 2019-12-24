context("test parse all drug nodes")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

classlist <- function(x) {
  map_df(x, class)
}

test_that(
  desc = "Read database",
  code = {
    expect_error(read_drugbank_xml_db("I_do_not_exist_file.xml"))
    expect_error(read_drugbank_xml_db("drugbank_record"))
  }
)
drugs <- drug_all()
drugs_types <- classlist(drugs)
test_that(
  desc = "Read all drug nodes",
  code = {
    expect_equal(length(drugs), 75)
    expect_equal(dim(drugs_types), c(3, 75))
    expect_error(drug_all(TRUE))
  }
)

test_that(
  desc = "Read selected drug nodes",
  code = {
    expect_equal(length(drug_element()), 75)
    expect_equal(length(drug_element(c("all"))), 75)
    expect_error(drug_element(save_table = TRUE))
    expect_error(drug_element(c("all"), save_table = TRUE))
    expect_error(drug_element(c("notvalid")))
    expect_error(drug_element(c("drug_ahfs_codes", "notvalid")))
    expect_equal(length(drug_element_options()), 76)
    expect_equal(length(drug_element(
      c(
        "ahfs_codes_drug",
        "affected_organisms_drug",
        "textbooks_transporter_drug"
      )
    )), 3)
  }
)
