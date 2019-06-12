context("test parse all drug nodes")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

classlist <- function(x) {
  sapply(x, class)
}

test_that(desc = "Read database",
          code = {
            expect_true(get_xml_db_rows(
              system.file("extdata", "drugbank_record.zip", package = "dbparser")
            ))
            expect_error(get_xml_db_rows("I_do_not_exist_file.xml"))
            expect_error(get_xml_db_rows("drugbank_record"))
          })
drugs <- parse_drug_all()
drugs_types <- classlist(drugs)
test_that(desc = "Read all drug nodes",
          code = {
            expect_equal(length(drugs), 75)
            expect_equal(length(drugs_types[1,] == "tbl_df"), 75)
            expect_error(parse_drug_all(TRUE))
          })

test_that(desc = "Read selected drug nodes",
          code = {
            expect_equal(length(parse_drug_element()), 75)
            expect_equal(length(parse_drug_element(c("all"))), 75)
            expect_error(parse_drug_element(save_table = TRUE))
            expect_error(parse_drug_element(c("all"), save_table = TRUE))
            expect_error(parse_drug_element(c("notvalid")))
            expect_error(parse_drug_element(c("drug_ahfs_codes", "notvalid")))
            expect_equal(length(parse_drug_element_options()), 76)
            expect_equal(length(parse_drug_element(
              c(
                "AHFS_Codes_Drug",
                "Affected_Organisms_Drug",
                "Textbooks_Transporter_Drug"
              )
            )), 3)
          })
