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
    expect_null(read_drugbank_xml_db("I_do_not_exist_file.xml"))
    expect_null(read_drugbank_xml_db("drugbank_record"))
  }
)

biotech  <- "drugbank_record_biotech.xml"
q_parser <- purrr::quietly(parseDrugBank)

test_that(
  desc = "parse DrugBank DB - default params",
  code = {
    local_edition(3)
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"))
    expect_snapshot(dvobj$result)
  }
)


test_that(
  desc = "parse DrugBank DB - invalid options",
  code = {
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options = NULL)
    expect_true(grepl("'drug_options' cannot be empty", dvobj$messages[1]))

    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options = NA)
    expect_true(grepl("'drug_options' cannot be empty",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options = c("a", "b"))
    expect_true(grepl("Options: ' a, b ' are invalid",
                      dvobj$messages[1]))

    ######## ref options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      references_options = NULL)
    expect_true(grepl("'references_options' cannot be empty",
                      dvobj$messages[1]))

    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      references_options = NA)
    expect_true(grepl("'references_options' cannot be empty",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      references_options = c("a", "b"))
    expect_true(grepl("Options: ' a, b ' are invalid",
                      dvobj$messages[1]))

    ####### cett options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      cett_options = NULL)
    expect_true(grepl("'cett_options' cannot be empty", dvobj$messages[1]))

    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      cett_options = NA)
    expect_true(grepl("'cett_options' cannot be empty",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      cett_options = c("a", "b"))
    expect_true(grepl("Options: ' a, b ' are invalid",
                      dvobj$messages[1]))

    ####### parse_products options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NULL)
    expect_true(grepl("'parse_products' must have logical value", dvobj$messages[1]))

    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NA)
    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c("a", "b"))
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c(1, 3))
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = "c")
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = 1)
    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    ####### parse_products options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NULL)
    expect_true(grepl("'parse_products' must have logical value", dvobj$messages[1]))

    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NA)
    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c("a", "b"))
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c(1, 3))
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = "c")
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = 1)
    expect_true(grepl("'parse_products' must have logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

  }
)
