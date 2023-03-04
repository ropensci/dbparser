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
    expect_true(grepl("'parse_products' must have single logical value", dvobj$messages[1]))

    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NA)
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c("a", "b"))
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c(1, 3))
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = "c")
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = 1)
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    ####### parse_products options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NULL)
    expect_true(grepl("'parse_products' must have single logical value", dvobj$messages[1]))

    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NA)
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c("a", "b"))
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c(1, 3))
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = "c")
    expect_equal(length(dvobj$result), 5)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = 1)
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 5)

    #test all invalid
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options       = NULL,
                      references_options = NULL,
                      cett_options       = c("a", "b"),
                      parse_salts        = 2,
                      parse_products     = NULL)
    expect_true(grepl("'drug_options' cannot be empty",
                      dvobj$messages[1]))
    expect_true(grepl("'references_options' cannot be empty",
                      dvobj$messages[2]))
    expect_true(grepl("Options: ' a, b ' are invalid",
                      dvobj$messages[3]))
    expect_true(grepl("'parse_salts' must have single logical value",
                      dvobj$messages[4]))
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[5]))

    expect_equal(length(dvobj$result), 5)
  }
)


test_that(
  desc = "parse DrugBank DB - drug_node_options",
  code = {
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options = c("atc_codes", "patents"))
    expect_equal(length(dvobj$result$drugs), 3)
    expect_equal(names(dvobj$result$drugs),
                 c("general_information", "atc_codes", "patents"))
    expect_equal(dim(dvobj$result$drugs$atc_codes), c(1, 10))
    expect_equal(dim(dvobj$result$drugs$patents), c(1, 6))
  }
)


test_that(
  desc = "parse DrugBank DB - references_node_options",
  code = {
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      references_options = c("carrier_books", "transporter_books"))
    expect_equal(length(dvobj$result$references), 2)
    expect_equal(names(dvobj$result$references),
                 c("carriers", "transporters"))
    expect_equal(dim(dvobj$result$references$carriers$books), c(0, 0))
    expect_equal(dim(dvobj$result$references$transporters$books), c(0, 0))
  }
)


test_that(
  desc = "parse DrugBank DB - cett_nodes_options",
  code = {
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      cett_options = c("targets"))
    expect_equal(length(dvobj$result$cett), 1)
    expect_equal(names(dvobj$result$cett),
                 c("targets"))
    expect_equal(dim(dvobj$result$cett$targets$general_information), c(1, 6))
    expect_equal(dim(dvobj$result$cett$targets$actions), c(1, 2))
  }
)


test_that(
  desc = "parse DrugBank DB - boolean params",
  code = {
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_salts    = FALSE,
                      parse_products = FALSE)
    expect_equal(length(dvobj$result), 3)
    expect_equal(names(dvobj$result),
                 c("drugs", "references", "cett" ))
  }
)


test_that(
  desc = "parse DrugBank DB - small molecule",
  code = {
    local_edition(3)
    small_molecule <- "drugbank_record_small_molecule.xml"
    dvobj          <- q_parser(system.file("extdata",
                                           small_molecule,
                                           package = "dbparser"))
    expect_snapshot(dvobj$result)
  }
)


test_that(
  desc = "parse DrugBank DB - zim file",
  code = {
    local_edition(3)
    small_molecule <- "drugbank_record.zip"
    dvobj          <- q_parser(system.file("extdata",
                                           small_molecule,
                                           package = "dbparser"))
    expect_snapshot(dvobj$result)
  }
)
