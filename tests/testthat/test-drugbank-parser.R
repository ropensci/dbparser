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
    # metadata holds file hashes and the package version; tested in
    # test_source_metadata.R, kept out of the snapshot
    attr(dvobj$result, "source_metadata") <- NULL
    expect_snapshot(dvobj$result)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options = NULL)
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options = NA)
    expect_equal(length(dvobj$result), 1)
  }
)


test_that(
  desc = "parse DrugBank DB - invalid options",
  code = {
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options = c("a", "b"))
    expect_true(grepl("Options: ' a, b ' are invalid, setting 'drug_options' to default value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    ######## ref options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      references_options = c("a", "b"))
    expect_true(grepl("Options: ' a, b ' are invalid, setting 'references_options' to default value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    ####### cett options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      cett_options = c("a", "b"))
    expect_true(grepl("Options: ' a, b ' are invalid, setting 'cett_options' to default value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    ####### parse_products options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NULL)
    expect_true(grepl("'parse_products' must have single logical value", dvobj$messages[1]))

    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NA)
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c("a", "b"))
    expect_equal(length(dvobj$result), 1)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c(1, 3))
    expect_equal(length(dvobj$result), 1)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = "c")
    expect_equal(length(dvobj$result), 1)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = 1)
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    ####### parse_products options
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NULL)
    expect_true(grepl("'parse_products' must have single logical value", dvobj$messages[1]))

    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = NA)
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c("a", "b"))
    expect_equal(length(dvobj$result), 1)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = c(1, 3))
    expect_equal(length(dvobj$result), 1)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = "c")
    expect_equal(length(dvobj$result), 1)

    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      parse_products = 1)
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[1]))
    expect_equal(length(dvobj$result), 1)

    #test all invalid
    dvobj <- q_parser(system.file("extdata",
                                  biotech,
                                  package = "dbparser"),
                      drug_options       = c("a", "b"),
                      references_options = c("a", "b"),
                      cett_options       = c("a", "b"),
                      parse_salts        = 2,
                      parse_products     = c("a", "b"))
    expect_true(grepl("Options: ' a, b ' are invalid",
                      dvobj$messages[1]))
    expect_true(grepl("Options: ' a, b ' are invalid",
                      dvobj$messages[2]))
    expect_true(grepl("Options: ' a, b ' are invalid",
                      dvobj$messages[3]))
    expect_true(grepl("'parse_salts' must have single logical value",
                      dvobj$messages[4]))
    expect_true(grepl("'parse_products' must have single logical value",
                      dvobj$messages[5]))

    expect_equal(length(dvobj$result), 1)
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
    expect_equal(dim(dvobj$result$cett$targets$actions), c(1, 3))
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
    expect_equal(length(dvobj$result), 1)
    expect_equal(names(dvobj$result), "drugs")
  }
)


test_that(
  desc = "parse DrugBank DB - small molecule",
  code = {
    local_edition(3)
    small_molecule <- "drugbank_record_small_molecule.xml"
    dvobj          <- q_parser(system.file("extdata",
                                           small_molecule,
                                           package = "dbparser"),
                               drug_options       = drug_node_options(),
                               parse_salts        = TRUE,
                               parse_products     = TRUE,
                               references_options = references_node_options(),
                               cett_options       = cett_nodes_options())
    # metadata holds file hashes and the package version; tested in
    # test_source_metadata.R, kept out of the snapshot
    attr(dvobj$result, "source_metadata") <- NULL
    expect_snapshot(dvobj$result)
  }
)


test_that(
  desc = "parse DrugBank DB - zip file",
  code = {
    local_edition(3)
    small_molecule <- "drugbank_record.zip"
    dvobj          <- q_parser(system.file("extdata",
                                           small_molecule,
                                           package = "dbparser"))
    # metadata holds file hashes and the package version; tested in
    # test_source_metadata.R, kept out of the snapshot
    attr(dvobj$result, "source_metadata") <- NULL
    expect_snapshot(dvobj$result)
  }
)

test_that(
  desc = "parse DrugBank DB - zip file leaves package and input folders unchanged",
  code = {
    folder_state <- function(path) {
      files <- list.files(path, recursive = TRUE, all.files = TRUE,
                          no.. = TRUE, full.names = TRUE)
      tools::md5sum(sort(files))
    }

    pkg_dir    <- system.file(package = "dbparser")
    pkg_before <- folder_state(pkg_dir)
    dvobj      <- q_parser(system.file("extdata",
                                       "drugbank_record.zip",
                                       package = "dbparser"))
    expect_s3_class(dvobj$result, "dvobject")
    expect_identical(folder_state(pkg_dir), pkg_before)

    input_dir <- tempfile("dbparser_input_")
    dir.create(input_dir)
    on.exit(unlink(input_dir, recursive = TRUE), add = TRUE)
    file.copy(system.file("extdata", "drugbank_record.zip",
                          package = "dbparser"),
              input_dir)
    dvobj <- q_parser(file.path(input_dir, "drugbank_record.zip"))
    expect_s3_class(dvobj$result, "dvobject")
    expect_identical(list.files(input_dir, all.files = TRUE, no.. = TRUE),
                     "drugbank_record.zip")
  }
)
