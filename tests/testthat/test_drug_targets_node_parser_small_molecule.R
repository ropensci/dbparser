context("test small molecule drug targ element parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)

small_molecule <- "drugbank_record_small_molecule.xml"
test_that(
  desc = "Read database",
  code = {
    expect_true(read_drugbank_xml_db(
      system.file("extdata", small_molecule, package = "dbparser")
    ))
  }
)

test_that(
  desc = "Read drug targ polypeptides attributes",
  code = {
    expect_match(
      as.character(targets_polypeptides()
      [["name"]][[1]]),
      "Prothrombin"
    )
    expect_true(is_tibble(targets_polypeptides()))
    expect_error(targets_polypeptides(TRUE))
  }
)

test_that(
  desc =
    "Read drug targ polypeptides external identifiers attributes",
  code = {
    expect_match(
      as.character(
        targets_polypep_ex_ident()
        [["identifier"]][1]
      ),
      "HGNC:3535"
    )
    expect_true(is_tibble(targets_polypep_ex_ident()))
    expect_error(
      targets_polypep_ex_ident(TRUE)
    )
  }
)

test_that(
  desc = "Read drug targ polypeptides syn attributes",
  code = {
    expect_match(
      as.character(targets_polypeptides_syn()
      [["synonym"]][1]),
      "3.4.21.5"
    )
    expect_true(is_tibble(targets_polypeptides_syn()))
    expect_error(targets_polypeptides_syn(TRUE))
  }
)

test_that(
  desc = "Read drug targ polypeptides pfams attributes",
  code = {
    expect_match(
      as.character(targets_polypeptides_pfams()
      [["name"]][1]),
      "Gla"
    )
    expect_true(is_tibble(targets_polypeptides_pfams()))
    expect_error(targets_polypeptides_pfams(TRUE))
  }
)

test_that(
  desc = "Read drug targ polypeptides go classifiers attributes",
  code = {
    expect_match(
      as.character(targets_polypeptides_go()
      [["description"]][1]),
      "blood microparticle"
    )
    expect_true(is_tibble(targets_polypeptides_go()))
    expect_error(targets_polypeptides_go(TRUE))
  }
)

test_that(
  desc = "Read drug targ attributes",
  code = {
    expect_match(
      as.character(targets()[["name"]][1]),
      "Prothrombin"
    )
    expect_true(is_tibble(targets()))
    expect_error(targets(TRUE))
  }
)
