context("test drug targ element parsers")

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
  desc = "Read darug targ actions attributes",
  code = {
    expect_match(
      as.character(
        targets_actions()[["action"]][[1]]
      ),
      "inhibitor"
    )
    expect_true(is_tibble(targets_actions()))
    expect_error(targets_actions(TRUE))
  }
)

test_that(
  desc = "Read darug targ polypeptides attributes",
  code = {
    expect_match(
      as.character(
        targets_polypeptides()[["name"]][[1]]
      ),
      "Prothrombin"
    )
    expect_true(is_tibble(targets_polypeptides()))
    expect_error(targets_polypeptides(TRUE))
  }
)

test_that(
  desc =
    "Read darug targ polypeptides external identifiers attributes",
  code = {
    expect_match(
      as.character(
        targets_polypeptide_ext_ident()
        [["identifier"]][1]
      ),
      "HGNC:3535"
    )
    expect_true(is_tibble(targets_polypeptide_ext_ident()))
    expect_error(
      targets_polypeptide_ext_ident(TRUE)
    )
  }
)

test_that(
  desc = "Read darug targ polypeptides syn attributes",
  code = {
    expect_match(
      as.character(targets_polypeptide_syn()
      [["syn"]][1]),
      "3.4.21.5,Coagulation factor II"
    )
    expect_true(is_tibble(targets_polypeptide_syn()))
    expect_error(targets_polypeptide_syn(TRUE))
  }
)

test_that(
  desc = "Read darug targ polypeptides pfams attributes",
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
  desc = "Read darug targ polypeptides go classifiers attributes",
  code = {
    expect_match(
      as.character(targets_polypeptide_go()
      [["description"]][1]),
      "blood microparticle"
    )
    expect_true(is_tibble(targets_polypeptide_go()))
    expect_error(targets_polypeptide_go(TRUE))
  }
)

test_that(
  desc = "Read darug targ attributes",
  code = {
    expect_match(
      as.character(targets()[["name"]][1]),
      "Prothrombin"
    )
    expect_true(is_tibble(targets()))
    expect_error(targets(TRUE))
  }
)
