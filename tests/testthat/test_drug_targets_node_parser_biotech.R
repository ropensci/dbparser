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
        parse_drug_targ_actions()[["action"]][[1]]
      ),
      "inhibitor"
    )
    expect_error(parse_drug_targ_actions(TRUE))
  }
)

test_that(
  desc = "Read darug targ_articles attributes",
  code = {
    expect_match(
      as.character(parse_drug_targ_articles()[["citation"]][[1]]),
      paste0(
        "coronary syndromes\\. Am J Cardiol\\. 1999 ",
        "Sep 2;84\\(5A\\):2M-6M\\."
      )
    )
    expect_error(parse_drug_targ_articles(TRUE))
  }
)

test_that(
  desc = "Read darug targ textbooks attributes",
  code = {
    expect_equal(
      nrow(parse_drug_targ_textbooks()),
      0
    )
    expect_error(parse_drug_targ_textbooks(TRUE))
  }
)

test_that(
  desc = "Read darug targ links attributes",
  code = {
    expect_equal(
      nrow(parse_drug_targ_links()),
      0
    )
    expect_error(parse_drug_targ_links(TRUE))
  }
)

test_that(
  desc = "Read darug targ polypeptides attributes",
  code = {
    expect_match(
      as.character(
        parse_drug_targ_polys()[["name"]][[1]]
      ),
      "Prothrombin"
    )
    expect_error(parse_drug_targ_polys(TRUE))
  }
)

test_that(
  desc =
    "Read darug targ polypeptides external identifiers attributes",
  code = {
    expect_match(
      as.character(
        parse_targ_poly_ext_identity()
        [["identifier"]][1]
      ),
      "HGNC:3535"
    )
    expect_error(
      parse_targ_poly_ext_identity(TRUE)
    )
  }
)

test_that(
  desc = "Read darug targ polypeptides syn attributes",
  code = {
    expect_match(
      as.character(parse_targ_poly_syn()
      [["syn"]][1]),
      "3.4.21.5,Coagulation factor II"
    )
    expect_error(parse_targ_poly_syn(TRUE))
  }
)

test_that(
  desc = "Read darug targ polypeptides pfams attributes",
  code = {
    expect_match(
      as.character(parse_drug_targ_polys_pfams()
      [["name"]][1]),
      "Gla"
    )
    expect_error(parse_drug_targ_polys_pfams(TRUE))
  }
)

test_that(
  desc = "Read darug targ polypeptides go classifiers attributes",
  code = {
    expect_match(
      as.character(parse_targ_poly_go()
      [["description"]][1]),
      "blood microparticle"
    )
    expect_error(parse_targ_poly_go(TRUE))
  }
)

test_that(
  desc = "Read darug targ attributes",
  code = {
    expect_match(
      as.character(parse_drug_targ()[["name"]][1]),
      "Prothrombin"
    )
    expect_error(parse_drug_targ(TRUE))
  }
)
