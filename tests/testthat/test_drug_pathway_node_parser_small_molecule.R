context("test small molecule drug pathway attributes parsers")

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
  desc = "Read drug pathway attributes",
  code = {
    expect_match(
      as.character(drug_pathway()[["name"]][[1]]),
      "Bivalirudin Action Pathway"
    )
    expect_true(is_tibble(drug_pathway()))
    expect_error(drug_pathway(TRUE))
  }
)

test_that(
  desc = "Read drug pathway drugs attributes",
  code = {
    expect_match(
      as.character(
        drug_pathway_drugs()[["name"]][[1]]
      ),
      "Bivalirudin"
    )
    expect_true(is_tibble(drug_pathway_drugs()))
    expect_error(drug_pathway_drugs(TRUE))
  }
)

test_that(
  desc = "Read drug pathway enzyme attributes",
  code = {
    expect_match(
      as.character(
        drug_pathway_enzyme()[["enzyme"]][[1]]
      ),
      "P00734"
    )
    expect_true(is_tibble(drug_pathway_enzyme()))
    expect_error(drug_pathway_enzyme(TRUE))
  }
)
