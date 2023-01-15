context("test biotech cett poly others parsers")

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
  desc = "Read drug carriers polypeptides external identifiers that",
  code = {
    expect_equal(
      nrow(
        carriers_polypep_ex_ident()
      ),
      0
    )
    expect_true(is_tibble(carriers_polypep_ex_ident()))
  }
)

test_that(
  desc = "Read drug carriers polypeptides syn that",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_syn()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_syn()))
  }
)

test_that(
  desc = "Read drug carriers polypeptides go classifiers that",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_go()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_go()))
  }
)

test_that(
  desc = "Read drug carriers polypeptides pfams that",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_pfams()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_pfams()))
  }
)

test_that(
  desc =
    "Read drug enzymes polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypep_ex_ident()),
      0
    )
    expect_true(is_tibble(enzymes_polypep_ex_ident()))
  }
)


test_that(
  desc = "Read drug enzymes polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_syn()),
      0
    )
    expect_true(is_tibble(enzymes_polypeptides_syn()))
  }
)

test_that(
  desc = "Read drug enzymes polypeptides pfams attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_pfams()),
      0
    )
    expect_true(is_tibble(enzymes_polypeptides_pfams()))
  }
)

test_that(
  desc = "Read drug enzymes polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_go()),
      0
    )
    expect_true(is_tibble(enzymes_polypeptides_go()))
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
  }
)

test_that(
  desc = "Read drug transporters polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(transporters_polypep_ex_ident()),
      0
    )
    expect_true(is_tibble(transporters_polypep_ex_ident()))
  }
)

test_that(
  desc = "Read drug transporters polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptides_syn()),
      0
    )
    expect_true(is_tibble(transporters_polypeptides_syn()))
  }
)

test_that(
  desc = "Read drug transporters polypeptides pfams attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptides_pfams()),
      0
    )
    expect_true(is_tibble(transporters_polypeptides_pfams()))
  }
)

test_that(
  desc = "Read drug transporters polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptides_go()),
      0
    )
    expect_true(is_tibble(transporters_polypeptides_go()))
  }
)

