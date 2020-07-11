context("test small molecule cett poly other parsers")

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
  desc = "Read drug carriers polypeptides external identifiers",
  code = {
    expect_equal(
      nrow(carriers_polypep_ex_ident()),
      0
    )
    expect_true(is_tibble(carriers_polypep_ex_ident()))
    expect_error(
      carriers_polypep_ex_ident(TRUE)
    )
  }
)

test_that(
  desc = "Read drug carriers polypeptides syn",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_syn()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_syn()))
    expect_error(carriers_polypeptides_syn(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides go classifiers",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_go()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_go()))
    expect_error(carriers_polypeptides_go(TRUE))
  }
)

test_that(
  desc = "Read drug carriers polypeptides pfams",
  code = {
    expect_equal(
      nrow(carriers_polypeptides_pfams()),
      0
    )
    expect_true(is_tibble(carriers_polypeptides_pfams()))
    expect_error(carriers_polypeptides_pfams(TRUE))
  }
)

test_that(
  desc =
    "Read drug enzymes polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(
        enzymes_polypep_ex_ident()
      ),
      7
    )
    expect_true(is_tibble(enzymes_polypep_ex_ident()))
    expect_error(
      enzymes_polypep_ex_ident(TRUE)
    )
  }
)


test_that(
  desc = "Read drug enzymes polypeptides syn attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_syn()),
      2
    )
    expect_true(is_tibble(enzymes_polypeptides_syn()))
    expect_error(enzymes_polypeptides_syn(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes polypeptides pfams attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_pfams()),
      1
    )
    expect_true(is_tibble(enzymes_polypeptides_pfams()))
    expect_error(enzymes_polypeptides_pfams(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(enzymes_polypeptides_go()),
      24
    )
    expect_true(is_tibble(enzymes_polypeptides_go()))
    expect_error(enzymes_polypeptides_go(TRUE))
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
  desc = "Read drug transporters polypeptides external identifiers attributes",
  code = {
    expect_equal(
      nrow(transporters_polypep_ex_ident()),
      0
    )
    expect_true(is_tibble(transporters_polypep_ex_ident()))
    expect_error(
      transporters_polypep_ex_ident(TRUE)
    )
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
    expect_error(transporters_polypeptides_syn(TRUE))
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
    expect_error(transporters_polypeptides_pfams(TRUE))
  }
)

test_that(
  desc = "Read drug transporters polypeptides go classifiers attributes",
  code = {
    expect_equal(
      nrow(transporters_polypeptides_go()), 0
    )
    expect_true(is_tibble(transporters_polypeptides_go()))
    expect_error(transporters_polypeptides_go(TRUE))
  }
)
