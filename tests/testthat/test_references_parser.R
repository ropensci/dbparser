context("test references parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)


biotech <- "drugbank_record_biotech.xml"
test_that(
  desc = "Read database",
  code = {
    expect_true(
      read_drugbank_xml_db(
        system.file("extdata", biotech, package = "dbparser")
      )
    )
  }
)

test_that(
  desc = "Read drug articles attributes",
  code = {
    expect_match(
      as.character(drugs_articles()[["pubmed-id"]][1]),
      "16244762"
    )
    expect_true(is_tibble(drugs_articles()))
    expect_error(drugs_articles(TRUE))
  }
)

test_that(
  desc = "Read drug books attributes",
  code = {
    expect_equal(nrow(drugs_textbooks()), 0)
    expect_true(is_tibble(drugs_textbooks()))
    expect_error(drugs_textbooks(TRUE))
  }
)

test_that(
  desc = "Read drug links attributes",
  code = {
    expect_match(
      as.character(drugs_links()[1][["title"]]),
      "Google books"
    )
    expect_true(is_tibble(drugs_links()))
    expect_error(drugs_links(TRUE))
  }
)

test_that(
  desc = "Read drug attachments attributes",
  code = {
    expect_equal(nrow(drugs_attachments()), 0)
    expect_true(is_tibble(drugs_attachments()))
    expect_error(drugs_attachments(TRUE))
  }
)

test_that(
  desc = "Read drug carriers articles",
  code = {
    expect_equal(
      nrow(carriers_articles()),
      0
    )
    expect_true(is_tibble(carriers_articles()))
    expect_error(carriers_articles(TRUE))
  }
)

test_that(
  desc = "Read drug carriers_textbooks",
  code = {
    expect_equal(
      nrow(carriers_textbooks()),
      0
    )
    expect_true(is_tibble(carriers_textbooks()))
    expect_error(carriers_textbooks(TRUE))
  }
)

test_that(
  desc = "Read drug carriers_links",
  code = {
    expect_equal(
      nrow(carriers_links()),
      0
    )
    expect_true(is_tibble(carriers_links()))
    expect_error(carriers_links(TRUE))
  }
)

test_that(
  desc = "Read drug carriers_attachments",
  code = {
    expect_equal(
      nrow(carriers_attachments()),
      0
    )
    expect_true(is_tibble(carriers_attachments()))
    expect_error(carriers_attachments(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes articles attributes",
  code = {
    expect_equal(
      nrow(enzymes_articles()),
      0
    )
    expect_true(is_tibble(enzymes_articles()))
    expect_error(enzymes_articles(TRUE))
  }
)


test_that(
  desc = "Read drug enzymes textbooks attributes",
  code = {
    expect_equal(
      nrow(enzymes_textbooks()),
      0
    )
    expect_true(is_tibble(enzymes_textbooks()))
    expect_error(enzymes_textbooks(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes links attributes",
  code = {
    expect_equal(
      nrow(enzymes_links()),
      0
    )
    expect_true(is_tibble(enzymes_links()))
    expect_error(enzymes_links(TRUE))
  }
)

test_that(
  desc = "Read drug enzymes attachments attributes",
  code = {
    expect_equal(
      nrow(enzymes_attachments()),
      0
    )
    expect_true(is_tibble(enzymes_attachments()))
    expect_error(enzymes_attachments(TRUE))
  }
)

test_that(
  desc = "Read drug targ_articles attributes",
  code = {
    expect_match(
      as.character(targets_articles()[["citation"]][[1]]),
      paste0(
        "coronary syndromes\\. Am J Cardiol\\. 1999 ",
        "Sep 2;84\\(5A\\):2M-6M\\."
      )
    )
    expect_true(is_tibble(targets_articles()))
    expect_error(targets_articles(TRUE))
  }
)

test_that(
  desc = "Read drug targ textbooks attributes",
  code = {
    expect_equal(
      nrow(targets_textbooks()),
      0
    )
    expect_true(is_tibble(targets_textbooks()))
    expect_error(targets_textbooks(TRUE))
  }
)

test_that(
  desc = "Read drug targ links attributes",
  code = {
    expect_equal(
      nrow(targets_links()),
      0
    )
    expect_true(is_tibble(targets_links()))
    expect_error(targets_links(TRUE))
  }
)

test_that(
  desc = "Read drug targ attachments",
  code = {
    expect_equal(
      nrow(targets_attachments()),
      0
    )
    expect_true(is_tibble(targets_attachments()))
    expect_error(targets_attachments(TRUE))
  }
)

test_that(
  desc = "Read drug transporters_articles attributes",
  code = {
    expect_equal(
      nrow(transporters_articles()),
      0
    )
    expect_true(is_tibble(transporters_articles()))
    expect_error(transporters_articlese(TRUE))
  }
)

test_that(
  desc = "Read drug transporters textbooks attributes",
  code = {
    expect_equal(
      nrow(transporters_textbooks()),
      0
    )
    expect_true(is_tibble(transporters_textbooks()))
    expect_error(transporters_textbooks(TRUE))
  }
)

test_that(
  desc = "Read drug transporters links attributes",
  code = {
    expect_equal(
      nrow(transporters_links()),
      0
    )
    expect_true(is_tibble(transporters_links()))
    expect_error(transporters_links(TRUE))
  }
)

test_that(
  desc = "Read drug transporters attachments attributes",
  code = {
    expect_equal(
      nrow(transporters_attachments()),
      0
    )
    expect_true(is_tibble(transporters_attachments()))
    expect_error(transporters_attachments(TRUE))
  }
)
