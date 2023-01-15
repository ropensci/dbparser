context("test biotech drug main attributes parsers")

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
  desc = "Read drug primary key attribute",
  code = {
    expect_match(drug_general_information()[1][["primary_key"]], "DB00001")
    expect_true(is_tibble(drug_general_information()))
  }
)

test_that(
  desc = "Read drug other keys attribute",
  code = {
    expect_match(drug_general_information()$other_keys, "BIOD00024")
  }
)

test_that(
  desc = "Read drug groups attributes",
  code = {
    expect_match(
      as.character(drug_groups()[1][["group"]]),
      "approved"
    )
    expect_true(is_tibble(drug_groups()))
  }
)


test_that(
  desc = "Read drug classification attributes",
  code = {
    expect_match(
      drug_classification()[["drugbank_id"]][[1]],
      "DB00001"
    )
    expect_true(is_tibble(drug_classification()))
  }
)

test_that(
  desc = "Read drug syn attributes",
  code = {
    expect_match(
      drug_syn()[["synonym"]][[1]],
      "Hirudin variant-1"
    )
    expect_true(is_tibble(drug_syn()))
  }
)

test_that(
  desc = "Read drug products attributes",
  code = {
    expect_match(
      as.character(drug_products()[["name"]][1]),
      "Refludan"
    )
    expect_true(is_tibble(drug_products()))
  }
)

test_that(
  desc = "Read drug mixtures attributes",
  code = {
    expect_match(
      as.character(drug_mixtures()[["name"]][1]),
      "Refludan"
    )
    expect_true(is_tibble(drug_mixtures()))
  }
)

test_that(
  desc = "Read drug packagers attributes",
  code = {
    expect_match(
      as.character(drug_packagers()[["name"]][1]),
      "Bayer Healthcare"
    )
    expect_true(is_tibble(drug_packagers()))
  }
)

test_that(
  desc = "Read drug manufacturers attributes",
  code = {
    expect_match(
      as.character(drug_manufacturers()[["manufacturer"]][[1]]),
      "Bayer healthcare pharmaceuticals inc"
    )
    expect_true(is_tibble(drug_manufacturers()))
  }
)

test_that(
  desc = "Read drug prices attributes",
  code = {
    expect_match(
      as.character(drug_prices()[["currency"]][[1]]),
      "USD"
    )
    expect_true(is_tibble(drug_prices()))
  }
)

test_that(
  desc = "Read drug categories attributes",
  code = {
    expect_match(
      as.character(drug_categories()[["mesh-id"]][[1]]),
      "D000602"
    )
    expect_true(is_tibble(drug_categories()))
  }
)

test_that(
  desc = "Read drug affected organisms attributes",
  code = {
    expect_match(
      as.character(
        drug_affected_organisms()[["affected_organism"]][[1]]
      ),
      "Humans and other mammals"
    )
    expect_true(is_tibble(drug_affected_organisms()))
  }
)

test_that(
  desc = "Read drug dosages attributes",
  code = {
    expect_match(
      as.character(drug_dosages()[["route"]][[1]]),
      "Intravenous"
    )
    expect_true(is_tibble(drug_dosages()))
  }
)

test_that(
  desc = "Read drug atc codes attributes",
  code = {
    expect_match(
      as.character(drug_atc_codes()[["atc_code"]][[1]]),
      "B01AE02"
    )
    expect_true(is_tibble(drug_atc_codes()))
  }
)

test_that(
  desc = "Read drug ahfs codes attributes",
  code = {
    expect_equal(
      nrow(drug_ahfs_codes()),
      0
    )
    expect_true(is_tibble(drug_ahfs_codes()))
  }
)

test_that(
  desc = "Read drug pdb entries attributes",
  code = {
    expect_equal(
      nrow(drug_pdb_entries()),
      0
    )
    expect_true(is_tibble(drug_pdb_entries()))
  }
)

test_that(
  desc = "Read drug patents attributes",
  code = {
    expect_match(
      as.character(drug_patents()[["country"]][[1]]),
      "United States"
    )
    expect_true(is_tibble(drug_patents()))
  }
)

test_that(
  desc = "Read drug interactions attributes",
  code = {
    expect_match(
      as.character(drug_interactions()[["name"]][[1]]),
      "St. John's Wort"
    )
    expect_true(is_tibble(drug_interactions()))
  }
)

test_that(
  desc = "Read drug food interactions attributes",
  code = {
    expect_equal(
      nrow(drug_food_interactions()),
      0
    )
    expect_true(is_tibble(drug_food_interactions()))
  }
)

test_that(
  desc = "Read drug sequences attributes",
  code = {
    expect_match(
      as.character(drug_sequences()[["format"]][[1]]),
      "FASTA"
    )
    expect_true(is_tibble(drug_sequences()))
  }
)

test_that(
  desc = "Read drug experimental properties attributes",
  code = {
    expect_match(
      as.character(drug_exp_prop()[["kind"]][[1]]),
      "Melting Point"
    )
    expect_true(is_tibble(drug_exp_prop()))
  }
)

test_that(
  desc = "Read drug external identifiers attributes",
  code = {
    expect_match(
      as.character(
        drug_ex_identity()[["resource"]][[1]]
      ),
      "Drugs Product Database \\(DPD\\)"
    )
    expect_true(is_tibble(drug_ex_identity()))
  }
)

test_that(
  desc = "Read drug external links attributes",
  code = {
    expect_match(
      as.character(drug_external_links()[["resource"]][[1]]),
      "RxList"
    )
    expect_true(is_tibble(drug_external_links()))
  }
)


test_that(
  desc = "Read drug snp effects attributes",
  code = {
    expect_equal(
      nrow(drug_snp_effects()),
      0
    )
    expect_true(is_tibble(drug_snp_effects()))
  }
)

test_that(
  desc = "Read drug snp adverse drug reactions attributes",
  code = {
    expect_equal(
      nrow(drug_snp_adverse_reactions()),
      0
    )
    expect_true(is_tibble(drug_snp_adverse_reactions()))
  }
)

test_that(
  desc = "Read drug pharmacology indication attribute",
  code = {
    expect_match(drug_pharmacology()[["indication"]][1],
                 "For the treatment of heparin-induced thrombocytopenia")
    expect_true(is_tibble(drug_pharmacology()))
  }
)
