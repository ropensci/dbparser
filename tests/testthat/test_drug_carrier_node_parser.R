context("test drug carrier attributes parsers")

library(dbparser)
library(testattributes)
library(XML)
library(tibble)
library(purrr)


test_attributes(desc = "Read database",
          code = {
            expect_true(get_xml_db_rows(
              system.file("extdata", "drugbank_record.xml", package = "dbparser")
            ))
          })

test_attributes(desc = "Read drug carriers actions attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_actions()),
                         0)
            expect_error(parse_drug_carriers_actions(TRUE))
          })

test_attributes(desc = "Read drug carriers articles attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_articles()),
                         0)
            expect_error(parse_drug_carriers_articles(TRUE))
          })

test_attributes(desc = "Read drug carriers_textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_textbooks()),
                         0)
            expect_error(parse_drug_carriers_textbooks(TRUE))
          })

test_attributes(desc = "Read drug carriers polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides()),
                         0)
            expect_error(parse_drug_carriers_polypeptides(TRUE))
          })

test_attributes(desc = "Read drug carriers polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_carriers_polypeptides_external_identifiers()
            ),
            0)
            expect_error(parse_drug_carriers_polypeptides_external_identifiers(TRUE))
          })

test_attributes(desc = "Read drug carriers polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_synonyms()),
                         0)
            expect_error(parse_drug_carriers_polypeptides_synonyms(TRUE))
          })

test_attributes(desc = "Read drug carriers polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_go_classifiers()),
                         0)
            expect_error(parse_drug_carriers_polypeptides_go_classifiers(TRUE))
          })

test_attributes(desc = "Read drug carriers attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers()),
                         0)
            expect_error(parse_drug_carriers(TRUE))
          })

test_attributes(desc = "Read drug carriers polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_pfams()),
                         0)
            expect_error(parse_drug_carriers_polypeptides_pfams(TRUE))
          })
