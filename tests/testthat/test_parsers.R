context("test parsers")

library(dbparser)
library(testthat)
library(XML)
library(tibble)
library(purrr)



test_that(desc = "Read database",
          code = {
            expect_true(get_xml_db_rows(
              system.file("extdata", "drugbank_record.xml", package = "dbparser")
            ))
          })
test_that(desc = "Read darug main attributes",
          code = {
            expect_match(parse_drug(FALSE)[1][["primary_key"]], "DB00001")
            expect_error(parse_drug())
          })

test_that(desc = "Read darug groups attributes",
          code = {
            expect_match(as.character(parse_drug_groups(FALSE)[1][["text"]]), "approved")
            expect_error(parse_drug_groups())
          })

test_that(desc = "Read darug articles attributes",
          code = {
            expect_match(as.character(parse_drug_articles(FALSE)[["pubmed-id"]][1]), "16244762")
            expect_error(parse_drug_articles())
          })

test_that(desc = "Read darug books attributes",
          code = {
            expect_equal(nrow(parse_drug_books(FALSE)), 0)
            expect_error(parse_drug_books())
          })

test_that(desc = "Read darug links attributes",
          code = {
            expect_match(as.character(parse_drug_links(FALSE)[1][["title"]]), "Google books")
            expect_error(parse_drug_links())
          })

test_that(desc = "Read darug classifications attributes",
          code = {
            expect_match(parse_drug_classfications(FALSE)[1][["parent_key"]], "DB00001")
            expect_error(parse_drug_classfications())
          })

test_that(desc = "Read darug synonyms attributes",
          code = {
            expect_match(parse_drug_synonyms(FALSE)[["synonym"]][[1]], "Hirudin variant-1")
            expect_error(parse_drug_synonyms())
          })

test_that(desc = "Read darug articles attributes",
          code = {
            expect_match(as.character(parse_drug_articles(FALSE)[["pubmed-id"]][1]), "16244762")
            expect_error(parse_drug_articles())
          })

test_that(desc = "Read darug products attributes",
          code = {
            expect_match(as.character(parse_drug_products(FALSE)[["name"]][1]), "Refludan")
            expect_error(parse_drug_products())
          })

test_that(desc = "Read darug mixtures attributes",
          code = {
            expect_match(as.character(parse_drug_mixtures(FALSE)[["name"]][1]), "Refludan")
            expect_error(parse_drug_mixtures())
          })

test_that(desc = "Read darug packagers attributes",
          code = {
            expect_match(as.character(parse_drug_packagers(FALSE)[["name"]][1]), "Bayer Healthcare")
            expect_error(parse_drug_packagers())
          })

test_that(desc = "Read darug manufacturers attributes",
          code = {
            expect_match(
              as.character(parse_drug_manufacturers(FALSE)[["text"]][[1]]),
              "Bayer healthcare pharmaceuticals inc"
            )
            expect_error(parse_drug_manufacturers())
          })

test_that(desc = "Read darug prices attributes",
          code = {
            expect_match(as.character(parse_drug_prices(FALSE)[["currency"]][[1]]),
                         "USD")
            expect_error(parse_drug_prices())
          })

test_that(desc = "Read darug categories attributes",
          code = {
            expect_match(as.character(parse_drug_categories(FALSE)[["mesh-id"]][[1]]),
                         "D000602")
            expect_error(parse_drug_categories())
          })

test_that(desc = "Read darug affected organisms attributes",
          code = {
            expect_match(as.character(parse_drug_affected_organisms(FALSE)[["text"]][[1]]),
                         "Humans and other mammals")
            expect_error(parse_drug_affected_organisms())
          })

test_that(desc = "Read darug dosages attributes",
          code = {
            expect_match(as.character(parse_drug_dosages(FALSE)[["route"]][[1]]),
                         "Intravenous")
            expect_error(parse_drug_dosages())
          })

test_that(desc = "Read darug atc codes attributes",
          code = {
            expect_match(as.character(parse_drug_atc_codes(FALSE)[["atc_code"]][[1]]),
                         "B01AE02")
            expect_error(parse_drug_atc_codes())
          })

test_that(desc = "Read darug ahfs codes attributes",
          code = {
            expect_equal(nrow(parse_drug_ahfs_codes(FALSE)),
                         0)
            expect_error(parse_drug_ahfs_codes())
          })

test_that(desc = "Read darug pdb entries attributes",
          code = {
            expect_equal(nrow(parse_drug_pdb_entries(FALSE)),
                         0)
            expect_error(parse_drug_pdb_entries())
          })

test_that(desc = "Read darug patents attributes",
          code = {
            expect_match(as.character(parse_drug_patents(FALSE)[["country"]][[1]]),
                         "United States")
            expect_error(parse_drug_patents())
          })

test_that(desc = "Read darug interactions attributes",
          code = {
            expect_match(as.character(parse_drug_interactions(FALSE)[["name"]][[1]]),
                         "St. John's Wort")
            expect_error(parse_drug_interactions())
          })

test_that(desc = "Read darug food interactions attributes",
          code = {
            expect_equal(nrow(parse_drug_food_interactions(FALSE)),
                         0)
            expect_error(parse_drug_food_interactions())
          })

test_that(desc = "Read darug sequences attributes",
          code = {
            expect_match(as.character(parse_drug_sequences(FALSE)[["format"]][[1]]),
                         "FASTA")
            expect_error(parse_drug_sequences())
          })

test_that(desc = "Read darug experimental properties attributes",
          code = {
            expect_match(as.character(parse_drug_experimental_properties(FALSE)[["kind"]][[1]]),
                         "Melting Point")
            expect_error(parse_drug_experimental_properties())
          })

test_that(desc = "Read darug external identifiers attributes",
          code = {
            expect_match(
              as.character(parse_drug_external_identifiers(FALSE)[["resource"]][[1]]),
              "Drugs Product Database \\(DPD\\)"
            )
            expect_error(parse_drug_external_identifiers())
          })

test_that(desc = "Read darug external links attributes",
          code = {
            expect_match(as.character(parse_drug_external_links(FALSE)[["resource"]][[1]]),
                         "RxList")
            expect_error(parse_drug_external_links())
          })

test_that(desc = "Read darug pathway attributes",
          code = {
            expect_match(as.character(parse_drug_pathway(FALSE)[["name"]][[1]]),
                         "Lepirudin Action Pathway")
            expect_error(parse_drug_pathway())
          })

test_that(desc = "Read darug pathway drugs attributes",
          code = {
            expect_match(as.character(parse_drug_pathway_drugs(FALSE)[["name"]][[1]]),
                         "Lepirudin")
            expect_error(parse_drug_pathway_drugs())
          })

test_that(desc = "Read darug pathway enzyme attributes",
          code = {
            expect_match(as.character(parse_drug_pathway_enzyme(FALSE)[["text"]][[1]]),
                         "P00734")
            expect_error(parse_drug_pathway_enzyme())
          })

test_that(desc = "Read darug snp effects attributes",
          code = {
            expect_equal(nrow(parse_drug_snp_effects(FALSE)),
                         0)
            expect_error(parse_drug_snp_effects())
          })

test_that(desc = "Read darug snp adverse drug reactions attributes",
          code = {
            expect_equal(nrow(parse_drug_snp_adverse_drug_reactions(FALSE)),
                         0)
            expect_error(parse_drug_snp_adverse_drug_reactions())
          })

test_that(desc = "Read darug enzymes attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes(FALSE)),
                         0)
            expect_error(parse_drug_enzymes())
          })

test_that(desc = "Read darug enzymes actions attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_actions(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_actions())
          })

test_that(desc = "Read darug targets actions attributes",
          code = {
            expect_match(as.character(parse_drug_targets_actions(FALSE)[["text"]][[1]]),
                         "inhibitor")
            expect_error(parse_drug_targets_actions())
          })

test_that(desc = "Read darug carriers actions attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_actions (FALSE)),
                         0)
            expect_error(parse_drug_carriers_actions ())
          })

test_that(desc = "Read darug transporters actions attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_actions(FALSE)),
                         0)
            expect_error(parse_drug_transporters_actions())
          })

test_that(desc = "Read darug enzymes articles attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_articles(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_articles())
          })

test_that(desc = "Read darug targets_articles attributes",
          code = {
            expect_match(
              as.character(parse_drug_targets_articles(FALSE)[["citation"]][[1]]),
              "coronary syndromes\\. Am J Cardiol\\. 1999 Sep 2;84\\(5A\\):2M-6M\\."
            )
            expect_error(parse_drug_targets_articles())
          })

test_that(desc = "Read darug carriers articles attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_articles(FALSE)),
                         0)
            expect_error(parse_drug_carriers_articles())
          })

test_that(desc = "Read darug transporters_articles attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_articles(FALSE)),
                         0)
            expect_error(parse_drug_transporters_articlese())
          })

test_that(desc = "Read darug targets textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_targets_textbooks(FALSE)),
                         0)
            expect_error(parse_drug_targets_textbooks())
          })

test_that(desc = "Read darug carriers_textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_textbooks(FALSE)),
                         0)
            expect_error(parse_drug_carriers_textbooks())
          })

test_that(desc = "Read darug transporters textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_textbooks(FALSE)),
                         0)
            expect_error(parse_drug_transporters_textbooks())
          })

test_that(desc = "Read darug enzymes textbooks attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_textbooks(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_textbooks())
          })

test_that(desc = "Read darug enzymes links attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_links(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_links())
          })

test_that(desc = "Read darug targets links attributes",
          code = {
            expect_equal(nrow(parse_drug_targets_links(FALSE)),
                         0)
            expect_error(parse_drug_targets_links())
          })

test_that(desc = "Read darug carriers links attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_links(FALSE)),
                         0)
            expect_error(parse_drug_carriers_links())
          })

test_that(desc = "Read darug transporters links attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_links(FALSE)),
                         0)
            expect_error(parse_drug_transporters_links())
          })

test_that(desc = "Read darug enzymes polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_polypeptides())
          })

test_that(desc = "Read darug targets polypeptides attributes",
          code = {
            expect_match(as.character(parse_drug_targets_polypeptides(FALSE)[["name"]][[1]]),
                         "Prothrombin")
            expect_error(parse_drug_targets_polypeptides())
          })

test_that(desc = "Read darug carriers polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides(FALSE)),
                         0)
            expect_error(parse_drug_carriers_polypeptides())
          })

test_that(desc = "Read darug transporters polypeptides attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides(FALSE)),
                         0)
            expect_error(parse_drug_transporters_polypeptides())
          })

test_that(desc = "Read darug enzymes polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_enzymes_polypeptides_external_identifiers(FALSE)
            ),
            0)
            expect_error(parse_drug_enzymes_polypeptides_external_identifiers())
          })

test_that(desc = "Read darug targets polypeptides external identifiers attributes",
          code = {
            expect_match(
              as.character(
                parse_drug_targets_polypeptides_external_identifiers(FALSE)[["identifier"]][1]
              ),
              "HGNC:3535"
            )
            expect_error(parse_drug_targets_polypeptides_external_identifiers())
          })

test_that(desc = "Read darug carriers polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_carriers_polypeptides_external_identifiers(FALSE)
            ),
            0)
            expect_error(parse_drug_carriers_polypeptides_external_identifiers())
          })

test_that(desc = "Read darug transporters polypeptides external identifiers attributes",
          code = {
            expect_equal(nrow(
              parse_drug_transporters_polypeptides_external_identifiers(FALSE)
            ),
            0)
            expect_error(parse_drug_transporters_polypeptides_external_identifiers())
          })

test_that(desc = "Read darug enzymes polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_synonyms(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_synonyms())
          })

test_that(desc = "Read darug targets polypeptides synonyms attributes",
          code = {
            expect_match(
              as.character(parse_drug_targets_polypeptides_synonyms(FALSE)[["synonyms"]][1]),
              "3.4.21.5,Coagulation factor II"
            )
            expect_error(parse_drug_targets_polypeptides_synonyms())
          })

test_that(desc = "Read darug carriers polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_synonyms(FALSE)),
                         0)
            expect_error(parse_drug_carriers_polypeptides_synonyms())
          })

test_that(desc = "Read darug transporters polypeptides synonyms attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides_synonyms(FALSE)),
                         0)
            expect_error(parse_drug_transporters_polypeptides_synonyms())
          })

test_that(desc = "Read darug enzymes polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_pfams(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_pfams())
          })

test_that(desc = "Read darug carriers polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_pfams(FALSE)),
                         0)
            expect_error(parse_drug_carriers_polypeptides_pfams())
          })

test_that(desc = "Read darug targets polypeptides pfams attributes",
          code = {
            expect_match(as.character(parse_drug_targets_polypeptides_pfams(FALSE)[["name"]][1]),
                         "Gla")
            expect_error(parse_drug_targets_polypeptides_pfams())
          })

test_that(desc = "Read darug transporters polypeptides pfams attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides_pfams(FALSE)),
                         0)
            expect_error(parse_drug_transporters_polypeptides_pfams())
          })

test_that(desc = "Read darug enzymes polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(parse_drug_enzymes_polypeptides_go_classifiers(FALSE)),
                         0)
            expect_error(parse_drug_enzymes_polypeptides_go_classifiers())
          })

test_that(desc = "Read darug targets polypeptides go classifiers attributes",
          code = {
            expect_match(as.character(parse_drug_targets_polypeptides_go_classifiers(FALSE)[["description"]][1]),
                         "blood microparticle")
            expect_error(parse_drug_targets_polypeptides_go_classifiers())
          })

test_that(desc = "Read darug carriers polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers_polypeptides_go_classifiers(FALSE)),
                         0)
            expect_error(parse_drug_carriers_polypeptides_go_classifiers())
          })

test_that(desc = "Read darug transporters polypeptides go classifiers attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters_polypeptides_go_classifiers(FALSE)),
                         0)
            expect_error(parse_drug_transporters_polypeptides_go_classifiers())
          })

test_that(desc = "Read darug reactions attributes",
          code = {
            expect_equal(nrow(parse_drug_reactions(FALSE)),
                         0)
            expect_error(parse_drug_reactions())
          })

test_that(desc = "Read darug reactions enzymes attributes",
          code = {
            expect_equal(nrow(parse_drug_reactions_enzymes(FALSE)),
                         0)
            expect_error(parse_drug_reactions_enzymes())
          })

test_that(desc = "Read darug carriers attributes",
          code = {
            expect_equal(nrow(parse_drug_carriers(FALSE)),
                         0)
            expect_error(parse_drug_carriers())
          })

test_that(desc = "Read darug transporters attributes",
          code = {
            expect_equal(nrow(parse_drug_transporters(FALSE)),
                         0)
            expect_error(parse_drug_transporters())
          })

test_that(desc = "Read darug targets attributes",
          code = {
            expect_match(as.character(parse_drug_targets(FALSE)[["name"]][1]),
                         "Prothrombin")
            expect_error(parse_drug_targets())
          })
