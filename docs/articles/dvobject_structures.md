# dvobject structures by parser

Every parser in `dbparser` returns a `dvobject`: a named list of tibbles
(DrugBank’s are nested in lists by section) and a `source_metadata`
record. This page lists, for each parser, the tables it returns, their
columns and column types, the keys the parser checks, and the source
record.

Nothing on this page is typed by hand. Each section runs the parser on
the small example files bundled with the package (`inst/extdata`) when
the page is built, and reads the tables, columns and types from the
result. Row counts are those of the example files, not of the full
databases.

The `source_metadata` fields are the same for every parser. Fields the
source does not state, or that the caller did not pass (such as
`retrieved_at`), are `NA`; nothing is guessed. They are read with
[`get_source_metadata()`](https://docs.ropensci.org/dbparser/reference/get_source_metadata.md).

## DrugBank: `parseDrugBank()`

The drug records of the DrugBank XML, grouped in lists: `drugs`
(always), and with their options `salts`, `products`, `references`
(articles, books, links and attachments, for drugs and for carriers,
enzymes, targets and transporters) and `cett` (carriers, enzymes,
targets and transporters, with their polypeptides and actions). Here
every option is on, on the bundled small-molecule record.

The parser checks no key. Drug tables carry `drugbank_id`; CETT tables
carry `drugbank_id` and the CETT id (e.g. `target_id`).

| table | rows | columns |
|:---|---:|:---|
| drugs\$general_information \| 1\|\`drugbank_id\` (character), \`other_keys\` (character), \`type\` (character), \`created\` (character), \`updated\` (character), \`name\` (character), \`description\` (character), \`cas_number\` (character), \`unii\` (character), \`average_mass\` (character), \`monoisotopic_mass\` (character), \`state\` (character), \`synthesis_reference\` (character), \`fda_label\` (character), \`msds\` (character) \| \|drugs\$drug_classification | 1 | `description` (character), `direct_parent` (character), `kingdom` (character), `superclass` (character), `class` (character), `subclass` (character), `alternative_parents` (character), `substituents` (character), `drugbank_id` (character) |
| drugs\$synonyms \| 3\|\`synonym\` (character), \`language\` (character), \`coder\` (character), \`drugbank_id\` (character) \| \|drugs\$pharmacology | 1 | `drugbank_id` (character), `indication` (character), `pharmacodynamics` (character), `mechanism_of_action` (character), `toxicity` (character), `metabolism` (character), `absorption` (character), `half_life` (character), `protein_binding` (character), `route_of_elimination` (character), `volume_of_distribution` (character), `clearance` (character) |
| drugs\$international_brands \| 2\|\`brand\` (character), \`company\` (character), \`drugbank_id\` (character) \| \|drugs\$mixtures | 0 | `` () | |drugs$packagers | 4|`name` (character), `url` (character), `drugbank_id` (character) | |drugs$manufacturers | 1|`manufacturer` (character), `generic` (character), `url` (character), `drugbank_id` (character) | |drugs$prices | 1|`description` (character), `currency` (character), `cost` (character), `unit` (character), `drugbank_id` (character) | |drugs$categories | 10|`category` (character), `mesh_id` (character), `drugbank_id` (character) | |drugs$dosages | 17|`form` (character), `route` (character), `strength` (character), `drugbank_id` (character) | |drugs$atc_codes | 1|`atc_code` (character), `level_1` (character), `code_1` (character), `level_2` (character), `code_2` (character), `level_3` (character), `code_3` (character), `level_4` (character), `code_4` (character), `drugbank_id` (character) | |drugs$patents | 4|`number` (character), `country` (character), `approved` (character), `expires` (character), `pediatric_extension` (character), `drugbank_id` (character) | |drugs$drug_interactions | 639|`target_drugbank_id` (character), `name` (character), `description` (character), `drugbank_id` (character) | |drugs$sequences | 0| `` () |
| drugs\$calculated_properties \| 26\|\`kind\` (character), \`value\` (character), \`source\` (character), \`drugbank_id\` (character) \| \|drugs\$experimental_properties | 0 | `` () | |drugs$external_identifiers | 12|`resource` (character), `identifier` (character), `drugbank_id` (character) | |drugs$pathway$general_information | 1|`smpdb_id` (character), `name` (character), `category` (character), `drugbank_id` (character) | |drugs$pathway$pathway_drugs | 3|`drugbank_id` (character), `name` (character), `smpdb_id` (character) | |drugs$pathway$pathway_enzyme | 20|`enzyme` (character), `smpdb_id` (character) | |drugs$reactions$general_information | 0| `` () |
| drugs$`reactions`$reactions_enzymes | 0 | `() | |drugs$snp_effects | 0|` () |
| drugs\$snp_adverse_reactions \| 0\|\`\` () \| \|drugs\$food_interactions | 2 | `food_interaction` (character), `drugbank_id` (character) |
| drugs\$pdb_entries \| 0\|\`\` () \| \|drugs\$ahfs_codes | 0 | `` () | |drugs$affected_organisms | 1|`affected_organism` (character), `drugbank_id` (character) | |drugs$groups | 2|`group` (character), `drugbank_id` (character) | |drugs$external_links | 2|`resource` (character), `url` (character), `drugbank_id` (character) | |salts | 0| `` () |
| products | 34 | `name` (character), `labeller` (character), `ndc_id` (character), `ndc_product_code` (character), `dpd_id` (character), `ema_product_code` (character), `ema_ma_number` (character), `started_marketing_on` (character), `ended_marketing_on` (character), `dosage_form` (character), `strength` (character), `route` (character), `fda_application_number` (character), `generic` (character), `over_the_counter` (character), `approved` (character), `country` (character), `source` (character), `drugbank_id` (character) |
| references$`drugs`$books | 0 | `` () | |references$drugs$articles | 7|`ref_id` (character), `pubmed_id` (character), `citation` (character), `drugbank_id` (character) | |references$drugs$links | 0| `` () |
| references$`drugs`$attachments | 0 | `() | |references$carriers$books | 0|` () |
| references$`carriers`$articles | 0 | `() | |references$carriers$links | 0|` () |
| references$`carriers`$attachments | 0 | `() | |references$enzymes$books | 0|` () |
| references$`enzymes`$articles | 1 | `ref_id` (character), `pubmed_id` (character), `citation` (character), `enzyme_id` (character) |
| references$`enzymes`$links | 0 | `() | |references$enzymes$attachments | 0|` () |
| references$`targets`$books | 0 | `` () | |references$targets$articles | 6|`ref_id` (character), `pubmed_id` (character), `citation` (character), `target_id` (character) | |references$targets$links | 0| `` () |
| references$`targets`$attachments | 0 | `() | |references$transporters$books | 0|` () |
| references$`transporters`$articles | 0 | `() | |references$transporters$links | 0|` () |
| references$`transporters`$attachments | 0 | `() | |cett$carriers$general_information | 0|` () |
| cett$`carriers`$actions | 0 | `() | |cett$carriers$polypeptides$general_information | 0|` () |
| cett$`carriers`$polypeptides\$external_identy \| 0\|\`\` () \| \|cett\$carriers$`polypeptides`$synonyms | 0 | `() | |cett$carriers$polypeptides$pfams | 0|` () |
| cett$`carriers`$polypeptides\$go \| 0\|\`\` () \| \|cett\$enzymes\$general_information \| 1\|\`enzyme_id\` (character), \`name\` (character), \`organism\` (character), \`known_action\` (character), \`position\` (character), \`drugbank_id\` (character), \`inhibition_strength\` (character), \`induction_strength\` (character) \| \|cett\$enzymes\$actions \| 1\|\`action\` (character), \`enzyme_id\` (character), \`drugbank_id\` (character) \| \|cett\$enzymes$`polypeptides`$general_information | 1 | `polypeptide_id` (character), `source` (character), `name` (character), `general_function` (character), `specific_function` (character), `gene_name` (character), `locus` (character), `cellular_location` (character), `transmembrane_regions` (character), `signal_regions` (character), `theoretical_pi` (character), `molecular_weight` (character), `chromosome_location` (character), `organism` (character), `organism_ncbi_taxonomy_id` (character), `amino_acid_sequence` (character), `amino_acid_format` (character), `gene_sequence` (character), `gene_format` (character), `enzyme_id` (character), `drugbank_id` (character) |
| cett$`enzymes`$polypeptides\$external_identy \| 7\|\`resource\` (character), \`identifier\` (character), \`enzyme_id\` (character) \| \|cett\$enzymes$`polypeptides`$synonyms | 2 | `synonym` (character), `enzyme_id` (character) |
| cett$`enzymes`$polypeptides\$pfams \| 1\|\`identifier\` (character), \`name\` (character), \`enzyme_id\` (character) \| \|cett\$enzymes$`polypeptides`$go | 24 | `category` (character), `description` (character), `enzyme_id` (character) |
| cett$`targets`$general_information | 1 | `target_id` (character), `name` (character), `organism` (character), `known_action` (character), `position` (character), `drugbank_id` (character) |
| cett$`targets`$actions | 1 | `action` (character), `target_id` (character), `drugbank_id` (character) |
| cett$`targets`$polypeptides\$general_information \| 1\|\`polypeptide_id\` (character), \`source\` (character), \`name\` (character), \`general_function\` (character), \`specific_function\` (character), \`gene_name\` (character), \`locus\` (character), \`cellular_location\` (character), \`transmembrane_regions\` (character), \`signal_regions\` (character), \`theoretical_pi\` (character), \`molecular_weight\` (character), \`chromosome_location\` (character), \`organism\` (character), \`organism_ncbi_taxonomy_id\` (character), \`amino_acid_sequence\` (character), \`amino_acid_format\` (character), \`gene_sequence\` (character), \`gene_format\` (character), \`target_id\` (character), \`drugbank_id\` (character) \| \|cett\$targets$`polypeptides`$external_identy | 7 | `resource` (character), `identifier` (character), `target_id` (character) |
| cett$`targets`$polypeptides\$synonyms \| 2\|\`synonym\` (character), \`target_id\` (character) \| \|cett\$targets$`polypeptides`$pfams | 4 | `identifier` (character), `name` (character), `target_id` (character) |
| cett$`targets`$polypeptides\$go \| 46\|\`category\` (character), \`description\` (character), \`target_id\` (character) \| \|cett\$transporters\$general_information \| 0\|\`\` () \| \|cett\$transporters\$actions \| 0\|\`\` () \| \|cett\$transporters$`polypeptides`$general_information | 0 | `() | |cett$transporters$polypeptides$external_identy | 0|` () |
| cett$`transporters`$polypeptides\$synonyms \| 0\|\`\` () \| \|cett\$transporters$`polypeptides`$pfams | 0 | `() | |cett$transporters$polypeptides$go | 0|` () |

| field | value |
|:---|:---|
| metadata_version | 1 |
| source | drugbank |
| source_name | DrugBank |
| release | 5.1 |
| release_date | 2018-07-03 |
| retrieved_at | NA |
| source_url | <https://go.drugbank.com/> |
| licence | NA |
| licence_url | <https://trust.drugbank.com/drugbank-trust-center/drugbank-terms-of-service> |
| attribution | Knox C, Wilson M, Klinger CM, et al. DrugBank 6.0: the DrugBank Knowledgebase for 2024. Nucleic Acids Res. 2024 Jan 5;52(D1):D1265-D1275. doi: 10.1093/nar/gkad976. |
| parser | dbparser 2.3.0 |

Files read, with their SHA-256 hashes:

| name | sha256 |
|:---|:---|
| drugbank_record_small_molecule.xml | e39ffed5573ebeea58dce57dda89a6f1666cab8476515c9c82cb8d654b035ef5 |

## OnSIDES: `parseOnSIDES()`

The seven core relational tables of OnSIDES, named after their files,
and `high_confidence` when that file is present.

The parser checks no key.

| table | rows | columns |
|:---|---:|:---|
| product_label | 29 | `label_id` (integer), `source` (character), `source_product_name` (character), `source_product_id` (character), `source_label_url` (character) |
| product_adverse_effect | 18742 | `product_label_id` (integer), `effect_id` (integer), `label_section` (character), `effect_meddra_id` (integer), `match_method` (character), `pred0` (numeric), `pred1` (numeric) |
| product_to_rxnorm | 44 | `label_id` (integer), `rxnorm_product_id` (character) |
| vocab_meddra_adverse_effect | 139 | `meddra_id` (integer), `meddra_name` (character), `meddra_term_type` (character) |
| vocab_rxnorm_ingredient | 1 | `rxnorm_id` (integer), `rxnorm_name` (character), `rxnorm_term_type` (character) |
| vocab_rxnorm_product | 3 | `rxnorm_id` (character), `rxnorm_name` (character), `rxnorm_term_type` (character) |
| vocab_rxnorm_ingredient_to_product | 3 | `product_id` (character), `ingredient_id` (integer) |
| high_confidence | 2 | `ingredient_id` (integer), `effect_meddra_id` (integer) |

| field | value |
|:---|:---|
| metadata_version | 1 |
| source | onsides |
| source_name | OnSIDES |
| release | NA |
| release_date | NA |
| retrieved_at | NA |
| source_url | <https://github.com/tatonetti-lab/onsides> |
| licence | CC-BY-4.0 |
| licence_url | <https://github.com/tatonetti-lab/onsides/blob/main/LICENSE-DATA> |
| attribution | Tanaka Y, Chen HY, Belloni P, Gisladottir U, Kefeli J, Patterson J, Srinivasan A, Zietz M, Sirdeshmukh G, Berkowitz J, LaRow Brown K, Tatonetti NP. OnSIDES database: Extracting adverse drug events from drug labels using natural language processing models. Med. 2025 Mar 27:100642. doi: 10.1016/j.medj.2025.100642. |
| parser | dbparser 2.3.0 |

Files read, with their SHA-256 hashes:

| name | sha256 |
|:---|:---|
| product_label.csv | 12e658df056e625bfccf0e438e875782dadd37e557b99d6ea0a7d813dbab31b4 |
| product_adverse_effect.csv | be04cd9fad75194ba448865d052a1ed3d0370d59597fa36067615798dcdff66a |
| product_to_rxnorm.csv | 504204dd9e8906a3df61a728af4d431325e068093b62d5aa78eae3a8d64cb266 |
| vocab_meddra_adverse_effect.csv | e924234bcdd406305132cdf8de138402535ce0d5a85aca56ef8248ffc7254aab |
| vocab_rxnorm_ingredient.csv | a23eeb6e6d0896ada63970f81b4107672e4b7f031e157fb0505f372feae5a80a |
| vocab_rxnorm_product.csv | 58a586a325282294784065e835f5a885b8ae2a3372177ab0d5d50d49824c8767 |
| vocab_rxnorm_ingredient_to_product.csv | 942644abc1c1b05d3c3b51f3ea3796f07be96de8e5b8032151d8790b2a63d253 |
| high_confidence.csv | 63747425e0fb14a2b4344116fafdb98494a3735f146c2e93bd7e6a24051d8507 |

## TWOSIDES: `parseTWOSIDES()`

The drug-drug interaction signals of TWOSIDES, one table.

The parser checks no key.

| table | rows | columns |
|:---|---:|:---|
| drug_drug_interactions | 1706 | `drug_1_rxnorn_id` (integer), `drug_1_concept_name` (character), `drug_2_rxnorm_id` (integer), `drug_2_concept_name` (character), `condition_meddra_id` (integer), `condition_concept_name` (character), `A` (integer), `B` (integer), `C` (integer), `D` (integer), `PRR` (numeric), `PRR_error` (numeric), `mean_reporting_frequency` (numeric) |

| field | value |
|:---|:---|
| metadata_version | 1 |
| source | twosides |
| source_name | TWOSIDES |
| release | NA |
| release_date | NA |
| retrieved_at | NA |
| source_url | <https://tatonettilab-resources.s3.amazonaws.com/nsides/TWOSIDES.csv.gz> |
| licence | NA |
| licence_url | NA |
| attribution | NA |
| parser | dbparser 2.3.0 |

Files read, with their SHA-256 hashes:

| name | sha256 |
|:---|:---|
| twoside_raw.csv | 2d8057c0989350e3c2ee037df01004d79a7d55ef5f07b7fb07e69b8ca9bfed17 |

## HPO: `parseHPO()`

Human Phenotype Ontology gene-phenotype associations as distributed by
Harmonizome, with one row per gene and one per phenotype in the lookup
tables.

The parser checks no key; `associations` holds distinct rows.

| table | rows | columns |
|:---|---:|:---|
| associations | 485 | `gene_symbol` (character), `entrez_id` (character), `hpo_id` (character), `phenotype_name` (character), `association_weight` (numeric) |
| genes | 7 | `gene_symbol` (character), `entrez_id` (character), `n_phenotypes` (integer) |
| phenotypes | 352 | `hpo_id` (character), `phenotype_name` (character), `n_genes` (integer) |

| field | value |
|:---|:---|
| metadata_version | 1 |
| source | hpo |
| source_name | Human Phenotype Ontology (Harmonizome) |
| release | NA |
| release_date | NA |
| retrieved_at | NA |
| source_url | <https://maayanlab.cloud/Harmonizome/> |
| licence | CC-BY-NC-SA-4.0 |
| licence_url | <https://maayanlab.cloud/Harmonizome/terms> |
| attribution | Diamant I, Clarke DJB, Evangelista JE, Lingam N, Ma’ayan A. Harmonizome 3.0: integrated knowledge about genes and proteins from diverse multi-omics resources. Nucleic Acids Res. 2024 Nov 20. pii: 53(1):D1016-D1028. Rouillard AD, Gundersen GW, Fernandez NF, Wang Z, Monteiro CD, McDermott MG, Ma’ayan A. The harmonizome: a collection of processed datasets gathered to serve and mine knowledge about genes and proteins. Database (Oxford). 2016 Jul 3;2016. pii: baw100. |
| parser | dbparser 2.3.0 |

Files read, with their SHA-256 hashes:

| name | sha256 |
|:---|:---|
| hpo_gene_attribute_edges.txt | 1e8065ca4f68ba2522547d6233da5943405d36f8aa1f3ed7f40431e3b07f67d6 |

## HGNC: `parseHGNC()`

The HGNC complete set (`gene`), every symbol and name of each gene
(`gene_name`) and the withdrawn entries (`withdrawn`).

The parser stops unless `hgnc_id` is set and unique in `gene`. On the
example files:

    #>     gene_hgnc_id_unique gene_name_ids_are_genes 
    #>                    TRUE                    TRUE

| table | rows | columns |
|:---|---:|:---|
| gene | 24 | `hgnc_id` (character), `symbol` (character), `name` (character), `locus_group` (character), `locus_type` (character), `status` (character), `location` (character), `alias_symbol` (character), `alias_name` (character), `prev_symbol` (character), `prev_name` (character), `gene_group` (character), `gene_group_id` (character), `date_approved_reserved` (Date), `date_symbol_changed` (Date), `date_name_changed` (Date), `date_modified` (Date), `entrez_id` (character), `ensembl_gene_id` (character), `vega_id` (character), `ucsc_id` (character), `ena` (character), `refseq_accession` (character), `ccds_id` (character), `uniprot_ids` (character), `pubmed_id` (character), `mgd_id` (character), `rgd_id` (character), `lsdb` (character), `cosmic` (character), `omim_id` (character), `mirbase` (character), `homeodb` (character), `snornabase` (character), `bioparadigms_slc` (character), `orphanet` (character), `pseudogene_org` (character), `horde_id` (character), `merops` (character), `imgt` (character), `iuphar` (character), `kznf_gene_catalog` (character), `mamit_trnadb` (character), `cd` (character), `lncrnadb` (character), `enzyme_id` (character), `intermediate_filament_db` (character), `rna_central_id` (character), `lncipedia` (character), `gtrnadb` (character), `agr` (character), `mane_select` (character), `gencc` (character) |
| gene_name | 215 | `hgnc_id` (character), `kind` (character), `name` (character) |
| withdrawn | 14 | `hgnc_id` (character), `status` (character), `withdrawn_symbol` (character), `merged_into_hgnc_id` (character), `merged_into_symbol` (character), `merged_into_status` (character), `merged_into_report` (character) |

| field | value |
|:---|:---|
| metadata_version | 1 |
| source | hgnc |
| source_name | HUGO Gene Nomenclature Committee (HGNC) |
| release | NA |
| release_date | NA |
| retrieved_at | NA |
| source_url | <https://www.genenames.org/download/statistics-and-files/> |
| licence | CC0-1.0 |
| licence_url | <https://www.genenames.org/about/license/> |
| attribution | Seal RL, Braschi B, Gray K, McClay J, Tweedie S, Bruford EA. Genenames.org: the HGNC and PGNC resources in 2026. Nucleic Acids Research (Database issue). HGNC (RRID:SCR_002827). |
| parser | dbparser 2.3.0 |

Files read, with their SHA-256 hashes:

| name | sha256 |
|:---|:---|
| hgnc_complete_set.txt | c3289254858cdae91c28c5a6139ef75253359fbe3be8ccb10cc8a96679841612 |
| withdrawn.txt | 0d869c95057eb960ec7b7ce7844a896abfda755eadedef3051e51b767925b85b |

## Health Canada DPD: `parseHealthCanadaDPD()`

The 13 tables of Health Canada’s read-me. Every table except
`inactive_products` holds the rows of each product set requested, with
`extract` naming the set.

The parser stops unless `drug_code` is unique in `drug_product` and
every other table’s `drug_code` is a product. On the example files:

    #> drug_product_drug_code_unique  every_drug_code_is_a_product 
    #>                          TRUE                          TRUE

| table | rows | columns |
|:---|---:|:---|
| drug_product | 12 | `drug_code` (integer), `product_categorization` (character), `class` (character), `drug_identification_number` (character), `brand_name` (character), `descriptor` (character), `pediatric_flag` (character), `accession_number` (character), `number_of_ais` (character), `last_update_date` (Date), `ai_group_no` (character), `class_f` (character), `brand_name_f` (character), `descriptor_f` (character), `extract` (character) |
| active_ingredients | 24 | `drug_code` (integer), `active_ingredient_code` (integer), `ingredient` (character), `ingredient_supplied_ind` (character), `strength` (character), `strength_unit` (character), `strength_type` (character), `dosage_value` (character), `base` (character), `dosage_unit` (character), `notes` (character), `ingredient_f` (character), `strength_unit_f` (character), `strength_type_f` (character), `dosage_unit_f` (character), `extract` (character) |
| companies | 12 | `drug_code` (integer), `mfr_code` (character), `company_code` (integer), `company_name` (character), `company_type` (character), `address_mailing_flag` (character), `address_billing_flag` (character), `address_notification_flag` (character), `address_other` (character), `suite_number` (character), `street_name` (character), `city_name` (character), `province` (character), `country` (character), `postal_code` (character), `post_office_box` (character), `province_f` (character), `country_f` (character), `extract` (character) |
| form | 12 | `drug_code` (integer), `pharm_form_code` (integer), `pharmaceutical_form` (character), `pharmaceutical_form_f` (character), `extract` (character) |
| packaging | 10 | `drug_code` (integer), `upc` (character), `package_size_unit` (character), `package_type` (character), `package_size` (character), `product_information` (character), `package_size_unit_f` (character), `package_type_f` (character), `extract` (character) |
| pharmaceutical_std | 8 | `drug_code` (integer), `pharmaceutical_std` (character), `extract` (character) |
| route | 16 | `drug_code` (integer), `route_of_administration_code` (integer), `route_of_administration` (character), `route_of_administration_f` (character), `extract` (character) |
| schedule | 13 | `drug_code` (integer), `schedule` (character), `schedule_f` (character), `extract` (character) |
| status | 57 | `drug_code` (integer), `current_status_flag` (character), `status` (character), `history_date` (Date), `status_f` (character), `lot_number` (character), `expiration_date` (Date), `extract` (character) |
| therapeutic_class | 9 | `drug_code` (integer), `tc_atc_number` (character), `tc_atc` (character), `extract` (character) |
| veterinary_species | 14 | `drug_code` (integer), `vet_species` (character), `vet_sub_species` (character), `vet_species_f` (character), `extract` (character) |
| biosimilars | 2 | `drug_code` (integer), `si_desc_e` (character), `si_desc_f` (character), `si_code` (integer), `extract` (character) |
| inactive_products | 3 | `drug_code` (integer), `drug_identification_number` (character), `brand_name` (character), `history_date` (Date), `brand_name_f` (character) |

| field | value |
|:---|:---|
| metadata_version | 1 |
| source | health_canada_dpd |
| source_name | Health Canada Drug Product Database (DPD) |
| release | NA |
| release_date | NA |
| retrieved_at | NA |
| source_url | <https://www.canada.ca/en/health-canada/services/drugs-health-products/drug-products/drug-product-database/what-data-extract-drug-product-database.html> |
| licence | OGL-Canada-2.0 |
| licence_url | <https://open.canada.ca/en/open-government-licence-canada> |
| attribution | Contains information licensed under the Open Government Licence – Canada. |
| parser | dbparser 2.3.0 |

Files read, with their SHA-256 hashes:

| name | sha256 |
|:---|:---|
| biosimilar.txt | b005d94dc329a71b632048317238a6daa956556daecd304c1789d69721a590b8 |
| comp.txt | 78747e4f5b6b7dd452027891963374e688c924c8169f054f9816d26e75f7f19a |
| drug.txt | 187220d80e9184f15f4cc727f462f3a6f1f00743e3abac5b2e8eb41b42f32057 |
| form.txt | 71f0da504d1b8871e9b35c61551213ab47c6b8d3e3d3121ab3d3e9f8a50211ca |
| ingred.txt | e6daae79e4ade015a7e67e006af0c87523203b8dac41fb969a0ce23297d376e2 |
| package.txt | 6f8487c94ad31b91fea2e93b5d14340b186523aec6ca45b37646717245136967 |
| pharm.txt | 86b34a70a60c7a0facaaada28d39ad71cfbc93205ecfd7433cef3661fda75d02 |
| route.txt | ce6ed42800a22f5b877979826dcda69cd577062e9988bfe99558ed111bdc6414 |
| schedule.txt | 077afb9f8685a0c684c6793ccf8ad04cc106f80fd3bb8db66b39b944b49b1ead |
| status.txt | 32bbfd1dbb1c601e97fd3f1dc9074d87e56b275f5c68f033b1345be17f0b490e |
| ther.txt | a6d509404295db4617ad881f6019836c160d09ee048a876861be947b1e31dab4 |
| vet.txt | cefc4c381ed3044d49ecab4b2e30183712a268e737ddd95ec5fac652b601ad93 |
| biosimilar_ap.txt | 9b9f057d660070545f0ea8682213fb2a8f23e23e660f54380108b85b0723e14f |
| comp_ap.txt | 16810166a3be5f59804a2b5ae277f01f517f06f0cceecd465ae70f86d7144831 |
| drug_ap.txt | 48aebe3f5d0aaedf690ee4a7795b81163cd3500fe3774bf15caf7f8b18d310fe |
| form_ap.txt | fa2789b5a5fd9d1a2b5fde4914ccb9e2ad373586ae53fd5799bb3ce7bf902e03 |
| ingred_ap.txt | 4ffa4577a6cdc83a05e7e909f3ccd042fed09c08d3f75f7917f42270de26be0c |
| package_ap.txt | 47df19e5f974caa7c3706e8ef4b632ba1c7689e18ce0decab00b606082af7a1f |
| pharm_ap.txt | 33b081346a4f5437e4da401374b7c351e9d7d40c41c45dc8d56ce57353210d4f |
| route_ap.txt | 1f99d2a786ef99cc54454dceaf5b3d10b14751faca744c3b4e52dc7a3f1f214b |
| schedule_ap.txt | d478d6e53f0d654a91ebcfcb6f8217b30768417ba255fb6b4103d491a22f9afc |
| status_ap.txt | be70304dbafa7bc09aa1604a7939b210d5551ee19c0f904544eb0c4d1f2ec2a9 |
| ther_ap.txt | 2641f5f216fe715e039137781048a61317c1642323ef8645e2b76bfacfada9d7 |
| vet_ap.txt | e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 |
| allfiles_dr.zip | 53d95504fca84c79bdc56b2888a5158ddc241570f346dd295542ba7f61039118 |
| biosimilar_ia.txt | e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 |
| comp_ia.txt | 60c480899386777cfcf23ccd42ef60528100f20e6ff9cac3446d1375639fba47 |
| drug_ia.txt | 1d1aaee99c166573ed620f73108f0cddbc8ae0dc4ece20830f7bfb323dec8106 |
| form_ia.txt | bed0b5690f82d8a6c459db212a0fcc99dc81ac0e9a097ac565a88ff1a2367736 |
| ingred_ia.txt | cce671b22492e916d995a1d186229d5cd15c2b7cc228d30d7bd7d3a2cdbd2290 |
| package_ia.txt | c8ccd86d61bd0f3d0c983b5fc8c682d65aa0e43cd9390e078602f478c54306ec |
| pharm_ia.txt | bd5ad1d748d01b62ecaf93fe2cf2c992d959ce862e3e8ebda69639f1528d780b |
| route_ia.txt | 6a6acab4eb459a01a8d5636467dedcaeb1b5fdfdc4b484cd27813e235647ec16 |
| schedule_ia.txt | de14be6afeabe1b390854899ce1befb1ec40ccec42673f809db902d65f071133 |
| status_ia.txt | 6652c880fbe2f680c71b9e8d1692a22a25480c898620c4b727fb3969fb1c8b99 |
| ther_ia.txt | 143768a1751ca05022fdfa6696c9613e27fece5804d9e5c26b2619b53553d9bb |
| vet_ia.txt | e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855 |
| inactive.txt | 1ce2ac28fc1f5c650f5085cddbf74c7333a81e69cb439c896fd6de99a2e1ba73 |

## openFDA: `parseOpenFDA()`

One table per record level of the <Drugs@FDA> (`drugsfda_*`) and NDC
directory (`ndc_*`) downloads, field names as in the files.

The parser stops unless each table’s key is unique and every row names
its parent row. The keys and parents it checks, read from the parser:

| table | key | parent |
|:---|:---|:---|
| drugsfda_application | `application_number` |  |
| drugsfda_product | `application_number`, `product_number` | drugsfda_application (`application_number`) |
| drugsfda_active_ingredient |  | drugsfda_product (`application_number`, `product_number`) |
| drugsfda_submission | `application_number`, `submission_type`, `submission_number` | drugsfda_application (`application_number`) |
| drugsfda_submission_property_type |  | drugsfda_submission (`application_number`, `submission_type`, `submission_number`) |
| drugsfda_application_doc |  | drugsfda_submission (`application_number`, `submission_type`, `submission_number`) |
| drugsfda_openfda |  | drugsfda_application (`application_number`) |
| ndc_product | `product_id` |  |
| ndc_active_ingredient |  | ndc_product (`product_id`) |
| ndc_route |  | ndc_product (`product_id`) |
| ndc_pharm_class |  | ndc_product (`product_id`) |
| ndc_packaging |  | ndc_product (`product_id`) |
| ndc_openfda |  | ndc_product (`product_id`) |

| table | rows | columns |
|:---|---:|:---|
| drugsfda_application | 9 | `application_number` (character), `sponsor_name` (character) |
| drugsfda_product | 21 | `application_number` (character), `product_number` (character), `brand_name` (character), `dosage_form` (character), `route` (character), `marketing_status` (character), `reference_drug` (character), `reference_standard` (character), `te_code` (character) |
| drugsfda_active_ingredient | 23 | `application_number` (character), `product_number` (character), `name` (character), `strength` (character) |
| drugsfda_submission | 165 | `application_number` (character), `submission_type` (character), `submission_number` (character), `submission_status` (character), `submission_status_date` (Date), `review_priority` (character), `submission_class_code` (character), `submission_class_code_description` (character), `submission_public_notes` (character) |
| drugsfda_submission_property_type | 5 | `application_number` (character), `submission_type` (character), `submission_number` (character), `code` (character) |
| drugsfda_application_doc | 122 | `application_number` (character), `submission_type` (character), `submission_number` (character), `id` (character), `type` (character), `title` (character), `date` (Date), `url` (character) |
| drugsfda_openfda | 121 | `application_number` (character), `field` (character), `value` (character) |
| ndc_product | 9 | `product_id` (character), `product_ndc` (character), `product_type` (character), `brand_name` (character), `brand_name_base` (character), `brand_name_suffix` (character), `generic_name` (character), `labeler_name` (character), `dosage_form` (character), `marketing_category` (character), `application_number` (character), `marketing_start_date` (Date), `marketing_end_date` (Date), `listing_expiration_date` (Date), `finished` (logical), `dea_schedule` (character), `spl_id` (character) |
| ndc_active_ingredient | 10 | `product_id` (character), `name` (character), `strength` (character) |
| ndc_route | 8 | `product_id` (character), `route` (character) |
| ndc_pharm_class | 28 | `product_id` (character), `pharm_class` (character) |
| ndc_packaging | 38 | `product_id` (character), `package_ndc` (character), `description` (character), `marketing_start_date` (character), `marketing_end_date` (character), `sample` (logical) |
| ndc_openfda | 45 | `product_id` (character), `field` (character), `value` (character) |

| field | value |
|:---|:---|
| metadata_version | 1 |
| source | openfda |
| source_name | openFDA (U.S. Food and Drug Administration) |
| release | NA |
| release_date | 2026-10-20 |
| retrieved_at | NA |
| source_url | <https://open.fda.gov/data/downloads/> |
| licence | CC0-1.0 |
| licence_url | <https://open.fda.gov/license/> |
| attribution | Data provided by the U.S. Food and Drug Administration (<https://open.fda.gov>) |
| parser | dbparser 2.3.0 |

Files read, with their SHA-256 hashes:

| name | sha256 |
|:---|:---|
| drug-drugsfda-0001-of-0001.json.zip | e69207845b6cb55a87a065de2c9305e3893f7a30c0ddd1dc112d53b2db6c334a |
| drug-ndc-0001-of-0001.json.zip | 7d82a88f11af5b81ada7515c93fdeef03a57118a75688362983634162df70df2 |
