context("test parse HPO gene-phenotype associations")
local_edition(3)

hpo_path <- system.file("extdata", "hpo_gene_attribute_edges.txt",
                        package = "dbparser")

test_that("parseHPO validates its inputs", {
  expect_error(parseHPO(c("a", "b")),
               "`filepath` must be a single character string.")
  expect_error(parseHPO("not_existing_file.txt"),
               "HPO file not found at path: not_existing_file.txt")
  expect_error(parseHPO(hpo_path, minimum_weight = -1),
               "`minimum_weight` must be a single non-negative number.")
  expect_error(parseHPO(hpo_path, minimum_weight = "1"),
               "`minimum_weight` must be a single non-negative number.")

  bad_file <- tempfile(fileext = ".txt")
  on.exit(unlink(bad_file))
  writeLines(c("gene\tphenotype", "F2\tvenous thrombosis"), bad_file)
  expect_error(suppressMessages(parseHPO(bad_file)),
               "Missing expected columns")
})

test_that("parseHPO parses the Harmonizome edges file", {
  hpo_db <- purrr::quietly(parseHPO)(hpo_path,
                                     db_version       = "2015",
                                     db_exported_date = "2015-07-01")
  expect_match(hpo_db$messages[1], "Parsing HPO gene-phenotype associations")
  hpo_db <- hpo_db$result

  expect_s3_class(hpo_db, "dvobject")
  expect_equal(names(hpo_db), c("associations", "genes", "phenotypes"))

  # descriptor row (GeneSym/GeneID/...) is dropped
  expect_false("GeneSym" %in% hpo_db$associations$gene_symbol)
  expect_equal(dim(hpo_db$associations), c(485L, 5L))
  expect_equal(names(hpo_db$associations),
               c("gene_symbol", "entrez_id", "hpo_id", "phenotype_name",
                 "association_weight"))
  expect_type(hpo_db$associations$association_weight, "double")
  expect_type(hpo_db$associations$entrez_id, "character")

  expect_equal(hpo_db$genes$gene_symbol,
               c("F2", "GNRH1", "GNRHR", "KCNQ2", "MPO", "SCN1A", "VDR"))
  expect_equal(hpo_db$genes$n_phenotypes,
               c(71L, 24L, 30L, 69L, 7L, 101L, 183L))
  expect_equal(NROW(hpo_db$phenotypes), 352L)
  seizures <- hpo_db$phenotypes[hpo_db$phenotypes$hpo_id == "HP:0001250", ]
  expect_equal(seizures$n_genes, 3L)

  expect_snapshot(show_dvobject_metadata(hpo_db))
})

test_that("parseHPO reads gzip-compressed files", {
  gz_file <- tempfile(fileext = ".txt.gz")
  on.exit(unlink(gz_file))
  con <- gzfile(gz_file, "wt")
  writeLines(readLines(hpo_path), con)
  close(con)

  expect_equal(suppressMessages(parseHPO(gz_file))$associations,
               suppressMessages(parseHPO(hpo_path))$associations)
})

test_that("parseHPO applies minimum_weight", {
  weighted_file <- tempfile(fileext = ".txt")
  on.exit(unlink(weighted_file))
  lines    <- readLines(hpo_path)
  # give the KCNQ2 associations a low weight
  kcnq2    <- grepl("^KCNQ2\t", lines)
  lines[kcnq2] <- sub("1.000000$", "0.200000", lines[kcnq2])
  writeLines(lines, weighted_file)

  filtered <- suppressMessages(parseHPO(weighted_file, minimum_weight = 0.5))
  expect_false("KCNQ2" %in% filtered$associations$gene_symbol)
  expect_equal(NROW(filtered$associations), 416L)
  expect_equal(NROW(filtered$genes), 6L)

  bad_weights <- lines
  bad_weights[3] <- sub("1.000000$", "high", bad_weights[3])
  writeLines(bad_weights, weighted_file)
  expect_error(suppressMessages(parseHPO(weighted_file)),
               "Column `weight` contains non-numeric values.")
})

test_that("search_phenotypes finds phenotypes by keyword", {
  hpo_db <- suppressMessages(parseHPO(hpo_path))

  expect_error(search_phenotypes(list(), "seizure"),
               "`hpo_db` must be a dvobject returned by parseHPO().")
  expect_error(search_phenotypes(hpo_db, ""),
               "`keyword` must be a single non-empty character string.")

  seizures <- search_phenotypes(hpo_db, "SEIZURE")
  expect_equal(names(seizures),
               c("hpo_id", "phenotype_name", "n_genes", "genes"))
  expect_equal(NROW(seizures), 16L)
  expect_equal(seizures$hpo_id[1], "HP:0001250")
  expect_equal(seizures$genes[1], "KCNQ2, SCN1A, VDR")
  expect_equal(NROW(search_phenotypes(hpo_db, "seizure", max_results = 2)),
               2L)
  # keyword is a plain string, not a regular expression
  expect_equal(NROW(search_phenotypes(hpo_db, "metabolism/homeostasis")), 1L)
  expect_equal(NROW(search_phenotypes(hpo_db, "seiz.*")), 0L)
})

test_that("get_genes_for_phenotype finds genes by id or name", {
  hpo_db <- suppressMessages(parseHPO(hpo_path))

  expect_error(get_genes_for_phenotype(hpo_db, NA_character_),
               "`phenotype` must be a single non-empty character string.")

  by_id <- get_genes_for_phenotype(hpo_db, "HP:0001250")
  expect_equal(by_id$gene_symbol, c("KCNQ2", "SCN1A", "VDR"))
  expect_equal(names(by_id),
               c("gene_symbol", "entrez_id", "hpo_id", "phenotype_name",
                 "association_weight"))

  expect_equal(get_genes_for_phenotype(hpo_db, "Venous Thrombosis",
                                       exact = TRUE)$gene_symbol, "F2")
  expect_equal(NROW(get_genes_for_phenotype(hpo_db, "seizures",
                                            exact = TRUE)), 3L)
  partial <- get_genes_for_phenotype(hpo_db, "seizures")
  expect_equal(NROW(partial), 24L)
  expect_equal(unique(partial$gene_symbol), c("KCNQ2", "SCN1A", "VDR"))
  expect_equal(NROW(get_genes_for_phenotype(hpo_db, "no such phenotype")), 0L)
})

test_that("parseHPO output merges with DrugBank", {
  hpo_db   <- suppressMessages(parseHPO(hpo_path))
  drugbank <- suppressMessages(parseDrugBank(
    db_path            = system.file("extdata",
                                     "drugbank_record_small_molecule.xml",
                                     package = "dbparser"),
    drug_options       = NULL,
    parse_salts        = FALSE,
    parse_products     = FALSE,
    references_options = NULL,
    cett_options       = "targets"))

  merged   <- suppressMessages(merge_drugbank_hpo(drugbank, hpo_db))
  enriched <- merged$integrated_data$gene_phenotype_enriched
  linked   <- enriched[!is.na(enriched$drugbank_id), ]

  expect_equal(NROW(enriched), NROW(hpo_db$associations))
  expect_equal(unique(linked$gene_symbol), "F2")
  expect_equal(unique(linked$drugbank_id), "DB00006")
  expect_equal(unique(linked$drug_name), "Bivalirudin")
  expect_equal(NROW(linked), 71L)
})
