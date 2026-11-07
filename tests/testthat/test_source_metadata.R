context("test source metadata block")

block_fields <- c("metadata_version", "source", "source_name", "release",
                  "release_date", "retrieved_at", "source_url", "licence",
                  "licence_url", "attribution", "files", "parser")

has_sha256 <- getRversion() >= "4.5.0"

expect_block <- function(block, source, file_names) {
  expect_equal(names(block), block_fields)
  expect_equal(block$metadata_version, "1")
  expect_equal(block$source, source)
  expect_equal(block$parser,
               paste("dbparser", utils::packageVersion("dbparser")))
  expect_equal(block$files$name, file_names)
  if (has_sha256) {
    expect_true(all(grepl("^[0-9a-f]{64}$", block$files$sha256)))
  } else {
    expect_true(all(is.na(block$files$sha256)))
  }
}

test_that("source_metadata builds the block with NA for unknown fields", {
  block <- source_metadata(source = "test")

  expect_equal(names(block), block_fields)
  expect_equal(block$source_name, "test")
  expect_true(is.na(block$release))
  expect_s3_class(block$release_date, "Date")
  expect_true(is.na(block$release_date))
  expect_true(is.na(block$retrieved_at))
  expect_true(is.na(block$licence))
  expect_true(is.na(block$attribution))
  expect_equal(nrow(block$files), 0L)
  expect_equal(names(block$files), c("name", "sha256"))

  expect_error(source_metadata(source = NA), "single character string")
  expect_error(source_metadata(source = c("a", "b")),
               "single character string")
})

test_that("source_metadata keeps given values and parses ISO dates", {
  path  <- system.file("extdata", "drugbank_record_biotech.xml",
                       package = "dbparser")
  block <- source_metadata(source       = "test",
                           source_name  = "Test source",
                           files        = path,
                           release      = "1.0",
                           release_date = "2026-10-06",
                           retrieved_at = as.Date("2026-10-16"),
                           licence      = "CC0-1.0",
                           attribution  = "Cite us.")

  expect_equal(block$release, "1.0")
  expect_equal(block$release_date, as.Date("2026-10-06"))
  expect_equal(block$retrieved_at, as.Date("2026-10-16"))
  expect_equal(block$licence, "CC0-1.0")
  expect_equal(block$attribution, "Cite us.")
  expect_equal(block$files$name, "drugbank_record_biotech.xml")
  if (has_sha256) {
    expect_equal(block$files$sha256,
                 unname(get("sha256sum", envir = asNamespace("tools"))(path)))
  }

  expect_message(bad <- source_metadata(source       = "test",
                                        release_date = "01-07-2015"),
                 "not YYYY-MM-DD")
  expect_true(is.na(bad$release_date))
})

test_that("set_source_metadata stores one named attribute", {
  dvobj <- set_source_metadata(init_dvobject(), source_metadata("a"))
  dvobj <- set_source_metadata(dvobj, source_metadata("b"))

  expect_equal(names(attr(dvobj, "source_metadata")), c("a", "b"))
  expect_null(attr(dvobj, "source", exact = TRUE))
  expect_null(attr(dvobj, "files", exact = TRUE))

  # same source replaces its block
  dvobj <- set_source_metadata(dvobj, source_metadata("a", release = "2"))
  expect_equal(names(attr(dvobj, "source_metadata")), c("a", "b"))
  expect_equal(attr(dvobj, "source_metadata")$a$release, "2")

  # a named list of blocks (from another dvobject) is merged as is
  other <- set_source_metadata(init_dvobject(), source_metadata("c"))
  dvobj <- set_source_metadata(dvobj, attr(other, "source_metadata"))
  expect_equal(names(attr(dvobj, "source_metadata")), c("a", "b", "c"))

  expect_identical(set_source_metadata(dvobj, NULL), dvobj)
})

test_that("existing parsers attach the block", {
  drugbank <- suppressMessages(parseDrugBank(
    system.file("extdata", "drugbank_record_biotech.xml",
                package = "dbparser")))
  block    <- attr(drugbank, "source_metadata")$drugbank
  expect_block(block, "drugbank", "drugbank_record_biotech.xml")
  expect_equal(block$release, "5.1")
  expect_equal(block$release_date, as.Date("2018-07-03"))
  expect_true(is.na(block$licence))
  # the earlier metadata is kept
  expect_equal(attr(drugbank, "original_db_info")$db_type, "DrugBank")

  hpo   <- suppressMessages(parseHPO(
    system.file("extdata", "hpo_gene_attribute_edges.txt",
                package = "dbparser"),
    db_version = "2015", db_exported_date = "2015-07-01"))
  block <- attr(hpo, "source_metadata")$hpo
  expect_block(block, "hpo", "hpo_gene_attribute_edges.txt")
  expect_equal(block$release, "2015")
  expect_equal(block$release_date, as.Date("2015-07-01"))

  unzip_dir <- tempfile("twoside_")
  on.exit(unlink(unzip_dir, recursive = TRUE), add = TRUE)
  data_path <- unzip(system.file("twoside_raw.zip", package = "dbparser"),
                     exdir = unzip_dir)
  twoside   <- suppressMessages(parseTWOSIDES(data_path))
  expect_block(attr(twoside, "source_metadata")$twosides, "twosides",
               "twoside_raw.csv")

  onside_dir <- dirname(unzip(system.file("onside.zip", package = "dbparser"),
                              exdir = unzip_dir)[1])
  onside     <- suppressMessages(parseOnSIDES(onside_dir))
  expect_block(attr(onside, "source_metadata")$onsides, "onsides",
               c("product_label.csv", "product_adverse_effect.csv",
                 "product_to_rxnorm.csv", "vocab_meddra_adverse_effect.csv",
                 "vocab_rxnorm_ingredient.csv", "vocab_rxnorm_product.csv",
                 "vocab_rxnorm_ingredient_to_product.csv",
                 "high_confidence.csv"))
})

test_that("merged dvobjects keep one block per source", {
  drugbank <- suppressMessages(parseDrugBank(
    system.file("extdata", "drugbank_record_small_molecule.xml",
                package = "dbparser"),
    drug_options = drug_node_options()))
  unzip_dir <- tempfile("twoside_")
  on.exit(unlink(unzip_dir, recursive = TRUE), add = TRUE)
  twoside   <- suppressMessages(parseTWOSIDES(
    unzip(system.file("twoside_raw.zip", package = "dbparser"),
          exdir = unzip_dir)))

  merged <- suppressMessages(merge_drugbank_twosides(db_object   = drugbank,
                                                     twosides_db = twoside))
  expect_equal(names(attr(merged, "source_metadata")),
               c("drugbank", "twosides"))

  # the block is not mistaken for a second database
  capture_output(metadata <- show_dvobject_metadata(merged))
  expect_equal(metadata$Type, c("DrugBank", "TWOSIDESDB"))

  subset <- suppressMessages(subset_drugbank_dvobject(
    drugbank, drug_ids = drugbank$drugs$general_information$drugbank_id[1]))
  expect_equal(attr(subset, "source_metadata"),
               attr(drugbank, "source_metadata"))
})
