context("test HGNC parser")

hgnc_dir       <- system.file("extdata", "hgnc", package = "dbparser")
complete_path  <- file.path(hgnc_dir, "hgnc_complete_set.txt")
withdrawn_path <- file.path(hgnc_dir, "withdrawn.txt")

parse_fixture <- function(...) {
  suppressMessages(parseHGNC(complete_path, withdrawn_path = withdrawn_path,
                             ...))
}

genes_named <- function(hgnc, name) {
  ids <- unique(hgnc$gene_name$hgnc_id[hgnc$gene_name$name == name])
  sort(hgnc$gene$symbol[hgnc$gene$hgnc_id %in% ids])
}

test_that("parseHGNC validates its inputs", {
  expect_error(parseHGNC(c("a", "b")), "single character string")
  expect_error(parseHGNC("not_existing_file.txt"), "HGNC file not found")
  expect_error(parseHGNC(complete_path, withdrawn_path = "missing.txt"),
               "HGNC file not found")

  bad_file <- tempfile(fileext = ".txt")
  on.exit(unlink(bad_file), add = TRUE)
  writeLines(c("a\tb", "1\t2"), bad_file)
  expect_error(suppressMessages(parseHGNC(bad_file)),
               "Missing expected columns")
  expect_error(suppressMessages(parseHGNC(complete_path,
                                          withdrawn_path = bad_file)),
               "Unexpected columns")
})

test_that("parseHGNC returns the expected tables and columns", {
  hgnc <- parse_fixture()

  expect_s3_class(hgnc, "dvobject")
  expect_equal(names(hgnc), c("gene", "gene_name", "withdrawn"))
  for (table in names(hgnc)) {
    expect_s3_class(hgnc[[table]], "tbl_df")
  }

  # gene: the file's 53 columns, cleaned names, dates typed, rest text
  expect_equal(dim(hgnc$gene), c(24L, 53L))
  expect_equal(names(hgnc$gene)[1:3], c("hgnc_id", "symbol", "name"))
  expect_true(all(c("pseudogene_org", "mamit_trnadb") %in% names(hgnc$gene)))
  expect_false(any(grepl("[.-]", names(hgnc$gene))))
  date_columns <- grep("^date_", names(hgnc$gene), value = TRUE)
  expect_equal(date_columns, c("date_approved_reserved", "date_symbol_changed",
                               "date_name_changed", "date_modified"))
  for (column in names(hgnc$gene)) {
    expect_true(inherits(hgnc$gene[[column]],
                         if (column %in% date_columns) "Date" else "character"),
                info = column)
  }
  expect_false(anyDuplicated(hgnc$gene$hgnc_id) > 0)

  # multi-valued cells: quote wrapper stripped, pipes kept
  expect_false(any(grepl('"', unlist(hgnc$gene), fixed = TRUE)))
  expect_match(hgnc$gene$alias_symbol[hgnc$gene$symbol == "CD274"], "\\|")

  # gene_name
  expect_equal(names(hgnc$gene_name), c("hgnc_id", "kind", "name"))
  expect_equal(sort(unique(hgnc$gene_name$kind)),
               c("alias_name", "alias_symbol", "name", "prev_name",
                 "prev_symbol", "symbol"))
  expect_equal(sum(hgnc$gene_name$kind == "symbol"), 24L)
  expect_true(all(hgnc$gene_name$hgnc_id %in% hgnc$gene$hgnc_id))
  expect_false(anyDuplicated(hgnc$gene_name) > 0)
  expect_true(all(hgnc$gene_name$name == trimws(hgnc$gene_name$name)))
  expect_true(all(nzchar(hgnc$gene_name$name)))

  # withdrawn
  expect_equal(names(hgnc$withdrawn),
               c("hgnc_id", "status", "withdrawn_symbol",
                 "merged_into_hgnc_id", "merged_into_symbol",
                 "merged_into_status", "merged_into_report"))
  expect_equal(length(unique(hgnc$withdrawn$hgnc_id)), 7L)
  expect_false(any(hgnc$withdrawn$hgnc_id %in% hgnc$gene$hgnc_id))
})

test_that("parseHGNC resolves names as HGNC does", {
  hgnc <- parse_fixture()

  expect_equal(genes_named(hgnc, "PD1"), c("PDCD1", "SNCA", "SPATA2"))
  expect_equal(length(genes_named(hgnc, "CAP")), 6L)
  expect_equal(genes_named(hgnc, "PDL1"), "CD274")
  expect_equal(genes_named(hgnc, "B7-H1"), "CD274")
  expect_equal(genes_named(hgnc, "SEPT1"), "SEPTIN1")
  expect_equal(genes_named(hgnc, "MARCH1"), "MARCHF1")
  expect_equal(genes_named(hgnc, "HER2"), "ERBB2")
  expect_equal(genes_named(hgnc, "CD152"), "CTLA4")
  expect_equal(genes_named(hgnc, "FXI"), "F11")
  expect_equal(genes_named(hgnc, "p53"), "TP53")
  expect_equal(genes_named(hgnc, "FOX1"), character(0))

  # UTF-8 names are read as written
  expect_equal(genes_named(hgnc, "β-actin"), "ACTB")
  # case variants are kept
  ruvbl1 <- hgnc$gene_name$name[hgnc$gene_name$hgnc_id == "HGNC:10474"]
  expect_true(all(c("RVB1", "Rvb1") %in% ruvbl1))
})

test_that("parseHGNC splits withdrawn targets", {
  withdrawn <- parse_fixture()$withdrawn

  # no target: one row with NA targets
  a12m1 <- withdrawn[withdrawn$hgnc_id == "HGNC:1", ]
  expect_equal(NROW(a12m1), 1L)
  expect_true(is.na(a12m1$merged_into_hgnc_id))

  # two targets: two rows, report kept
  cbbm <- withdrawn[withdrawn$hgnc_id == "HGNC:1531", ]
  expect_equal(cbbm$merged_into_symbol, c("OPN1MW", "OPN1LW"))
  expect_equal(unique(cbbm$merged_into_status), "Approved")
  expect_match(unique(cbbm$merged_into_report), "HGNC:4206|OPN1MW|Approved",
               fixed = TRUE)

  # a target can itself be withdrawn (chains need more than one hop)
  adcr <- withdrawn[withdrawn$hgnc_id == "HGNC:231", ]
  expect_equal(adcr$merged_into_hgnc_id, "HGNC:12797")
  expect_true("HGNC:12797" %in% withdrawn$hgnc_id)

  # seven targets, as written in the file
  expect_equal(sum(withdrawn$hgnc_id == "HGNC:21128"), 7L)
})

test_that("parseHGNC works without the withdrawn file", {
  hgnc <- suppressMessages(parseHGNC(complete_path))
  expect_equal(names(hgnc), c("gene", "gene_name"))
  expect_equal(get_source_metadata(hgnc)$files[[1]]$name,
               "hgnc_complete_set.txt")
})

test_that("parseHGNC attaches the metadata block", {
  hgnc     <- parse_fixture(release = "2026-10", retrieved_at = "2026-10-16")
  metadata <- get_source_metadata(hgnc)

  expect_equal(metadata$source, "hgnc")
  expect_equal(metadata$release, "2026-10")
  expect_true(is.na(metadata$release_date))
  expect_equal(metadata$retrieved_at, as.Date("2026-10-16"))
  expect_equal(metadata$licence, "CC0-1.0")
  expect_equal(metadata$licence_url, "https://www.genenames.org/about/license/")
  expect_match(metadata$attribution, "RRID:SCR_002827", fixed = TRUE)
  expect_equal(metadata$files[[1]]$name,
               c("hgnc_complete_set.txt", "withdrawn.txt"))
  if (getRversion() >= "4.5.0") {
    expect_true(all(grepl("^[0-9a-f]{64}$", metadata$files[[1]]$sha256)))
  }
  expect_equal(attr(hgnc, "original_db_info")$db_type, "HGNC")
})
