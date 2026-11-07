context("test openFDA parser")

openfda_dir   <- system.file("extdata", "openfda", package = "dbparser")
drugsfda_path <- file.path(openfda_dir, "drug-drugsfda-0001-of-0001.json.zip")
ndc_path      <- file.path(openfda_dir, "drug-ndc-0001-of-0001.json.zip")

drugsfda_columns <- list(
  drugsfda_application = c("application_number", "sponsor_name"),
  drugsfda_product = c("application_number", "product_number", "brand_name",
                       "dosage_form", "route", "marketing_status",
                       "reference_drug", "reference_standard", "te_code"),
  drugsfda_active_ingredient = c("application_number", "product_number",
                                 "name", "strength"),
  drugsfda_submission = c("application_number", "submission_type",
                          "submission_number", "submission_status",
                          "submission_status_date", "review_priority",
                          "submission_class_code",
                          "submission_class_code_description",
                          "submission_public_notes"),
  drugsfda_submission_property_type = c("application_number",
                                        "submission_type",
                                        "submission_number", "code"),
  drugsfda_application_doc = c("application_number", "submission_type",
                               "submission_number", "id", "type", "title",
                               "date", "url"),
  drugsfda_openfda = c("application_number", "field", "value"))

ndc_columns <- list(
  ndc_product = c("product_id", "product_ndc", "product_type", "brand_name",
                  "brand_name_base", "brand_name_suffix", "generic_name",
                  "labeler_name", "dosage_form", "marketing_category",
                  "application_number", "marketing_start_date",
                  "marketing_end_date", "listing_expiration_date",
                  "finished", "dea_schedule", "spl_id"),
  ndc_active_ingredient = c("product_id", "name", "strength"),
  ndc_route = c("product_id", "route"),
  ndc_pharm_class = c("product_id", "pharm_class"),
  ndc_packaging = c("product_id", "package_ndc", "description",
                    "marketing_start_date", "marketing_end_date", "sample"),
  ndc_openfda = c("product_id", "field", "value"))

date_columns    <- c("submission_status_date", "date", "listing_expiration_date")
logical_columns <- c("finished", "sample")

parse_fixture <- function(...) {
  suppressMessages(parseOpenFDA(drugsfda_path = drugsfda_path,
                                ndc_path      = ndc_path, ...))
}

test_that("parseOpenFDA validates its inputs", {
  expect_error(parseOpenFDA(), "Give `drugsfda_path`, `ndc_path` or both")
  expect_error(parseOpenFDA(c("a", "b")), "single character string")
  expect_error(parseOpenFDA("missing.json.zip"), "openFDA file not found")

  bad_file <- tempfile(fileext = ".json")
  on.exit(unlink(bad_file), add = TRUE)
  writeLines('{"results": []}', bad_file)
  expect_error(suppressMessages(parseOpenFDA(bad_file)),
               "Not an openFDA download")
  writeLines('{"meta": {"results": {"total": 2}}, "results": [{}]}', bad_file)
  expect_error(suppressMessages(parseOpenFDA(bad_file)),
               "1 records parsed, meta total 2")
})

test_that("parseOpenFDA returns the expected tables and columns", {
  openfda <- parse_fixture()
  columns <- c(drugsfda_columns, ndc_columns)

  expect_s3_class(openfda, "dvobject")
  expect_equal(names(openfda), names(columns))
  for (table in names(columns)) {
    expect_s3_class(openfda[[table]], "tbl_df")
    expect_equal(names(openfda[[table]]), columns[[table]], info = table)
  }
  for (column in names(openfda$ndc_product)) {
    expected <- if (column %in% c("marketing_start_date", "marketing_end_date",
                                  date_columns)) {
      "Date"
    } else if (column %in% logical_columns) {
      "logical"
    } else {
      "character"
    }
    expect_true(inherits(openfda$ndc_product[[column]], expected),
                info = column)
  }
  expect_s3_class(openfda$drugsfda_submission$submission_status_date, "Date")
  expect_s3_class(openfda$drugsfda_application_doc$date, "Date")
  expect_type(openfda$ndc_packaging$sample, "logical")

  expect_equal(NROW(openfda$drugsfda_application), 9L)
  expect_equal(NROW(openfda$ndc_product), 9L)
})

test_that("parseOpenFDA keeps every record as the files have it", {
  openfda <- parse_fixture()

  # keys are unique
  expect_false(anyDuplicated(openfda$drugsfda_application$application_number) > 0)
  expect_false(anyDuplicated(openfda$ndc_product$product_id) > 0)
  # product_ndc is not a key: two listings share one
  expect_equal(sum(openfda$ndc_product$product_ndc == "71288-166"), 2L)

  # a tentative approval is kept, statuses are not grouped
  expect_true("TA" %in% openfda$drugsfda_submission$submission_status)
  # applications without products, submissions or openfda block are kept
  apps <- openfda$drugsfda_application$application_number
  expect_true(all(c("NDA200861", "NDA007245", "ANDA076219") %in% apps))
  expect_false("NDA200861" %in% openfda$drugsfda_product$application_number)
  expect_false("NDA007245" %in% openfda$drugsfda_submission$application_number)
  expect_false("ANDA076219" %in% openfda$drugsfda_openfda$application_number)
  # an empty submission date is NA
  expect_true(any(is.na(openfda$drugsfda_submission$submission_status_date[
    openfda$drugsfda_submission$application_number == "NDA021014"])))

  # openFDA's annotation is kept as written, even when it names another drug
  andas <- openfda$drugsfda_openfda[
    openfda$drugsfda_openfda$application_number == "ANDA202111" &
      openfda$drugsfda_openfda$field == "generic_name", ]
  expect_equal(andas$value, "MEXILETINE HYDROCHLORIDE")
  expect_match(openfda$drugsfda_active_ingredient$name[
    openfda$drugsfda_active_ingredient$application_number == "ANDA202111"][1],
    "PEMETREXED")

  # NDC: unfinished bulk listings kept, packaging dates as written
  expect_true(any(!openfda$ndc_product$finished))
  expect_true(any(grepl("^[0-9]{2}-[A-Z]{3}-[0-9]{2}$",
                        openfda$ndc_packaging$marketing_start_date)))
  expect_true(any(openfda$ndc_packaging$sample, na.rm = TRUE))

  # every child row names its parent
  expect_true(all(openfda$ndc_packaging$product_id %in%
                    openfda$ndc_product$product_id))
  expect_true(all(openfda$drugsfda_product$application_number %in% apps))
})

test_that("parseOpenFDA reads one file, or unzipped JSON", {
  drugsfda <- suppressMessages(parseOpenFDA(drugsfda_path = drugsfda_path))
  expect_equal(names(drugsfda), names(drugsfda_columns))

  json_dir <- tempfile("openfda_json_")
  on.exit(unlink(json_dir, recursive = TRUE), add = TRUE)
  json_path <- utils::unzip(ndc_path, exdir = json_dir)
  ndc_json  <- suppressMessages(parseOpenFDA(ndc_path = json_path))
  ndc_zip   <- suppressMessages(parseOpenFDA(ndc_path = ndc_path))
  expect_equal(names(ndc_json), names(ndc_columns))
  for (table in names(ndc_columns)) {
    expect_equal(ndc_json[[table]], ndc_zip[[table]], info = table)
  }
})

test_that("parseOpenFDA does not extract the zips", {
  before <- list.files(openfda_dir, recursive = TRUE, all.files = TRUE)
  parse_fixture()
  expect_equal(list.files(openfda_dir, recursive = TRUE, all.files = TRUE),
               before)
})

test_that("parseOpenFDA attaches the metadata block", {
  openfda  <- parse_fixture(release = "2026-10", retrieved_at = "2026-10-21")
  metadata <- get_source_metadata(openfda)

  expect_equal(metadata$source, "openfda")
  expect_equal(metadata$release, "2026-10")
  # the files' own export date
  expect_equal(metadata$release_date, as.Date("2026-10-20"))
  expect_equal(metadata$retrieved_at, as.Date("2026-10-21"))
  expect_equal(metadata$licence, "CC0-1.0")
  expect_equal(metadata$licence_url, "https://open.fda.gov/license/")
  expect_match(metadata$attribution, "U.S. Food and Drug Administration",
               fixed = TRUE)
  expect_equal(metadata$files[[1]]$name,
               c("drug-drugsfda-0001-of-0001.json.zip",
                 "drug-ndc-0001-of-0001.json.zip"))
  if (getRversion() >= "4.5.0") {
    expect_true(all(grepl("^[0-9a-f]{64}$", metadata$files[[1]]$sha256)))
  }

  # a caller's release date wins
  given <- parse_fixture(release_date = "2026-10-15")
  expect_equal(get_source_metadata(given)$release_date, as.Date("2026-10-15"))
})
