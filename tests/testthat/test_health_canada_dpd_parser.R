context("test Health Canada DPD parser")

dpd_dir <- system.file("extdata", "health_canada_dpd", package = "dbparser")

dpd_tables <- c("drug_product", "active_ingredients", "companies", "form",
                "packaging", "pharmaceutical_std", "route", "schedule",
                "status", "therapeutic_class", "veterinary_species",
                "biosimilars", "inactive_products")

dpd_columns <- list(
  drug_product = c("drug_code", "product_categorization", "class",
                   "drug_identification_number", "brand_name", "descriptor",
                   "pediatric_flag", "accession_number", "number_of_ais",
                   "last_update_date", "ai_group_no", "class_f",
                   "brand_name_f", "descriptor_f", "extract"),
  active_ingredients = c("drug_code", "active_ingredient_code", "ingredient",
                         "ingredient_supplied_ind", "strength",
                         "strength_unit", "strength_type", "dosage_value",
                         "base", "dosage_unit", "notes", "ingredient_f",
                         "strength_unit_f", "strength_type_f",
                         "dosage_unit_f", "extract"),
  companies = c("drug_code", "mfr_code", "company_code", "company_name",
                "company_type", "address_mailing_flag",
                "address_billing_flag", "address_notification_flag",
                "address_other", "suite_number", "street_name", "city_name",
                "province", "country", "postal_code", "post_office_box",
                "province_f", "country_f", "extract"),
  form = c("drug_code", "pharm_form_code", "pharmaceutical_form",
           "pharmaceutical_form_f", "extract"),
  packaging = c("drug_code", "upc", "package_size_unit", "package_type",
                "package_size", "product_information", "package_size_unit_f",
                "package_type_f", "extract"),
  pharmaceutical_std = c("drug_code", "pharmaceutical_std", "extract"),
  route = c("drug_code", "route_of_administration_code",
            "route_of_administration", "route_of_administration_f",
            "extract"),
  schedule = c("drug_code", "schedule", "schedule_f", "extract"),
  status = c("drug_code", "current_status_flag", "status", "history_date",
             "status_f", "lot_number", "expiration_date", "extract"),
  therapeutic_class = c("drug_code", "tc_atc_number", "tc_atc", "extract"),
  veterinary_species = c("drug_code", "vet_species", "vet_sub_species",
                         "vet_species_f", "extract"),
  biosimilars = c("drug_code", "si_desc_e", "si_desc_f", "si_code",
                  "extract"),
  inactive_products = c("drug_code", "drug_identification_number",
                        "brand_name", "history_date", "brand_name_f"))

integer_columns <- c("drug_code", "active_ingredient_code", "company_code",
                     "pharm_form_code", "route_of_administration_code",
                     "si_code")
date_columns    <- c("last_update_date", "history_date", "expiration_date")

parse_fixture <- function(...) {
  suppressMessages(parseHealthCanadaDPD(dpd_dir, ...))
}

test_that("parseHealthCanadaDPD validates its inputs", {
  expect_error(parseHealthCanadaDPD(c("a", "b")), "single character string")
  expect_error(parseHealthCanadaDPD("no_such_folder"), "DPD folder not found")
  expect_error(parseHealthCanadaDPD(dpd_dir, extracts = "recalled"),
               "`extracts` must be one or more of")
  expect_error(parseHealthCanadaDPD(tempdir(), extracts = "marketed"),
               "DPD marketed set not found")
})

test_that("parseHealthCanadaDPD returns the 13 tables with the read-me's columns", {
  dpd <- parse_fixture()

  expect_s3_class(dpd, "dvobject")
  expect_equal(names(dpd), dpd_tables)
  for (table in dpd_tables) {
    expect_s3_class(dpd[[table]], "tbl_df")
    expect_equal(names(dpd[[table]]), dpd_columns[[table]], info = table)
    for (column in names(dpd[[table]])) {
      expected <- if (column %in% integer_columns) {
        "integer"
      } else if (column %in% date_columns) {
        "Date"
      } else {
        "character"
      }
      expect_true(inherits(dpd[[table]][[column]], expected),
                  info = paste(table, column))
    }
  }

  # rows of every set, read from folders and from allfiles_dr.zip
  expect_equal(as.vector(table(dpd$drug_product$extract)[
    c("marketed", "approved", "cancelled", "dormant")]), c(5L, 2L, 3L, 2L))
  expect_equal(vapply(dpd, NROW, integer(1), USE.NAMES = FALSE),
               c(12L, 24L, 12L, 12L, 10L, 8L, 16L, 13L, 57L, 9L, 14L, 2L, 3L))
})

test_that("parseHealthCanadaDPD keeps the keys consistent", {
  dpd <- parse_fixture()

  expect_false(anyDuplicated(dpd$drug_product$drug_code) > 0)
  for (table in setdiff(dpd_tables, "drug_product")) {
    expect_true(all(dpd[[table]]$drug_code %in% dpd$drug_product$drug_code),
                info = table)
  }
  # each product has exactly one current status
  current <- dpd$status[dpd$status$current_status_flag == "Y", ]
  expect_equal(sort(current$drug_code), sort(dpd$drug_product$drug_code))
  # the parser groups nothing: statuses are as written
  expect_true(all(c("MARKETED", "APPROVED", "DORMANT") %in% current$status))
})

test_that("parseHealthCanadaDPD reads values as written", {
  dpd <- parse_fixture()

  # empty quoted values become NA, not ""
  for (table in dpd_tables) {
    text <- unlist(dpd[[table]][vapply(dpd[[table]], is.character, TRUE)])
    expect_false(any(text == "", na.rm = TRUE), info = table)
  }

  # DD-MON-YYYY dates, including the years 0030 and 0031 kept as written
  expiry <- dpd$status[!is.na(dpd$status$expiration_date), ]
  # (compared as numbers: Linux prints year 30 as "30", not "0030")
  expiry_year <- function(code) {
    as.POSIXlt(expiry$expiration_date[expiry$drug_code == code])$year + 1900L
  }
  expect_equal(expiry_year(80771), 30)
  expect_equal(expiry_year(94947), 31)
  expect_equal(dpd$drug_product$last_update_date[
    dpd$drug_product$drug_code == 15], as.Date("2025-03-22"))

  # UTF-8 French text is read as written
  expect_true(any(grepl("É", dpd$status$status_f)))

  # an empty file gives zero rows (biosimilar_ia.txt in the fixture)
  expect_false(any(dpd$biosimilars$extract == "cancelled"))
})

test_that("parseHealthCanadaDPD stops on unexpected file layouts", {
  bad_dir <- tempfile("dpd_bad_")
  on.exit(unlink(bad_dir, recursive = TRUE), add = TRUE)
  dir.create(file.path(bad_dir, "allfiles"), recursive = TRUE)
  file.copy(list.files(file.path(dpd_dir, "allfiles"), full.names = TRUE),
            file.path(bad_dir, "allfiles"))
  ther <- file.path(bad_dir, "allfiles", "ther.txt")

  writeLines('"15","N06AA10","NORTRIPTYLINE","X"', ther)
  expect_error(suppressMessages(parseHealthCanadaDPD(bad_dir,
                                                     extracts = "marketed")),
               "does not name holds values")

  writeLines('"15","N06AA10","NORTRIPTYLINE"', ther)
  expect_error(suppressMessages(parseHealthCanadaDPD(bad_dir,
                                                     extracts = "marketed")),
               "3 fields, the read-me gives 4")

  writeLines('"15","N06AA10","NORTRIPTYLINE",""', ther)
  status <- file.path(bad_dir, "allfiles", "status.txt")
  writeLines('"15","Y","MARKETED","2025-03-22","COMMERCIALISÉ","",""',
             status, useBytes = TRUE)
  expect_error(suppressMessages(parseHealthCanadaDPD(bad_dir,
                                                     extracts = "marketed")),
               "not DD-MON-YYYY")
})

test_that("parseHealthCanadaDPD reads only the requested sets", {
  dpd <- parse_fixture(extracts = c("marketed", "dormant"))

  expect_equal(sort(unique(dpd$drug_product$extract)),
               c("dormant", "marketed"))
  # inactive products are cancelled products: left out without that set
  expect_false("inactive_products" %in% names(dpd))
  files <- get_source_metadata(dpd)$files[[1]]$name
  # the 12 marketed files and the dormant zip
  expect_equal(length(files), 13L)
  expect_true(all(c("allfiles_dr.zip", "drug.txt") %in% files))
})

test_that("parseHealthCanadaDPD leaves no extracted files behind", {
  before <- list.files(tempdir(), pattern = "^dbparser_dpd_")
  parse_fixture(extracts = "dormant")
  expect_equal(list.files(tempdir(), pattern = "^dbparser_dpd_"), before)
})

test_that("parseHealthCanadaDPD attaches the metadata block", {
  dpd      <- parse_fixture(release = "2026-10", retrieved_at = "2026-10-21")
  metadata <- get_source_metadata(dpd)

  expect_equal(metadata$source, "health_canada_dpd")
  expect_equal(metadata$release, "2026-10")
  expect_true(is.na(metadata$release_date))
  expect_equal(metadata$retrieved_at, as.Date("2026-10-21"))
  expect_equal(metadata$licence, "OGL-Canada-2.0")
  expect_equal(metadata$licence_url,
               "https://open.canada.ca/en/open-government-licence-canada")
  expect_equal(metadata$attribution,
               paste("Contains information licensed under the Open",
                     "Government Licence – Canada."))

  files <- metadata$files[[1]]
  # 12 files for each of the three unzipped sets, the dormant zip, inactive
  expect_equal(NROW(files), 38L)
  expect_true("allfiles_dr.zip" %in% files$name)
  expect_false("drug_dr.txt" %in% files$name)
  if (getRversion() >= "4.5.0") {
    expect_true(all(grepl("^[0-9a-f]{64}$", files$sha256)))
  }
  expect_equal(attr(dpd, "original_db_info")$db_type, "HealthCanadaDPD")
})
