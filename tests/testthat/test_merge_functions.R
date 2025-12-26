context("test different merge functions")
local_edition(3)

test_that("merge drugbank and onside", {
    drugbank <- readRDS(system.file("two_drugs.RDS",
                                    package = "dbparser"))
    onside <- readRDS(system.file("onside.RDS",
                                  package = "dbparser"))
    expect_error(merge_drugbank_onsides(
      db_object = list(),
      onsides_db  = onside),
    "`db_object` must contain a valid DrugBank dvobject.")

    invalid_drugbank <- drugbank
    invalid_drugbank$drugs$external_identifiers <- NULL
    expect_error(merge_drugbank_onsides(
      db_object = invalid_drugbank,
      onsides_db  = onside),
      "`drugbank_db` must contain external_identifiers data.")

    expect_error(merge_drugbank_onsides(
      db_object = drugbank,
      onsides_db  = list()),
      "`onsides_db` must be a valid dvobject from parseOnSIDES().")

    all_dbs <- merge_drugbank_onsides(
      db_object = drugbank,
      onsides_db  = onside)
    expect_snapshot(show_dvobject_metadata(all_dbs))

})


test_that("merge drugbank and twoside", {
  drugbank <- readRDS(system.file("two_drugs.RDS",
                                  package = "dbparser"))
  data_path <- unzip(system.file("twoside_raw.zip",
                                 package = "dbparser"))
  twoside <- parseTWOSIDES(twosides_file_path = data_path)

  expect_error(merge_drugbank_twosides(
    db_object = list(),
    twosides_db = list()),
    "`db_object` must contain a valid DrugBank dvobject.")

  invalid_drugbank <- drugbank
  invalid_drugbank$drugs$external_identifiers <- NULL
  expect_error(merge_drugbank_twosides(
    db_object   = invalid_drugbank,
    twosides_db = list()),
    "`drugbank_db` must contain external_identifiers data.")

  expect_error(merge_drugbank_twosides(
    db_object   = drugbank,
    twosides_db = list()),
    "`twosides_db` must be a valid dvobject from parseTWOSIDES().")

  all_dbs <- merge_drugbank_twosides(
    db_object   = drugbank,
    twosides_db = twoside)
  expect_snapshot(show_dvobject_metadata(all_dbs))
  unlink(data_path, recursive = TRUE)

})


test_that("merge chain", {
  drugbank <- readRDS(system.file("two_drugs.RDS",
                                  package = "dbparser"))
  data_path <- unzip(system.file("twoside_raw.zip",
                                 package = "dbparser"))
  onside <- readRDS(system.file("onside.RDS",
                                package = "dbparser"))
  twoside <- parseTWOSIDES(twosides_file_path = data_path)
  unlink(data_path, recursive = TRUE)

  final_db_1 <- drugbank %>%
    merge_drugbank_onsides(onsides_db = onside) %>%
    merge_drugbank_twosides(twosides_db = twoside)

  final_db_2 <- drugbank %>%
    merge_drugbank_twosides(twosides_db = twoside) %>%
    merge_drugbank_onsides(onsides_db = onside)

  expect_all_true(class(final_db_1) %in% class(final_db_2))
  expect_true(length(class(final_db_1)) == length(class(final_db_2)))

  expect_all_true(names(final_db_1) %in% names(final_db_2))
  expect_true(length(names(final_db_1)) == length(names(final_db_2)))

  expect_all_true(names(final_db_1$drugbank) %in% names(final_db_2$drugbank))
  expect_true(length(names(final_db_1$drugbank)) == length(names(final_db_2$drugbank)))

  expect_all_true(names(final_db_1$onsides) %in% names(final_db_2$onsides))
  expect_true(length(names(final_db_1$onsides)) == length(names(final_db_2$onsides)))

  expect_all_true(names(final_db_1$twosides) %in% names(final_db_2$twosides))
  expect_true(length(names(final_db_1$twosides)) == length(names(final_db_2$twosides)))

  expect_all_true(names(final_db_1$integrated_data) %in% names(final_db_2$integrated_data))
  expect_true(length(names(final_db_1$integrated_data)) == length(names(final_db_2$integrated_data)))
})
