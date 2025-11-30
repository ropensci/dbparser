context("test utils functions")

library(testthat)

biotech <- "drugbank_record_biotech.xml"
test_that(
  desc = "Read dvobject metadata",
  code = {
    dvobj <- readRDS(system.file("two_drugs.RDS",
                                 package = "dbparser"))

    new_dvobj <- testthat::expect_warning(
      subset_drugbank_dvobject(dvobject = dvobj,
                               drug_ids = ""),
      "`drug_ids` is empty. Returning NULL")
    expect_equal(length(new_dvobj), 0)

    new_dvobj <- subset_drugbank_dvobject(dvobject = dvobj,
                                          drug_ids = "DB00007")
    expect_equal(length(new_dvobj), 5)
    expect_equal(NROW(new_dvobj$drugs$general_information), 1)
    expect_equal(length(new_dvobj$cett), 1)
  }
)
