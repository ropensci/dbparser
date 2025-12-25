context("test different merge functions")

test_that(
  desc = "merge drugbank and onside",
  code = {
    local_edition(3)
    drugbank <- readRDS(system.file("two_drugs.RDS",
                                    package = "dbparser"))
    onside <- readRDS(system.file("onside.RDS",
                                  package = "dbparser"))
    expect_error(merge_drugbank_onsides(
      drugbank_db = list(),
      onsides_db  = onside),
    "`drugbank_db` must be a valid dvobject from parseDrugBank().")

    invalid_drugbank <- drugbank
    invalid_drugbank$drugs$external_identifiers <- NULL
    expect_error(merge_drugbank_onsides(
      drugbank_db = invalid_drugbank,
      onsides_db  = onside),
      "`drugbank_db` dvobject must contain external_identifiers data.")

    expect_error(merge_drugbank_onsides(
      drugbank_db = drugbank,
      onsides_db  = list()),
      "`onsides_db` must be a valid dvobject from parseOnSIDES().")

    all_dbs <- merge_drugbank_onsides(
      db_object = drugbank,
      onsides_db  = onside)
    expect_snapshot(show_dvobject_metadata(all_dbs))

  }
)
