context("test parse onside nodes")

test_that(
  desc = "parse two side DB - default params",
  code = {
    local_edition(3)
    expect_snapshot(
      expect_error(parseTWOSIDES(twosides_file_path = "data_path")))
    data_path <- unzip(system.file("twoside_raw.zip",
                                   package = "dbparser"))
    twoside <- parseTWOSIDES(twosides_file_path = data_path)
    unlink(data_path, recursive = TRUE)
    expect_equal(length(twoside), 1)
    expect_equal(dim(twoside$drug_drug_interactions), c(1706, 13))
    expect_snapshot(show_dvobject_metadata(twoside))
  }
)
