context("test parse onside nodes")

test_that(
  desc = "parse two side DB - default params",
  code = {
    local_edition(3)
    expect_snapshot(
      expect_error(parseTWOSIDES(twosides_file_path = "data_path")))
    unzip_dir <- tempfile("twoside_")
    data_path <- unzip(system.file("twoside_raw.zip",
                                   package = "dbparser"),
                       exdir = unzip_dir)
    twoside <- parseTWOSIDES(twosides_file_path = data_path)
    unlink(unzip_dir, recursive = TRUE)
    expect_equal(length(twoside), 1)
    expect_equal(dim(twoside$drug_drug_interactions), c(1706, 13))
    expect_snapshot(show_dvobject_metadata(twoside))
  }
)
