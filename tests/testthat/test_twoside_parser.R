context("test parse onside nodes")

test_that(
  desc = "parse two side DB - default params",
  code = {
    local_edition(3)
    expect_snapshot(
      expect_error(parseTWOSIDES(twosides_file_path = "data_path")))
    data_path <- dirname(unzip(system.file("onside.zip",
                                           package = "dbparser"))[1])
    onside <- parseOnSIDES(dataDir = data_path)
    unlink(data_path, recursive = TRUE)
    expect_equal(length(onside), 8)
    expect_snapshot(show_dvobject_metadata(onside))
  }
)
