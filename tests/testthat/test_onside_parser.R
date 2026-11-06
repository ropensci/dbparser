context("test parse onside nodes")

test_that(
  desc = "parse onside DB - default params",
  code = {
    local_edition(3)
    expect_snapshot(expect_error(parseOnSIDES(dataDir = "data_path")))
    unzip_dir <- tempfile("onside_")
    data_path <- dirname(unzip(system.file("onside.zip",
                                           package = "dbparser"),
                               exdir = unzip_dir)[1])
    onside <- parseOnSIDES(dataDir = data_path)
    unlink(unzip_dir, recursive = TRUE)
    expect_equal(length(onside), 8)
    expect_snapshot(show_dvobject_metadata(onside))
  }
)
