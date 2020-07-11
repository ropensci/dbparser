Sys.setenv(R_TESTS="")
library(testthat)
library(dbparser)

test_check("dbparser")
