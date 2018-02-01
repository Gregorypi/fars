Sys.setenv("R_TESTS" = "")

library(testthat)
library(fars)

test_check("fars")
