Sys.setenv("R_TESTS" = "")

library(testthat)
library(drake)

test_check("drake")
