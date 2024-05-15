Sys.setenv("R_TESTS" = "")
Sys.setenv("drake_session_info" = "false")

library(testthat)
library(drake)

# test_check("drake", reporter = "summary")
