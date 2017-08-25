Sys.setenv("R_TESTS" = "")

library(testthat)
library(drake)

dir <- file.path("testthat", "workspaces")
unlink(dir, recursive = TRUE, force = TRUE)
test_check("drake")
unlink(dir, recursive = TRUE, force = TRUE)
