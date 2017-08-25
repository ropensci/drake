Sys.setenv("R_TESTS" = "")

library(testthat)
library(drake)

unlink(file.path("testthat", "workspaces"), recursive = TRUE)
test_check("drake")
