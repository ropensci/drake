# Run this file to test drake on many different 
# combinations of parallelism, jobs, and 
# execution environment.
# For the Makefile parallelism to work, drake needs
# to be properly installed first.

library(magrittr)
library(testthat)
devtools::load_all()

setwd("..")

system.time(
  for (opt_name in names(test_opts)){
    os <- Sys.info()["sysname"] %>%
      tolower %>%
      unname
    set_test_opt(opt_name)
    cat(opt_name, "\n")
    opt <- test_opt()
    if (length(opt$skip_os))
      if (os %in% opt$skip_os){
        cat("  Skipping.\n")
        next
      }
    test_dir("testthat")
  }
)
