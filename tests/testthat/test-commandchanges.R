# library(testthat); library(devtools); load_all()
context("commandchanges")
source("utils.R")

test_that("changes to commands are handled well", {
  dclean()
  args = dbug()
  run(args$plan, envir = args$envir, verbose = F)
  args$plan$command[2] = "f(1+ 1) # nothing should rebuild"
  run(args$plan, envir = args$envir, verbose = F)
  nobuild(args)
  args$plan$command[2] = "f(1+ 1 -2 + 2) -1 + 1 # only this target should rebuild"
  run(args$plan, envir = args$envir, verbose = F)
  expect_equal(justbuilt(), "yourinput")
  args$plan$command[2] = "f(1+2) # now, everything downstream should rebuild"
  run(args$plan, envir = args$envir, verbose = F)
  expect_equal(justbuilt(), 
    c("'intermediatefile.rds'", "combined", "final", "yourinput"))
  run(args$plan, envir = args$envir, verbose = F)
  nobuild(args)
  args$plan$command[1] = "saveRDS(combined + 1, \"intermediatefile.rds\")"
  run(args$plan, envir = args$envir, verbose = F)
  expect_equal(justbuilt(), c("'intermediatefile.rds'", "final"))
  dclean()
})
