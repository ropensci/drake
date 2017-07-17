# library(testthat); library(devtools); load_all()
context("command-changes")

test_that("changes to commands are handled well", {
  dclean()
  config = dbug()
  expect_equal(sort(outdated(config$plan, envir = config$envir)), 
    sort(c("'intermediatefile.rds'", "combined", "final",
      "myinput", "nextone", "yourinput")))
  testrun(config)
  expect_equal(outdated(config$plan, envir = config$envir), character(0))
  config$plan$command[2] = "f(1+ 1) # nothing should rebuild"
  testrun(config)
  nobuild(config)
  config$plan$command[2] = "f(1+ 1 -2 + 2) -1 + 1 #only yourinput changed"
  expect_equal(sort(outdated(config$plan, envir = config$envir)),
    sort(c("'intermediatefile.rds'", "combined", "final", "yourinput")))
  testrun(config)
  expect_equal(justbuilt(config), "yourinput")
  config$plan$command[2] = "f(1+2) # now downstream should rebuild"
  testrun(config)
  expect_equal(justbuilt(config), 
    sort(c("'intermediatefile.rds'", "combined", "final", "yourinput")))
  testrun(config)
  nobuild(config)
  
  # command changed for an intermediate file
  config$plan$command[1] = 
    "saveRDS(combined + 1, \"intermediatefile.rds\")"
  testrun(config)
  expect_equal(justbuilt(config), 
    sort(c("'intermediatefile.rds'", "final")))
  dclean()
})
