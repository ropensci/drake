drake_context("command changes")

test_with_dir("changes to commands are handled well", {
  config <- dbug()
  expect_equal(
    sort(outdated(config)),
    sort(c(
      "'intermediatefile.rds'",
      "combined",
      "final",
      "myinput",
      "nextone",
      "yourinput"
    ))
  )
  config <- testrun(config)
  expect_equal(
    outdated(config),
    character(0)
  )
  config$plan$command[2] <- "f(1+ 1) # nothing should rebuild"
  config <- testrun(config)
  nobuild(config)
  config$plan$command[2] <- "f(1+ 1 -2 + 2) -1 + 1 #only yourinput changed"
  expect_equal(
    sort(outdated(config)),
    sort(c(
      "'intermediatefile.rds'",
      "combined",
      "final",
      "yourinput"
    ))
  )
  config <- testrun(config)
  expect_equal(justbuilt(config), "yourinput")
  config$plan$command[2] <- "f(1+2) # now downstream should rebuild"
  config <- testrun(config)
  expect_equal(
    justbuilt(config),
    sort(c(
      "'intermediatefile.rds'",
      "combined",
      "final",
      "yourinput"
    ))
  )
  config <- testrun(config)
  nobuild(config)

  # command changed for an intermediate file
  config$plan$command[1] <- "saveRDS(combined + 1, \"intermediatefile.rds\")"
  config <- testrun(config)
  expect_equal(
    justbuilt(config),
    sort(c("'intermediatefile.rds'", "final"))
  )
})
