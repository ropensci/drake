drake_context("command changes")

test_with_dir("changes to commands are handled well", {
  config <- dbug()
  expect_equal(
    sort(outdated(config)),
    sort(c(
      "drake_target_1",
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
  config$layout <- whole_static_analysis(
    plan = config$plan,
    envir = config$envir,
    cache = config$cache
  )$layout
  config <- testrun(config)
  nobuild(config)
  config$plan$command[2] <- "f(1+ 1 -2 + 2) -1 + 1 #only yourinput changed"
  config$layout <- whole_static_analysis(
    plan = config$plan,
    envir = config$envir,
    cache = config$cache
  )
  expect_equal(
    sort(outdated(config)),
    sort(c(
      "drake_target_1",
      "combined",
      "final",
      "yourinput"
    ))
  )
  config <- testrun(config)
  expect_equal(justbuilt(config), "yourinput")
  config$plan$command[2] <- "f(1+2) # now downstream should rebuild"
  config$layout <- whole_static_analysis(
    plan = config$plan,
    envir = config$envir,
    cache = config$cache
  )$layout
  config <- testrun(config)
  expect_equal(
    justbuilt(config),
    sort(c(
      "drake_target_1",
      "combined",
      "final",
      "yourinput"
    ))
  )
  config <- testrun(config)
  nobuild(config)

  # command changed for an intermediate file
  config$plan$command[1] <-
    "saveRDS(combined + 1, file_out(\"intermediatefile.rds\"))"
  config$layout <- whole_static_analysis(
    plan = config$plan,
    envir = config$envir,
    cache = config$cache
  )$layout
  config <- testrun(config)
  expect_equal(
    justbuilt(config),
    sort(c("drake_target_1", "final"))
  )
})

test_with_dir("add a new target", {
  plan <- drake_plan(a = as.integer(sqrt(4)))
  cache <- storr::storr_environment()
  config <- make(plan, cache = cache, session_info = FALSE)
  expect_equal(justbuilt(config), "a")
  expect_equal(readd(a, cache = cache), 2L)
  plan <- rbind(plan, drake_plan(b = as.integer(sqrt(16))))
  config <- make(plan, cache = cache, session_info = FALSE)
  expect_equal(justbuilt(config), "b")
  expect_equal(readd(b, cache = cache), 4L)
})
