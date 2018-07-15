drake_context("intermediate file")

test_with_dir("responses to intermediate file", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  plan <- dbug_plan()
  command1 <- wide_deparse(quote({
    saveRDS(combined, file_out("intermediatefile.rds"))
    saveRDS(combined + 1, file_out("out2.rds"))
  }))
  command2 <- wide_deparse(quote({
    saveRDS(combined, "intermediatefile.rds")
    saveRDS(combined + 1, "out2.rds")
    file_out("intermediatefile.rds", "out2.rds")
  }))
  for (command in c(command1, command2)){
    plan$command[1] <- command
    plan$command[6] <- wide_deparse(quote({
      readRDS(file_in("intermediatefile.rds")) +
      readRDS(file_in("out2.rds"))
    }))
    config <- drake_config(plan = plan, targets = plan$target,
      envir = envir, parallelism = scenario$parallelism,
      jobs = scenario$jobs, verbose = FALSE,
      session_info = FALSE,
      log_progress = TRUE,
      caching = scenario$caching
    )
    testrun(config)
    expect_equal(justbuilt(config), sort(config$plan$target))
    expect_equal(outdated(config), character(0))
    final0 <- readd(final, search = FALSE)
    val <- readRDS("intermediatefile.rds")
    val2 <- readRDS("out2.rds")
    expect_equal(val + 1, val2)

    # actually change a file
    for (file in c("intermediatefile.rds", "out2.rds")){
      saveRDS(sum(val) + 100, file)
      testrun(config)
      expect_equal(justbuilt(config), "drake_target_1")
      expect_equal(final0, readd(final, search = FALSE))
      expect_equal(val, readRDS("intermediatefile.rds"))
      expect_equal(val2, readRDS("out2.rds"))
    }

    # break a file
    for (file in c("intermediatefile.rds", "out2.rds")){
      unlink(file, force = TRUE)
      testrun(config)
      expect_equal(justbuilt(config), "drake_target_1")
      expect_equal(final0, readd(final, search = FALSE))
      expect_equal(val, readRDS("intermediatefile.rds"))
      expect_equal(val2, readRDS("out2.rds"))
    }
    clean(destroy = TRUE)
  }
})
