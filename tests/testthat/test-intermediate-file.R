drake_context("intermediate file")

test_with_dir("responses to intermediate file", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  plan <- dbug_plan()
  plan$command[6] <- wide_deparse(quote({
    readRDS(file_in("intermediatefile.rds")) +
    readRDS(file_in("out2.rds"))
  }))
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
    config <- drake_config(plan = plan, targets = plan$target,
      envir = envir, parallelism = scenario$parallelism,
      jobs = scenario$jobs, verbose = TRUE,
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

    # change what out2.rds is supposed to be
    config$plan$command[1] <- gsub("1", "2", plan$command[1])
    testrun(config)
    expect_equal(
      sort(justbuilt(config)),
      sort(c("drake_target_1", "final"))
    )
    expect_equal(final0 + 1, readd(final, search = FALSE))
    expect_equal(val, readRDS("intermediatefile.rds"))
    expect_equal(val2 + 1, readRDS("out2.rds"))
    clean(destroy = TRUE)
  }
})

test_with_dir("imported file_in file", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  eval(parse(text = "j <- function(x){
      knitr_in(\"report.Rmd\")
      file_in(\"a.rds\", \"b.rds\")
      x + 2 + c + readRDS(file_in(\"c.rds\"))
    }"),
    envir = envir
  )
  dbug_files()
  for (file in paste0(letters[1:3], ".rds")){
    saveRDS(1, file)
  }
  load_mtcars_example() # for report.Rmd
  config <- drake_config(dbug_plan(), envir = envir, verbose = 4)
  testrun(config)
  for (file in paste0(letters[1:2], ".rds")){
    saveRDS(2, file)
    testrun(config)
    expect_equal(sort(justbuilt(config)), sort(c("nextone", "yourinput")))
  }
  write("new content", file = "report.Rmd", append = TRUE)
  testrun(config)
  expect_equal(sort(justbuilt(config)), sort(c("nextone", "yourinput")))
  saveRDS(2, "c.rds")
  testrun(config)
  expect_equal(sort(justbuilt(config)), sort(c(
    "nextone", "yourinput", "combined", "drake_target_1", "final"
  )))
})
