drake_context("triggers")

test_with_dir("triggers work as expected", {
  con <- dbug()
  con$plan$trigger <- "missing"
  con <- testrun(config = con)
  expect_equal(outdated(config = con), character(0))

  # Illegal trigger
  con$plan$trigger[1] <- 5
  expect_error(testrun(con))
  con$plan$trigger[1] <- "missing"

  # Change a command.
  cmd <- con$plan$command[con$plan$target == "combined"]
  con$plan$command[con$plan$target == "combined"] <-
    "nextone + yourinput + 1"
  for (trigger in setdiff(triggers(), triggers_with_command())){
    con$plan$trigger <- trigger
    expect_equal(outdated(config = con), character(0))
  }
  for (trigger in triggers_with_command()){
    con$plan$trigger[con$plan$target == "combined"] <- trigger
    expect_equal(sort(outdated(config = con)),
      sort(c("combined", "final", "'intermediatefile.rds'")))
  }
  con$plan$command[con$plan$target == "combined"] <- cmd

  # Destroy a file target.
  file.rename("intermediatefile.rds", "tmp")
  check_file <- function(con){
    for (trigger in setdiff(triggers(), triggers_with_file())){
      con$plan$trigger <- trigger
      expect_equal(outdated(config = con), character(0))
    }
    for (trigger in triggers_with_file()){
      con$plan$trigger[con$plan$target == "'intermediatefile.rds'"] <- trigger
      expect_equal(sort(outdated(config = con)),
        sort(c("final", "'intermediatefile.rds'")))
    }
  }
  check_file(con)

  # Restore the file target.
  file.rename("tmp", "intermediatefile.rds")
  for (trigger in triggers()){
    con$plan$trigger <- trigger
    expect_equal(outdated(config = con), character(0))
  }

  # Corrupt a file target
  value <- readRDS("intermediatefile.rds") + 1
  saveRDS(value, "intermediatefile.rds")
  check_file(con)

  # Rush builds nothing:
  con$plan$command[con$plan$target == "yourinput"] <- "1+2"
  file.rename("intermediatefile.rds", "tmp")
  con <- make(
    con$plan, rush = TRUE, parallelism = con$parallelism,
    envir = con$envir, jobs = con$jobs, verbose = FALSE)
  expect_equal(justbuilt(con), character(0))

  # Rush builds just one thing.
  clean(combined)
  con <- make(
    con$plan, rush = TRUE, parallelism = con$parallelism,
    envir = con$envir, jobs = con$jobs, verbose = FALSE)
  expect_equal(justbuilt(con), "combined")

  # Rush does not build imports
  clean()
  con <- make(
    con$plan, rush = TRUE, parallelism = con$parallelism,
    envir = con$envir, jobs = con$jobs, verbose = FALSE)
  expect_equal(sort(justbuilt(con)), sort(con$plan$target))
  expect_true(all(con$plan$trigger == "missing"))


})
