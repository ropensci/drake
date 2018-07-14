drake_context("triggers")

test_with_dir("empty triggers return logical", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_identical(depends_trigger("x", list(), list()), FALSE)
  expect_identical(command_trigger("x", list(), list()), FALSE)
  expect_identical(file_trigger("x", list(), list()), FALSE)
})

test_with_dir("triggers work as expected", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  for (trigger in setdiff(triggers(), c("always", triggers_with_command()))){
    con$plan$trigger <- trigger
    expect_equal(outdated(config = con), character(0))
  }
  for (trigger in triggers_with_command()){
    con$plan$trigger[con$plan$target == "combined"] <- trigger
    expect_equal(sort(outdated(config = con)),
      sort(c("combined", "final", "drake_target_1")))
  }
  con$plan$command[con$plan$target == "combined"] <- cmd

  # Destroy a file target.
  file.rename("intermediatefile.rds", "tmp")
  check_file <- function(con){
    for (trigger in setdiff(triggers(), c("always", triggers_with_file()))){
      con$plan$trigger <- trigger
      expect_equal(outdated(config = con), character(0))
    }
    for (trigger in triggers_with_file()){
      con$plan$trigger[con$plan$target == "drake_target_1"] <- trigger
      expect_equal(sort(outdated(config = con)),
        sort(c("final", "drake_target_1")))
    }
  }
  check_file(con)

  # Restore the file target.
  file.rename("tmp", "intermediatefile.rds")
  con$plan$trigger <- "always"
  expect_equal(sort(outdated(config = con)), sort(con$plan$target))
  for (trigger in setdiff(triggers(), "always")){
    con$plan$trigger <- trigger
    expect_equal(outdated(config = con), character(0))
  }

  # Corrupt a file target
  value <- readRDS("intermediatefile.rds") + 1
  saveRDS(value, "intermediatefile.rds")
  check_file(con)

  # Nothing is built (global missing trigger)
  con$plan$command[con$plan$target == "yourinput"] <- "1+2"
  file.rename("intermediatefile.rds", "tmp")
  con$plan$trigger <- NULL
  con <- make(
    con$plan, trigger = "missing",
    envir = con$envir, verbose = TRUE, session_info = FALSE)
  expect_equal(justbuilt(con), character(0))

  # Global trigger is overridden
  con$plan$trigger <- "missing"
  con <- make(
    con$plan, trigger = "command",
    envir = con$envir, verbose = TRUE, session_info = FALSE)
  expect_equal(justbuilt(con), character(0))

  # 'always' trigger rebuilts up-to-date targets
  con$plan$trigger <- "any"
  con <- make(con$plan, envir = con$envir, session_info = FALSE)
  out <- outdated(con)
  expect_equal(out, character(0))
  con$plan$trigger[con$plan$target == "final"] <- "always"
  con2 <- make(
    con$plan, parallelism = con$parallelism,
    envir = con$envir, jobs = con$jobs, verbose = FALSE,
    session_info = FALSE)
  expect_equal(justbuilt(con2), "final")
})

test_with_dir("all triggers bring targets up to date", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  for (trigger in triggers()){
    clean(destroy = TRUE)
    con <- dbug()
    con$plan$trigger <- trigger
    con <- make(
      con$plan, parallelism = con$parallelism,
      envir = con$envir, jobs = con$jobs, verbose = FALSE,
      session_info = FALSE)
    expect_equal(sort(justbuilt(con)), sort(con$plan$target))
    con$plan$trigger <- NULL
    out <- outdated(con)
    expect_equal(out, character(0))
  }
})

# Similar enough to the triggers to include here:
test_with_dir("make(..., skip_imports = TRUE) works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  con <- dbug()
  verbose <- max(con$jobs) < 2 &&
    targets_setting(con$parallelism) == "parLapply"
  suppressMessages(
    con <- make(
      con$plan, parallelism = con$parallelism,
      envir = con$envir, jobs = con$jobs, verbose = verbose,
      hook = silencer_hook,
      skip_imports = TRUE,
      session_info = FALSE
    )
  )
  expect_equal(
    sort(cached()),
    sort(c("\"intermediatefile.rds\"", con$plan$target))
  )

  # If the imports are already cached, the targets built with
  # skip_imports = TRUE should be up to date.
  make(con$plan, verbose = FALSE, envir = con$envir, session_info = FALSE)
  clean(list = con$plan$target, verbose = FALSE)
  suppressMessages(
    con <- make(
      con$plan, parallelism = con$parallelism,
      envir = con$envir, jobs = con$jobs, verbose = verbose,
      hook = silencer_hook,
      skip_imports = TRUE, session_info = FALSE
    )
  )
  out <- outdated(con)
  expect_equal(out, character(0))
})
