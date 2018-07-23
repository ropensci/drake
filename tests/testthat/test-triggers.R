drake_context("triggers")

test_with_dir("trigger() function works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- 1
  y <- trigger(
    condition = 1 + 1,
    command = TRUE,
    depend = FALSE,
    file = FALSE,
    change = sqrt(!!x)
  )
  z <- list(
    condition = quote(1 + 1),
    command = TRUE,
    depend = FALSE,
    file = FALSE,
    change = quote(sqrt(1))
  )
  expect_equal(y, z)
})

test_with_dir("can detect trigger deps", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  f <- function(x){
    identity(x)
  }
  plan <- drake_plan(
    x = target(
      command = 1 + 1,
      trigger = trigger(
        condition = f(FALSE),
        command = FALSE,
        file = FALSE,
        depend = TRUE,
        change = NULL
      )
    ),
    strings_in_dots = "literals"
  )
  config <- drake_config(
    plan, session_info = FALSE, cache = storr::storr_environment(),
    log_progress = TRUE)
  expect_equal(dependencies("x", config), "f")
  expect_equal(outdated(config), "x")
  make(config = config)
  expect_equal(justbuilt(config), "x")
  expect_equal(outdated(config), character(0))
  make(config = config)
  nobuild(config)
  f <- function(x){
    identity(x) || FALSE
  }
  expect_equal(outdated(config), "x")
  make(config = config)
  expect_equal(justbuilt(config), "x")
})

test_with_dir("empty triggers return logical", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_identical(depend_trigger("x", list(), list()), FALSE)
  expect_identical(command_trigger("x", list(), list()), FALSE)
  expect_identical(file_trigger("x", list(), list()), FALSE)
  expect_identical(change_trigger("x", list(), list()), FALSE)
})

test_with_dir("triggers can be NA in the plan", {
  expect_silent(
    config <- make(
      drake_plan(x = target(1, NA)),
      session_info = FALSE,
      cache = storr::storr_environment(),
      verbose = FALSE
    )
  )
  expect_equal(justbuilt(config), "x")
})

test_with_dir("trigger components react appropriately", {
  skip_on_cran()
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  caching <- scenario$caching
  eval(
    quote(f <- function(x){
      1 + x
    }),
    envir = e
  )
  writeLines("1234", "report.Rmd")
  saveRDS(1, "file.rds")
  saveRDS(1, "change.rds")
  saveRDS(TRUE, "condition.rds")
  plan <- drake_plan(
    missing = target(
      "",
      trigger(command = FALSE, depend = FALSE, file = FALSE)
    ),
    condition = target(
      "",
      trigger(
        condition = readRDS("condition.rds"),
        command = FALSE, depend = FALSE, file = FALSE
      )
    ),
    command = target(
      "",
      trigger(command = TRUE, depend = FALSE, file = FALSE)
    ),
    depend = target(
      "",
      trigger(command = FALSE, depend = TRUE, file = FALSE)
    ),
    file = target(
      "",
      trigger(command = FALSE, depend = FALSE, file = TRUE)
    ),
    change = target(
      "",
      trigger(
        change = readRDS("change.rds"),
        command = FALSE, depend = FALSE, file = FALSE
      )
    ),
    strings_in_dots = "literals"
  )
  plan$command <- paste0("
    knitr_in(\"report.Rmd\")
    out <- f(readRDS(file_in(\"file.rds\")))
    saveRDS(out, file_out(\"out_", plan$target, ".rds\"))
    out
  ")
  config <- make(
    plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE, caching = caching
  )
  expect_equal(sort(justbuilt(config)), sort(config$plan$target))

  # Condition trigger
  for (i in 1:2){
    expect_equal(sort(outdated(config)), "condition")
    make(config = config)
    expect_equal(sort(justbuilt(config)), "condition")
  }
  saveRDS(FALSE, "condition.rds")
  for (i in 1:2){
    make(config = config)
    nobuild(config)
  }

  # Change trigger
  saveRDS(2, "change.rds")
  expect_equal(sort(outdated(config)), "change")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "change")
  make(config = config)
  nobuild(config)

  # File trigger: input files
  saveRDS(2, "file.rds")
  expect_equal(sort(outdated(config)), "file")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "file")
  make(config = config)
  nobuild(config)

  # File trigger: knitr files
  writeLines("5678", "report.Rmd")
  expect_equal(sort(outdated(config)), "file")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "file")
  make(config = config)
  nobuild(config)

  # File trigger: output files
  for (target in plan$target){
    saveRDS("1234", paste0("out_", target, ".rds"))
  }
  expect_equal(sort(outdated(config)), "file")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "file")
  make(config = config)
  nobuild(config)

  # Command trigger
  config$plan$command <- paste0("
    knitr_in(\"report.Rmd\")
    out <- f(1 + readRDS(file_in(\"file.rds\")))
    saveRDS(out, file_out(\"out_", plan$target, ".rds\"))
    out
  ")
  expect_equal(sort(outdated(config)), "command")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "command")
  make(config = config)
  nobuild(config)

  # Depend trigger
  eval(
    quote(f <- function(x){
      2 + x
    }),
    envir = e
  )
  expect_equal(sort(outdated(config)), "depend")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "depend")
  make(config = config)
  nobuild(config)
})
