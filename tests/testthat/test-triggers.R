drake_context("triggers")

test_with_dir("empty triggers return logical", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_identical(depend_trigger("x", list(), list()), FALSE)
  expect_identical(command_trigger("x", list(), list()), FALSE)
  expect_identical(file_trigger("x", list(), list()), FALSE)
  expect_identical(change_trigger("x", list(), list()), FALSE)
})

test_with_dir("triggers can be expressions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(x = 1)
  plan$trigger <- expression(trigger(condition = TRUE))
  for (i in 1:3) {
    config <- make(plan)
    expect_equal(justbuilt(config), "x")
  }
})

test_with_dir("triggers in plan override make(trigger = whatever)", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  saveRDS(1, "file.rds")
  plan <- drake_plan(
    x = readRDS(file_in("file.rds")),
    y = target(
      readRDS(file_in("file.rds")),
      trigger(file = TRUE)
    ),
    strings_in_dots = "literals"
  )
  config <- make(plan)
  expect_equal(sort(justbuilt(config)), c("x", "y"))
  saveRDS(2, "file.rds")
  expect_equal(sort(outdated(config)), c("x", "y"))
  config <- make(plan, trigger = trigger(file = FALSE))
  expect_equal(justbuilt(config), "y")
})

test_with_dir("change trigger on a fresh build", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  saveRDS(1, "file.rds")
  plan <- drake_plan(
    x = target(1 + 1, trigger(
      condition = FALSE,
      command = FALSE,
      depend = FALSE,
      file = FALSE,
      change = readRDS("file.rds"))
    ),
    strings_in_dots = "literals"
  )
  config <- make(plan, session_info = FALSE)
  expect_equal(justbuilt(config), "x")
  config <- make(plan, session_info = FALSE)
  expect_equal(justbuilt(config), character(0))
  saveRDS(2, "file.rds")
  config <- make(plan, session_info = FALSE)
  expect_equal(justbuilt(config), "x")
})

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

test_with_dir("triggers can be NA in the plan", {
  skip_on_cran()
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
    verbose = FALSE, caching = caching, session_info = FALSE
  )
  expect_equal(sort(justbuilt(config)), sort(config$plan$target))
  simple_plan <- plan
  simple_plan$trigger <- NULL
  simple_config <- make(
    simple_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE, caching = caching, session_info = FALSE
  )

  # Condition trigger
  for (i in 1:2){
    expect_equal(sort(outdated(config)), "condition")
    make(config = config)
    expect_equal(sort(justbuilt(config)), "condition")
  }
  saveRDS(FALSE, "condition.rds")
  expect_equal(outdated(simple_config), character(0))
  expect_equal(outdated(config), character(0))
  for (i in 1:2){
    make(config = config)
    nobuild(config)
  }

  # Change trigger
  saveRDS(2, "change.rds")
  expect_equal(sort(outdated(config)), "change")
  expect_equal(outdated(simple_config), character(0))
  make(config = config)
  expect_equal(sort(justbuilt(config)), "change")
  expect_equal(outdated(config), character(0))
  expect_equal(outdated(simple_config), character(0))

  # File trigger: input files
  saveRDS(2, "file.rds")
  expect_equal(sort(outdated(config)), "file")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "file")
  expect_equal(
    sort(outdated(simple_config)),
    sort(setdiff(config$plan$target, "file"))
  )
  expect_equal(outdated(config), character(0))

  # File trigger: knitr files
  writeLines("5678", "report.Rmd")
  expect_equal(sort(outdated(config)), "file")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "file")
  expect_equal(
    sort(outdated(simple_config)),
    sort(setdiff(config$plan$target, "file"))
  )
  expect_equal(outdated(config), character(0))

  # File trigger: output files
  for (target in plan$target){
    saveRDS("1234", paste0("out_", target, ".rds"))
  }
  expect_equal(sort(outdated(config)), "file")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "file")
  expect_equal(
    sort(outdated(simple_config)),
    sort(setdiff(config$plan$target, "file"))
  )
  expect_equal(outdated(config), character(0))

  # Done with the change trigger
  plan <- plan[1:5, ]
  config <- drake_config(
    plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE, caching = caching, log_progress = TRUE,
    session_info = FALSE
  )
  simple_plan <- simple_plan[1:5, ]
  simple_config <- drake_config(
    simple_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE, caching = caching, log_progress = TRUE,
    session_info = FALSE
  )
  make(config = simple_config)

  # Command trigger
  config$plan$command <- simple_config$plan$command <- paste0("
    knitr_in(\"report.Rmd\")
    out <- f(1 + readRDS(file_in(\"file.rds\")))
    saveRDS(out, file_out(\"out_", plan$target, ".rds\"))
    out
  ")
  expect_equal(sort(outdated(config)), "command")
  make(config = config)
  expect_equal(sort(justbuilt(config)), "command")
  expect_equal(
    sort(outdated(simple_config)),
    sort(setdiff(config$plan$target, "command"))
  )
  expect_equal(outdated(config), character(0))
  make(config = simple_config)
  expect_equal(outdated(config), character(0))

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
  expect_equal(
    sort(outdated(simple_config)),
    sort(setdiff(config$plan$target, "depend"))
  )
  expect_equal(outdated(config), character(0))
  make(config = simple_config)
  expect_equal(outdated(config), character(0))
})
