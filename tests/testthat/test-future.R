drake_context("future")

test_with_dir("future package functionality", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("txtq")
  future::plan(future::sequential)
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_mtcars_example(envir = e)
  backends <- c("future_lapply", rep("future", 2), "future_lapply_staged")
  caching <- c(rep("worker", 2), rep("master", 2))
  for (i in 1:4){
    clean(destroy = TRUE)
    config <- make(
      e$my_plan,
      envir = e,
      parallelism = backends[i],
      caching = caching[i],
      jobs = 1,
      verbose = 4,
      session_info = FALSE
    )
    expect_equal(
      outdated(config),
      character(0)
    )
  }

  # Stuff is already up to date.
  for (i in 1:4){
    config <- make(
      e$my_plan,
      envir = e,
      parallelism = backends[i],
      caching = caching[i],
      jobs = 1,
      verbose = FALSE,
      session_info = FALSE
    )
    expect_equal(justbuilt(config), character(0))
  }

  # Workers can wait for dependencies.
  e$my_plan$command[2] <- "Sys.sleep(2); simulate(48)"
  future::plan(future::multicore)
  make(
    e$my_plan,
    envir = e,
    parallelism = backends[3],
    caching = caching[3],
    jobs = c(imports = 1, targets = 2),
    verbose = FALSE,
    session_info = FALSE,
    ensure_workers = FALSE
  )
})

test_with_dir("prepare_distributed() writes cache folder if nonexistent", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  config$cache_path <- "nope"
  prepare_distributed(config)
  expect_true(file.exists("nope"))
})

test_with_dir("can gracefully conclude a crashed worker", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  for (caching in c("master", "worker")){
    con <- dbug()
    con$caching <- caching
    con$schedule <- con$graph
    worker <- structure(list(), target = "myinput")
    class(worker) <- "Future"
    expect_false(is_empty_worker(worker))
    expect_error(future::value(worker))
    expect_error(
      expect_warning(
        conclude_worker(
          worker = worker,
          config = con,
          queue = new_priority_queue(config = con)
        ),
        regexp = "checksum"
      ),
      regexp = "failed"
    )
    meta <- diagnose(myinput)
    expect_true(
      grepl(
        "Worker terminated unexpectedly",
        meta$error$message,
        fixed = TRUE
      )
    )
    clean(destroy = TRUE)
  }
})
