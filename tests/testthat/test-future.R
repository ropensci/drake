drake_context("future")

test_with_dir("future package functionality", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("future")
  future::plan(future::sequential)
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_mtcars_example(envir = e)
  backends <- rep("future", 2)
  caching <- c("master", "worker")
  for (i in seq_along(backends)) {
    clean(destroy = TRUE)
    make(
      e$my_plan,
      envir = e,
      parallelism = backends[i],
      caching = caching[i],
      jobs = 1,
      verbose = FALSE,
      session_info = FALSE,
      lock_envir = TRUE
    )
    config <- drake_config(
      e$my_plan,
      envir = e,
      parallelism = backends[i],
      caching = caching[i],
      jobs = 1,
      verbose = FALSE,
      session_info = FALSE,
      lock_envir = TRUE
    )
    expect_equal(
      outdated(config),
      character(0)
    )
    e$my_plan$command[[2]] <- as.call(
      c(quote(identity), e$my_plan$command[[2]])
    )
    make(
      e$my_plan,
      envir = e,
      parallelism = backends[i],
      caching = caching[i],
      jobs = 1,
      verbose = FALSE,
      session_info = FALSE,
      lock_envir = TRUE
    )
    expect_equal(justbuilt(config), "small")
  }

  # Stuff is already up to date.
  for (i in seq_along(backends)) {
    make(
      e$my_plan,
      envir = e,
      parallelism = backends[i],
      caching = caching[i],
      jobs = 1,
      verbose = FALSE,
      session_info = FALSE,
      lock_envir = TRUE
    )
    config <- drake_config(
      e$my_plan,
      envir = e,
      parallelism = backends[i],
      caching = caching[i],
      jobs = 1,
      verbose = FALSE,
      session_info = FALSE,
      lock_envir = TRUE
    )
    nobuild(config)
  }

  # Workers can wait for dependencies.
  for (i in 1:2) {
    e$my_plan$command[[2]] <- as.call(
      c(quote(identity), quote({Sys.sleep(2); simulate(48)}))
    )
    future::plan(future::multicore)
    make(
      e$my_plan,
      envir = e,
      parallelism = backends[i],
      caching = caching[i],
      jobs = 2,
      verbose = FALSE,
      session_info = FALSE,
      ensure_workers = FALSE
    )
    clean(destroy = TRUE)
  }
})

test_with_dir("can gracefully conclude a crashed worker", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("future")
  for (caching in c("master", "worker")) {
    con <- dbug()
    con$caching <- caching
    con$schedule <- con$graph
    worker <- structure(list(), target = "myinput")
    class(worker) <- "Future"
    expect_false(is_empty_worker(worker))
    expect_error(future::value(worker))
    expect_error(
      suppressWarnings(
        conclude_worker(
          worker = worker,
          config = con,
          queue = new_priority_queue(config = con)
        )
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
