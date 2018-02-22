drake_context("future")

test_with_dir("future package functionality", {
  future::plan(future::sequential)
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  load_basic_example(envir = e)
  backends <- c("future_lapply", rep("future", 2))
  caching <- c(rep("worker", 2), "master")
  for (i in 1:3){
    clean(destroy = TRUE)
    withr::with_options(
      new = list(mc.cores = 2), code = {
        config <- make(
          e$my_plan,
          envir = e,
          parallelism = backends[i],
          caching = caching[i],
          jobs = 1,
          verbose = FALSE,
          session_info = FALSE
        )
      }
    )
    expect_equal(
      outdated(config),
      character(0)
    )
  }

  # Stuff is already up to date.
  config <- make(
    e$my_plan,
    envir = e,
    parallelism = backends[3],
    caching = caching[3],
    jobs = 1,
    verbose = FALSE,
    session_info = FALSE
  )
  expect_equal(justbuilt(config), character(0))

  # Workers can wait for dependencies.
  e$my_plan$command[2] <- "Sys.sleep(2); simulate(48)"
  future::plan(future::multicore)
  make(
    e$my_plan,
    envir = e,
    parallelism = backends[3],
    caching = caching[3],
    jobs = 2,
    verbose = FALSE,
    session_info = FALSE
  )
})

test_with_dir("prepare_distributed() writes cache folder if nonexistent", {
  config <- dbug()
  config$cache_path <- "nope"
  prepare_distributed(config)
  expect_true(file.exists("nope"))
})

test_with_dir("can gracefully conclude a crashed worker", {
  for (caching in c("master", "worker")){
    con <- dbug()
    con$caching <- caching
    con$execution_graph <- con$graph
    worker <- structure(list(), target = "myinput")
    class(worker) <- "Future"
    expect_false(is_empty_worker(worker))
    expect_error(future::value(worker))
    expect_error(
      conclude_worker(
        worker = worker,
        config = con,
        queue = new_target_queue(config = con)
      ),
      regexp = "failed. Use diagnose"
    )
    meta <- diagnose(myinput)
    expect_true(grepl("Worker terminated unexpectedly", meta$error$message))
    clean(destroy = TRUE)
  }
})
