if (FALSE) {

drake_context("always skipped")

test_with_dir("imported online file with no internet", {
  # Disconnect from the internet.
  plan <- drake_plan(
    x = file_in("https://github.com/ropensci/drake/archive/v7.3.0.tar.gz")
  )
  expect_error(make(plan), regexp = "no internet. Cannot check url")
})

test_with_dir("time stamps and large files", {
  # Reconnect to the internet.
  skip_if_not_installed("downloader")
  dir_csv <- tempfile()
  file_zip <- tempfile()
  file_csv <- tempfile()
  file_large <- tempfile()
  log1 <- tempfile()
  log2 <- tempfile()
  log3 <- tempfile()
  log4 <- tempfile()
  downloader::download(
    "http://eforexcel.com/wp/wp-content/uploads/2017/07/1500000%20Sales%20Records.zip", # nolint
    file_zip,
    quiet = TRUE
  )
  dir.create(dir_csv)
  utils::unzip(file_zip, exdir = dir_csv)
  tmp <- file.rename(
    file.path(dir_csv, list.files(dir_csv, pattern = "csv$")[1]),
    file_csv
  )
  for (i in 1:58) {
    message(i)
    file.append(file_large, file_csv)
  }
  plan <- drake_plan(x = file_in(!!file_large))
  cache <- storr::storr_rds(tempfile())
  config <- drake_config(plan, cache = cache)
  make(plan, cache = cache, console_log_file = log1)
  expect_equal(justbuilt(config), "x")
  make(plan, cache = cache, console_log_file = log2)
  expect_equal(justbuilt(config), character(0))
  tmp <- file.append(file_large, file_csv)
  make(plan, cache = cache, console_log_file = log3)
  expect_equal(justbuilt(config), "x")
  system2("touch", file_large)
  make(plan, cache = cache, console_log_file = log4)
  expect_equal(justbuilt(config), character(0))
  # Now, compare the times stamps on the logs.
  # Make sure the imported file takes a long time to process the first time
  # but is instantaneous the second time.
  # The third time, the file changed, so the processing time
  # should be longer. Likewise for the fourth time because
  # the file was touched.
  message(paste(readLines(log1), collapse = "\n"))
  message(paste(readLines(log2), collapse = "\n"))
  message(paste(readLines(log3), collapse = "\n"))
  message(paste(readLines(log4), collapse = "\n"))
  # Now remove those huge files!
  files <- c(dir_csv, file_zip, file_csv, file_large)
  unlink(files, recursive = TRUE, force = TRUE)
})

test_with_dir("use_drake()", {
  # Load drake with library(drake)
  # and not with devtools::load_all().
  # Reason: https://github.com/r-lib/usethis/issues/347
  # If that problem is ever solved, we should move this test
  # to tests/testthat/test-examples.R.
  skip_if_not_installed("usethis")
  usethis::create_project(".", open = FALSE, rstudio = FALSE)
  files <- c("make.R", "_drake.R")
  for (file in files) {
    expect_false(file.exists(file))
  }
  use_drake(open = FALSE)
  for (file in files) {
    expect_true(file.exists(file))
  }
})

test_with_dir("can keep going in parallel", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(
    a = stop(123),
    b = a + 1
  )
  make(
    plan,
    jobs = 2,
    session_info = FALSE,
    keep_going = TRUE,
    verbose = 0L
  )
  expect_error(readd(a))
  expect_equal(readd(b), numeric(0))
})

test_with_dir("drake_debug()", {
  skip_on_cran()
  load_mtcars_example()
  my_plan$command[[2]] <- quote({
    simulate(48)
    stop(1234)
  })
  config <- drake_config(my_plan, lock_envir = TRUE)
  expect_error(make(my_plan), regexp = "1234")
  out <- drake_debug(large, config = config)
  out <- drake_debug(
    "large",
    config = config,
    verbose = 0L,
    character_only = TRUE
  )
  expect_true(is.data.frame(out))
  for (i in 1:2) {
    clean(destroy = TRUE)
    load_mtcars_example()
    make(my_plan)
    config <- drake_config(my_plan)
    expect_true(config$cache$exists("small"))
    clean(small)
    expect_false(config$cache$exists("small"))
    out <- drake_debug(small, config = config)
    expect_false(config$cache$exists("small"))
    expect_true(is.data.frame(out))
  }
})

test_with_dir("clustermq error messages get back to master", {
  plan <- drake_plan(a = stop(123))
  options(clustermq.scheduler = "multicore")
  for (caching in c("worker", "master")) {
    expect_error(
      make(
        plan,
        parallelism = "clustermq",
        caching = "worker"
      ),
      regexp = "123"
    )
  }
})

test_with_dir("forks + lock_envir = informative error msg", {
  # Don't run this test for real because (1) we would have to add
  # furrr to "Suggests" and (2) at some point, base R may be patched
  # so forking in the parallel package does not give this warning anyway.
  regexp <- "workaround"
  plan <- drake_plan(x = parallel::mclapply(1:2, identity, mc.cores = 2))
  expect_warning(
    make(plan, envir = globalenv(), lock_envir = TRUE),
    regexp = regexp
  )
  future::plan(future::multicore)
  plan <- drake_plan(
    # install.packages("furrr") # nolint
    # Not in "Suggests"
    x = eval(parse(text = "furrr::future_map(1:2, function(x) Sys.getpid())"))
  )
  expect_error(
    make(plan, envir = globalenv(), lock_envir = TRUE),
    regexp = regexp
  )
})

test_with_dir("make() in interactive mode", {
  # Must run this test in a fresh new interactive session.
  # Cannot be fully automated like the other tests.
  options(drake_make_menu = TRUE, drake_clean_menu = TRUE)
  load_mtcars_example()
  config <- drake_config(my_plan)
  make(my_plan) # Select 2.
  expect_equal(cached(), character(0))
  expect_equal(sort(outdated(config)), sort(my_plan$target))
  expect_equal(sort(justbuilt(config)), character(0))
  make(my_plan) # No menu
  expect_equal(cached(), sort(my_plan$target))
  expect_equal(sort(outdated(config)), character(0))
  expect_equal(sort(justbuilt(config)), sort(my_plan$target))
  clean() # Select 1.
  .pkg_envir$drake_make_menu <- NULL
  make(my_plan) # Select 1.
  expect_equal(cached(), sort(my_plan$target))
  expect_equal(sort(outdated(config)), character(0))
  expect_equal(sort(justbuilt(config)), sort(my_plan$target))
  clean() # No menu
  .pkg_envir$drake_make_menu <- NULL
  options(drake_make_menu = FALSE)
  make(my_plan) # No menu.
  expect_equal(sort(outdated(config)), character(0))
  expect_equal(sort(justbuilt(config)), sort(my_plan$target))
  unlink(".drake", recursive = TRUE)
  .pkg_envir$drake_make_menu <- NULL
  options(drake_make_menu = TRUE)
  make(my_plan) # Select 0.
  expect_equal(sort(outdated(config)), sort(my_plan$target))
  expect_equal(sort(justbuilt(config)), character(0))
})

test_with_dir("clean() in interactive mode", {
  # Must run this test in a fresh new interactive session.
  # Cannot be fully automated like the other tests.
  options(drake_make_menu = TRUE, drake_clean_menu = TRUE)
  load_mtcars_example()
  config <- drake_config(my_plan)
  make(my_plan) # Select 1.
  expect_equal(sort(cached()), sort(my_plan$target))
  clean() # Select 2.
  expect_equal(sort(cached()), sort(my_plan$target))
  .pkg_envir$drake_clean_menu <- NULL
  clean() # Select 0.
  expect_equal(sort(cached()), sort(my_plan$target))
  .pkg_envir$drake_clean_menu <- NULL
  clean() # Select 1.
  expect_equal(sort(cached()), character(0))
  make(my_plan)
  expect_equal(sort(cached()), sort(my_plan$target))
  clean() # No menu.
  expect_equal(sort(cached()), character(0))
  make(my_plan)
  .pkg_envir$drake_clean_menu <- NULL
  options(drake_clean_menu = FALSE)
  clean() # No menu.
  expect_equal(sort(cached()), character(0))
})

test_with_dir("r_make() + clustermq", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("callr")
  if (identical(Sys.getenv("drake_skip_callr"), "true")) {
    skip("Skipping callr tests.")
  }
  writeLines(
    c(
      "library(drake)",
      "load_mtcars_example()",
      "options(clustermq.scheduler = \"multicore\")",
      "drake_config(my_plan, parallelism = \"clustermq\", jobs = 2)"
    ),
    "_drake.R"
  )
  expect_true(length(r_outdated()) > 1)
  r_make()
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(), character(0))
})

test_with_dir("r_make() + multicore future", {
  skip_on_cran()
  skip_on_os("windows")
  skip_if_not_installed("callr")
  if (identical(Sys.getenv("drake_skip_callr"), "true")) {
    skip("Skipping callr tests.")
  }
  writeLines(
    c(
      "library(drake)",
      "load_mtcars_example()",
      "future::plan(future::multicore)",
      "drake_config(my_plan, parallelism = \"future\", jobs = 2)"
    ),
    "_drake.R"
  )
  expect_true(length(r_outdated()) > 1)
  r_make()
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(), character(0))
})

test_with_dir("Output from the callr RStudio addins", {
  skip_on_cran()
  skip_if_not_installed("callr")
  skip_if_not_installed("knitr")
  if (identical(Sys.getenv("drake_skip_callr"), "true")) {
    skip("Skipping callr tests.")
  }
  writeLines(
    c(
      "library(drake)",
      "load_mtcars_example()",
      "drake_config(my_plan, verbose = FALSE)"
    ),
    default_drake_source
  )
  r_args <- list(show = FALSE)
  expect_true(length(rs_addin_r_outdated(r_args)) > 1) # Should print.
  rs_addin_r_make(r_args)
  expect_equal(rs_addin_r_outdated(r_args), character(0)) # Should print.
  skip_if_not_installed("visNetwork")
  graph <- rs_addin_r_vis_drake_graph(r_args) # Should show a graph.
  expect_true(inherits(graph, "visNetwork"))
})

}
