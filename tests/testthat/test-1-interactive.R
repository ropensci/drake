drake_context("interactive")

test_with_dir("print.drake_deps()", {
  skip_on_cran()
  x <- drake_deps(quote(x)) # print by hand
  m <- utils::capture.output(print(x))
  expect_true(any(grepl("drake_deps", m)))
})

test_with_dir("print.drake_deps_ht()", {
  skip_on_cran()
  x <- drake_deps_ht(quote(x)) # print by hand
  m <- utils::capture.output(print(x))
  expect_true(any(grepl("drake_deps_ht", m)))
})

test_with_dir("print.drake_settings()", {
  skip_on_cran()
  x <- drake_settings() # print by hand
  m <- utils::capture.output(print(x))
  expect_true(any(grepl("drake_settings", m)))
})

test_with_dir("print.drake_triggers()", {
  skip_on_cran()
  x <- trigger() # print by hand
  m <- utils::capture.output(print(x))
  expect_true(any(grepl("drake_triggers", m)))
})

test_with_dir("drake spec print method", {
  skip_on_cran()
  f <- identity
  plan <- drake_plan(x = 1)
  config <- drake_config(plan)
  x1 <- config$spec$f # print by hand
  x2 <- config$spec$x # print by hand
  m1 <- utils::capture.output(print(x1))
  m2 <- utils::capture.output(print(x2))
  expect_true(any(grepl("specification of import f", m1)))
  expect_true(any(grepl("specification of target x", m2)))
})

test_with_dir("drake_config() print method", {
  skip_on_cran()
  x <- drake_config(drake_plan(y = 1)) # print by hand
  m <- utils::capture.output(print(x))
  expect_true(any(grepl("drake_config", m)))
})

test_with_dir("drake_graph_info() print method", {
  skip_on_cran()
  skip_if_not_installed("visNetwork")
  x <- drake_graph_info(drake_plan(y = 1)) # print by hand
  m <- utils::capture.output(print(x))
  expect_true(any(grepl("drake graph", m)))
})

test_with_dir("drake_meta_() print method", {
  skip_on_cran()
  plan <- drake_plan(x = 1)
  make(plan)
  x <- diagnose(x) # print by hand
  m <- utils::capture.output(print(x))
  expect_true(any(grepl("drake metadata", m)))
})

test_with_dir("logger", {
  skip_if_not_installed("tibble")
  skip_on_cran()
  # testthat suppresses messages,
  # so we need to inspect the console output manually.
  files <- list.files()
  x <- refclass_logger$new(verbose = 0L, file = NULL)
  x$disk("abc") # Should be empty.
  x <- refclass_logger$new(verbose = 1L, file = NULL)
  x$disk("abc") # Should be empty.
  x <- refclass_logger$new(verbose = 2L, file = NULL)
  x$disk("abc") # Should be empty.
  expect_equal(files, list.files())
  x$term("abc", "def") # Should say "abc def"
  for (verbose in c(0L, 1L, 2L)) {
    tmp <- tempfile()
    x <- refclass_logger$new(verbose = verbose, file = tmp)
    expect_equal(x$file, tmp)
    expect_false(file.exists(tmp))
    x$disk("abc")
    expect_equal(length(readLines(tmp)), 1L)
    x$target("abc", "target") # Should say "target abc"
    expect_equal(length(readLines(tmp)), 2L)
  }
  # Retries need a special test since the logger is muted
  # in those tests.
  plan <- drake_plan(x = target(stop(1), retries = 3))
  expect_error(make(plan, verbose = 1L))
})

if (FALSE) {
test_with_dir("imported online file with no internet", {
  # Disconnect from the internet.
  plan <- drake_plan(
    x = file_in("https://github.com/ropensci/drake/archive/v7.3.0.tar.gz")
  )
  expect_error(make(plan), regexp = "no internet. Cannot check url")
})
}

if (FALSE) {
test_with_dir("time stamps and large files", {
  skip_on_cran()
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
  file.append(file_large, file_csv)
  plan <- drake_plan(x = file_in(!!file_large))
  cache <- storr::storr_rds(file.path(tempfile(), "cache"))
  config <- drake_config(plan, cache = cache)
  make(plan, cache = cache, log_make = log1) # should be a little slow
  expect_equal(justbuilt(config), "x")
  make(plan, cache = cache, log_make = log2) # should be quick
  expect_equal(justbuilt(config), character(0))
  tmp <- file.append(file_large, file_csv)
  make(plan, cache = cache, log_make = log3) # should be a little slow
  expect_equal(justbuilt(config), "x")
  system2("touch", file_large)
  make(plan, cache = cache, log_make = log4) # should be a little slow
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
  # New plan: dynamic files.
  plan <- drake_plan(x = target(file_large, format = "file"))
  # Same as before.
  make(plan, cache = cache, log_make = log1) # should be a little slow
  expect_equal(justbuilt(config), "x")
  make(plan, cache = cache, log_make = log2) # should be quick
  expect_equal(justbuilt(config), character(0))
  tmp <- file.append(file_large, file_csv)
  make(plan, cache = cache, log_make = log3) # should be a little slow
  expect_equal(justbuilt(config), "x")
  system2("touch", file_large)
  make(plan, cache = cache, log_make = log4) # should be a little slow
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
}

if (FALSE) {
test_with_dir("use_drake()", {
  skip_on_cran()
  # Load drake with library(drake)
  # and not with devtools::load_all().
  # Reason: https://github.com/r-lib/usethis/issues/347
  # If that problem is ever solved, we should move this test
  # to tests/testthat/test-examples.R.
  skip_if_not_installed("usethis")
  usethis::create_project(".", open = FALSE, rstudio = FALSE)
  files <- c("make", "_drake", "R/packages", "R/functions", "R/plan")
  files <- paste0(files, ".R")
  for (file in files) {
    expect_false(file.exists(file))
  }
  use_drake(open = FALSE)
  for (file in files) {
    expect_true(file.exists(file))
  }
  drake::r_make()
  model <- readd(model)
  expect_true(inherits(model, "summary.lm"))
})
}

if (FALSE) {
test_with_dir("can keep going in parallel", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  plan <- drake_plan(
    a = stop(123),
    b = a + 1
  )
  make(
    plan,
    jobs = 2,
    session_info = FALSE,
    keep_going = TRUE,
    verbose = 1
  )
  expect_error(readd(a))
  expect_error(readd(b))
})
}

if (FALSE) {
test_with_dir("drake_debug()", {
  skip_on_cran()
  load_mtcars_example()
  my_plan$command[[2]] <- quote({
    simulate(48)
    stop(1234)
  })
  expect_error(make(my_plan), regexp = "1234")
  out <- drake_debug(regression1_large, my_plan)
  out <- drake_debug(
    "regression1_large",
    plan = my_plan,
    verbose = 0L,
    character_only = TRUE
  )
  expect_true(inherits(out, "lm"))
  for (i in seq_len(2)) {
    clean(destroy = TRUE)
    load_mtcars_example()
    make(my_plan)
    config <- drake_config(my_plan)
    expect_true(config$cache$exists("regression1_small"))
    clean(regression1_small)
    expect_false(config$cache$exists("regression1_small"))
    out <- drake_debug(regression1_small, plan = my_plan)
    expect_false(config$cache$exists("regression1_small"))
    expect_true(inherits(out, "lm"))
  }
})
}

if (FALSE) {
test_with_dir("clustermq error messages get back to main", {
  skip_on_cran()
  plan <- drake_plan(a = stop(123))
  options(clustermq.scheduler = "multicore")
  for (caching in c("worker", "main")) {
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
}

if (FALSE) {
test_with_dir("clean() in interactive mode", {
  skip_on_cran()
  # Must run this test in a fresh new interactive session.
  # Cannot be fully automated like the other tests.
  .pkg_envir$drake_clean_menu <- NULL
  options(drake_clean_menu = TRUE)
  load_mtcars_example()
  config <- drake_config(my_plan)
  make(my_plan)
  expect_equal(sort(cached()), sort(my_plan$target))
  clean(garbage_collection = TRUE) # Select 2.
  expect_equal(sort(cached()), sort(my_plan$target))
  .pkg_envir$drake_clean_menu <- NULL
  clean(garbage_collection = TRUE) # Select 0.
  expect_equal(sort(cached()), sort(my_plan$target))
  .pkg_envir$drake_clean_menu <- NULL
  clean(garbage_collection = TRUE) # Select 1.
  expect_equal(sort(cached()), character(0))
  make(my_plan)
  expect_equal(sort(cached()), sort(my_plan$target))
  clean(garbage_collection = TRUE) # No menu.
  expect_equal(sort(cached()), character(0))
  make(my_plan)
  .pkg_envir$drake_clean_menu <- NULL
  options(drake_clean_menu = FALSE)
  clean(garbage_collection = TRUE) # No menu.
  expect_equal(sort(cached()), character(0))
})
}

if (FALSE) {
test_with_dir("rescue_cache() in interactive mode", {
  skip_on_cran()
  # Must run this test in a fresh new interactive session.
  # Cannot be fully automated like the other tests.
  .pkg_envir$drake_clean_menu <- NULL
  options(drake_clean_menu = TRUE)
  load_mtcars_example()
  config <- drake_config(my_plan)
  make(my_plan)
  expect_equal(sort(cached()), sort(my_plan$target))
  clean(garbage_collection = FALSE)
  rescue_cache(garbage_collection = TRUE) # Select 2
  expect_equal(sort(cached()), character(0))
  .pkg_envir$drake_clean_menu <- NULL
  clean(garbage_collection = FALSE)
  rescue_cache(garbage_collection = TRUE) # Select 1.
  expect_equal(sort(cached()), character(0))
})
}

if (FALSE) {
test_with_dir("recovery ad in clean()", {
  skip_on_cran()
  # Must run this test in a fresh new interactive session.
  # Cannot be fully automated like the other tests.
  .pkg_envir$drake_clean_menu <- NULL
  options(
    drake_clean_menu = TRUE,
    drake_clean_recovery_msg = TRUE
  )
  plan <- drake_plan(x = 1)
  make(plan)
  .pkg_envir$drake_clean_recovery_msg <- NULL
  expect_message(clean(garbage_collection = FALSE), regexp = "recover")
  expect_silent(clean(garbage_collection = FALSE))
  options(drake_clean_recovery_msg = TRUE)
  .pkg_envir$drake_clean_recovery_msg <- NULL
  expect_message(clean(garbage_collection = FALSE), regexp = "recover")
})
}

if (FALSE) {
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
}

# Needs to run outside the RStudio IDE to fork processes.
if (FALSE) {
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
      "future::plan(future::multicore, workers = 2)",
      "drake_config(my_plan, parallelism = \"future\", jobs = 2)"
    ),
    "_drake.R"
  )
  expect_true(length(r_outdated()) > 1)
  r_make()
  expect_true(is.data.frame(readd(small)))
  expect_equal(r_outdated(), character(0))
})
}

if (FALSE) {
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

if (FALSE) {
test_with_dir("progress bars", {
  skip_on_cran()
  # Needs visual inspection at every step
  plan <- drake_plan(
    x = target(
      Sys.sleep(.1),
      transform = map(y = !!seq_len(40))
    )
  )
  make(plan, verbose = 2)
  plan <- drake_plan(
    x = target(
      Sys.sleep(.1),
      transform = map(y = !!seq_len(40))
    ),
    w = seq_len(40),
    z = target(
      Sys.sleep(.1),
      dynamic = map(y = w)
    )
  )
  make(plan, verbose = 2)
  clean()
  make(plan, verbose = 2, parallelism = "future")
  skip_on_os("windows")
  clean()
  options(clustermq.scheduler = "multicore")
  make(plan, verbose = 2, parallelism = "clustermq")
})
}

if (FALSE) {
test_with_dir("dynamic branching + format file checksums (#1168)", {
  write_lines <- function(files, ...) {
    for (file in files) {
      writeLines(c(file, "stuff"), file)
    }
    files
  }
  plan <- drake_plan(
    x = c("a", "b"),
    y = target(
      write_lines(x),
      format = "file",
      dynamic = map(x)
    )
  )
  # Need to walk through this function manually.
  debug(format_file_checksum_impl.file)
  # Browse in a sub-target of y.
  # Make sure nonempty hashes are returned from the function.
  make(plan, parallelism = "future")
})
}

if (FALSE) {
test_with_dir("main caching, environment caches and parallelism", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  skip_if_not_installed("future")
  skip_on_os("windows")
  if (!grepl("loop", get_testing_scenario_name())) {
    skip("avoid conflicts with other hpc scenarios")
  }
  load_mtcars_example()
  future::plan(future::multisession, workers = 2)
  cache <- storr::storr_environment() # not thread-safe
  make(
    my_plan,
    cache = cache,
    caching = "main",
    parallelism = "future",
    jobs = 2
  )
  config <- drake_config(
    my_plan,
    cache = cache,
    caching = "main",
    parallelism = "future",
    jobs = 2
  )
  expect_true("report" %in% justbuilt(config))
})
}
