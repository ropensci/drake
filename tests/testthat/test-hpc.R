drake_context("hpc")

test_with_dir("example template files", {
  expect_true(is.character(drake_hpc_template_files()))
  expect_true(length(drake_hpc_template_files()) > 0)
  expect_false(file.exists("slurm_batchtools.tmpl"))
  expect_silent(drake_hpc_template_file("slurm_batchtools.tmpl"))
  expect_true(file.exists("slurm_batchtools.tmpl"))
})

test_with_dir("safe_jobs()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_error(safe_jobs(1:3))
  expect_true(is.numeric(safe_jobs(1)))
})

test_with_dir("check_jobs()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_error(check_jobs(NULL), regexp = "length")
  expect_error(check_jobs(-1), regexp = "jobs > 0")
  expect_error(check_jobs(c(1, 1)), regexp = "of length 1")
})

test_with_dir("check_parallelism()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_error(check_parallelism(character(0), 1), regexp = "length")
  expect_error(check_parallelism(-1, 1), regexp = "character")
  expect_error(
    check_parallelism(letters[1:2], 1), regexp = "of length 1")
  expect_warning(
    make(drake_plan(x = 1), parallelism = "loop", jobs = 2),
    regexp = "should not be"
  )
})

test_with_dir("parallel imports", {
  config <- dbug()
  config$jobs_preprocess <- 2
  process_imports(config)
  process_imports_parLapply(config)
  expect_true("a" %in% cached(targets_only = FALSE))
  clean(cache = config$cache)
  process_imports_mclapply(config)
  expect_true("a" %in% cached(targets_only = FALSE))
})

test_with_dir("lightly_parallelize_atomic() is correct", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  with_seed(seed = 2017, code = {
    x <- sample(LETTERS[1:3], size = 1e3, replace = TRUE)
    append <- function(x) {
      paste0(x, "_text")
    }
    out0 <- lightly_parallelize(X = x, FUN = append, jobs = 2)
    out1 <- lightly_parallelize_atomic(X = x, FUN = append, jobs = 2)
    out2 <- lapply(X = x, FUN = append)
    expect_identical(out0, out1)
    expect_identical(out0, out2)
    y <- gsub("_text", "", unlist(out1))
    expect_identical(x, y)
  })
})

test_with_dir("checksum functionality", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- dbug()
  config$parallelism <- "loop"
  config$jobs <- 1
  config$cache <- decorate_storr(storr::storr_environment())
  testrun(config)
  checksum <- get_checksum(target = "combined", config = config)
  bad <- "askldfklhjsdfkj"
  expect_false(grepl("NA", checksum))
  expect_true(
    is_good_checksum(
      target = "combined", checksum = checksum, config = config))
  expect_false(
    is_good_checksum(
      target = "combined", checksum = bad, config = config))
  expect_silent(
    wait_checksum(
      target = "combined", checksum = checksum, config = config, timeout = 0.1))
  expect_error(
    wait_checksum(
      target = "combined", checksum = bad, config = config, timeout = 0.1))
  checksum <- get_outfile_checksum(target = "combined", config = config)
  expect_silent(
    wait_outfile_checksum(
      target = "combined", checksum = checksum, config = config, timeout = 0.1))
  expect_error(
    wait_outfile_checksum(
      target = "combined", checksum = bad, config = config, timeout = 0.1))
})

test_with_dir("direct users to GitHub issue #675", {
  skip_on_cran()
  plan <- drake_plan(
    # If base R is patched, mclapply may not always give this warning.
    x = warning("all scheduled cores encountered errors in user code")
  )
  cache <- storr::storr_environment()
  regexp <- "workaround"
  expect_warning(
    make(plan, envir = globalenv(), session_info = FALSE, cache = cache),
    regexp = regexp
  )
})

test_with_dir("drake_pmap", {
  # Basic functionality: example from purrr::pmap
  x <- list(1, 10, 100)
  y <- list(1, 2, 3)
  z <- list(5, 50, 500)
  ans <- list(x[[1]] + y[[1]] + z[[1]],
              x[[2]] + y[[2]] + z[[2]],
              x[[3]] + y[[3]] + z[[3]])
  expect_identical(ans, drake_pmap(list(x, y, z), sum))

  # Catches inputs of wrong type
  expect_error(drake_pmap("not a list", sum))
  expect_error(drake_pmap(list(), "not a function"))

  # Handles empty list
  expect_identical(list(), drake_pmap(list(), sum))

  # Passes dots to function
  x[2] <- NA
  ans[[2]] <- sum(x[[2]], y[[2]], z[[2]], na.rm = TRUE)
  expect_identical(ans, drake_pmap(list(x, y, z), sum, na.rm = TRUE))

  # Catches unequally-lengthed sublists
  x[[2]] <- NULL
  expect_error(drake_pmap(list(x, y, z), sum))
})

test_with_dir("parallelism can be a scheduler function", {
  plan <- drake_plan(x = file.create("x"))
  build_ <- function(target, config) {
    tidy_expr <- eval(
      expr = config$layout[[target]]$command_build,
      envir = config$envir_targets
    )
    eval(expr = tidy_expr, envir = config$envir_targets)
  }
  loop_ <- function(config) {
    targets <- igraph::topo_sort(config$graph)$name
    for (target in targets) {
      config$logger$minor(target)
      config$envir_targets[[target]] <- build_(
        target = target,
        config = config
      )
    }
    invisible()
  }
  config <- drake_config(plan, parallelism = loop_)
  expect_warning(
    make(config = config),
    regexp = "Use at your own risk"
  )
  expect_true(file.exists("x"))
  expect_false(config$cache$exists("x"))
})

test_with_dir("caching arg and column", {
  plan <- drake_plan(
    x = 1,
    y = target(x, caching = "master"),
    z = target(y, caching = "worker")
  )
  config <- drake_config(plan, caching = "master")
  expect_equal(hpc_caching("x", config), "master")
  expect_equal(hpc_caching("y", config), "master")
  expect_equal(hpc_caching("z", config), "worker")
  config <- drake_config(plan, caching = "worker")
  expect_equal(hpc_caching("x", config), "worker")
  expect_equal(hpc_caching("y", config), "master")
  expect_equal(hpc_caching("z", config), "worker")
  p2 <- drake_plan(x = 1, y = 2)
  config <- drake_config(p2, caching = "master")
  expect_equal(hpc_caching("x", config), "master")
  expect_equal(hpc_caching("y", config), "master")
  config <- drake_config(p2, caching = "worker")
  expect_equal(hpc_caching("x", config), "worker")
  expect_equal(hpc_caching("y", config), "worker")
})

test_with_dir("custom caching column and clustermq", {
  skip_if_not_installed("clustermq")
  skip_on_os("windows")
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
  options(clustermq.scheduler = "multicore")
  plan <- drake_plan(
    x = 1,
    y = target(x, caching = "master"),
    z = target(y, caching = "worker")
  )
  make(plan, parallelism = "clustermq", jobs = 1)
  expect_true(all(plan$target %in% cached()))
  if ("package:clustermq" %in% search()) {
    detach("package:clustermq", unload = TRUE) # nolint
  }
})

test_with_dir("custom caching column and future", {
  skip_if_not_installed("future")
  skip_on_os("windows")
  future::plan(future::multicore)
  plan <- drake_plan(
    x = 1,
    y = target(x, caching = "master"),
    z = target(y, caching = "worker")
  )
  make(plan, parallelism = "future", jobs = 1)
  expect_true(all(plan$target %in% cached()))
})
