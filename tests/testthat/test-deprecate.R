drake_context("deprecation")

test_with_dir("deprecation: fetch_cache", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  dp <- drake_plan(x = 1)
  expect_warning(make(dp, fetch_cache = ""), regexp = "deprecated")
  expect_warning(drake_config(dp, fetch_cache = ""), regexp = "deprecated")
  expect_warning(get_cache(fetch_cache = ""), regexp = "deprecated")
})

test_with_dir("deprecation: deps_targets()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  config <- drake_config(drake_plan(x = 1))
  expect_warning(deps_targets("x", config), regexp = "deprecated")
})

test_with_dir("deprecation: cache functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(x = 1)
  expect_error(expect_warning(tmp <- read_drake_meta(search = FALSE)))
  expect_silent(make(plan, verbose = FALSE, session_info = FALSE))
  expect_true(is.numeric(readd(x, search = FALSE)))
  expect_equal(cached(), "x")
  cache <- get_cache()
  expect_warning(short_hash(cache))
  expect_warning(long_hash(cache))
  expect_warning(default_short_hash_algo(cache))
  expect_warning(default_long_hash_algo(cache))
  expect_warning(available_hash_algos())
  expect_warning(new_cache(short_hash_algo = "123", long_hash_algo = "456"))
})

test_with_dir("arg deprecation", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_warning(drake::drake_plan(x = y, file_targets = TRUE))
})

test_with_dir("drake version checks in previous caches", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  # We need to be able to set the drake version
  # to check back compatibility.
  plan <- drake_plan(x = 1)
  expect_silent(make(plan, verbose = FALSE))
  x <- get_cache()
  suppressWarnings(expect_error(drake_session(cache = NULL), regexp = "make"))
  expect_warning(drake_session(cache = x), regexp = "deprecated")
})

test_with_dir("deprecated graphing functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl <- drake_plan(a = 1, b = 2)
  expect_warning(build_drake_graph(pl))
  con <- drake_config(plan = pl)
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  skip_if_not_installed("ggraph")
  expect_warning(out <- static_drake_graph(config = con))
  expect_true(inherits(out, "gg"))
  df <- drake_graph_info(config = con)
  expect_warning(out <- render_static_drake_graph(df))
  expect_true(inherits(out, "gg"))
})

test_with_dir("deprecate misc utilities", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  expect_error(parallel_stages(1), regexp = "parallelism")
  expect_error(rate_limiting_times(1), regexp = "parallelism")
  expect_warning(drake_unquote("x"))
  expect_warning(drake_quotes(character(0)))
  expect_warning(drake_quotes(""))
  expect_warning(drake_quotes("x"))
  expect_warning(drake_quotes("x", single = TRUE))
  expect_warning(drake_quotes("x", single = FALSE))
  expect_warning(drake_strings("x"))
  cache <- storr::storr_environment()
  expect_warning(configure_cache(
    cache, log_progress = TRUE, init_common_values = TRUE
  ))
  expect_warning(max_useful_jobs(drake_config(drake_plan(x = 1))))
  expect_warning(deps(123))
  load_mtcars_example()
  expect_warning(config <- drake_config(my_plan, graph = 1, layout = 2))
  expect_warning(migrate_drake_project())
  expect_warning(default_verbose())
})

test_with_dir("deprecated arguments", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl <- drake_plan(a = 1, b = a)
  con <- drake_config(plan = pl)
  expect_warning(drake_build(a, config = con, meta = list()))
})

test_with_dir("example template files (deprecated)", {
  skip_on_cran()
  expect_false(file.exists("slurm_batchtools.tmpl"))
  expect_warning(
    drake_batchtools_tmpl_file("slurm_batchtools.tmpl"),
    regexp = "deprecated"
  )
  expect_true(file.exists("slurm_batchtools.tmpl"))
})

test_with_dir("force with a non-back-compatible cache", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(cache_vers_check(NULL), character(0))
  expect_null(get_cache())
  expect_null(this_cache())
  expect_true(inherits(recover_cache(), "storr"))
  write_v6.2.1_project() # nolint
  expect_warning(get_cache(), regexp = "compatible")
  expect_warning(this_cache(), regexp = "compatible")
  expect_warning(recover_cache(), regexp = "compatible")
  suppressWarnings(
    expect_error(drake_config(drake_plan(x = 1)), regexp = "compatible")
  )
  suppressWarnings(
    expect_error(make(drake_plan(x = 1)), regexp = "compatible")
  )
  expect_warning(make(drake_plan(x = 1), force = TRUE), regexp = "compatible")
  expect_silent(tmp <- get_cache())
  expect_silent(tmp <- this_cache())
  expect_silent(tmp <- recover_cache())
})

test_with_dir("deprecate the `force` argument", {
  expect_warning(tmp <- get_cache(force = TRUE), regexp = "deprecated")
  expect_warning(tmp <- this_cache(force = TRUE), regexp = "deprecated")
  expect_warning(tmp <- recover_cache(force = TRUE), regexp = "deprecated")
  expect_warning(load_mtcars_example(force = TRUE), regexp = "deprecated")
})

test_with_dir("timeout argument", {
  expect_warning(
    make(
      drake_plan(x = 1),
      timeout = 5,
      session_info = FALSE,
      cache = storr::storr_environment()
    )
  )
})

test_with_dir("old trigger interface", {
  skip_on_cran()
  for (old_trigger in suppressWarnings(triggers())) {
    plan <- drake_plan(x = 1)
    plan$trigger <- old_trigger
    clean()
    cache <- storr::storr_environment()
    expect_warning(
      make(
        plan,
        session_info = FALSE,
        cache = cache
      ),
      regexp = "old trigger interface is deprecated"
    )
    suppressWarnings(
      config <- drake_config(
        plan,
        session_info = FALSE,
        cache = cache
      ) 
    )
    trigger <- diagnose(x, cache = config$cache)$trigger
    expect_true(is.list(trigger))
    if (identical(trigger$condition, TRUE)) {
      expect_equal(old_trigger, "always")
    } else {
      expect_false(old_trigger == "always")
    }
    expect_equal(
      trigger$command,
      old_trigger %in% c("always", "any", "command")
    )
    expect_equal(
      trigger$file,
      old_trigger %in% c("always", "any", "file")
    )
    expect_equal(
      trigger$depend,
      old_trigger %in% c("always", "any", "depends")
    )
  }
})

test_with_dir("mtcars example", {
  skip_on_cran()
  expect_warning(
    load_mtcars_example(report_file = "other_name.Rmd"),
    regexp = "report_file"
  )
})

test_with_dir("deprecated hooks", {
  expect_warning(
    make(
      drake_plan(x = 1),
      hook = 123,
      session_info = FALSE,
      cache = storr::storr_environment()
    ),
    regexp = "deprecated"
  )
})

test_with_dir("pruning_strategy", {
  expect_warning(
    make(
      drake_plan(x = 1),
      pruning_strategy = 123,
      session_info = FALSE,
      cache = storr::storr_environment()
    ),
    regexp = "deprecated"
  )
})

test_with_dir("main example", {
  skip_on_cran()
  skip_if_not_installed("downloader")
  skip_if_not_installed("ggplot2")
  for (file in c("raw_data.xlsx", "report.Rmd")) {
    expect_false(file.exists(file))
  }

  # load_main_example() is now deprecated so should get a warning
  expect_warning(load_main_example())

  for (file in c("raw_data.xlsx", "report.Rmd")) {
    expect_true(file.exists(file))
  }
  expect_warning(load_main_example(overwrite = TRUE), regexp = "Overwriting")
  expect_warning(clean_main_example())
  for (file in c("raw_data.xlsx", "report.Rmd")) {
    expect_false(file.exists(file))
  }
})

test_with_dir("session arg to make()", {
  expect_warning(
    make(drake_plan(x = 1), session = "callr::r_vanilla"),
    regexp = "lock_envir"
  )
})
