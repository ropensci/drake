drake_context("deprecation")

test_with_dir("deprecation: target()", {
  expect_warning(target(123), regexp = "deprecated")
})

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

test_with_dir("deprecation: built", {
  plan <- drake_plan(x = 1)
  make(plan)
  config <- drake_config(plan)
  expect_warning(built(cache = NULL))
  expect_equal(
    sort(suppressWarnings(built(search = FALSE))),
    sort(display_keys(config$plan$target))
  )
  twopiece <- sort(
    c(
      suppressWarnings(built(search = FALSE)),
      suppressWarnings(imported(search = FALSE, files_only = FALSE))
    )
  )
  expect_equal(
    sort(cached(search = FALSE)),
    sort(display_keys(twopiece))
  )
  expect_equal(
    sort(suppressWarnings(built(search = TRUE))),
    sort(display_keys(c(config$plan$target)))
  )
  expect_warning(imported(search = FALSE, files_only = TRUE))
})

test_with_dir("deprecation: imported", {
  expect_identical(
    suppressWarnings(imported(cache = NULL)),
    character(0)
  )
  for (fo in c(FALSE, TRUE)) {
    imp <- suppressWarnings(imported(files_only = fo, search = FALSE))
    expect_equal(
      sort(imp),
      sort(setdiff(cached(targets_only = FALSE), cached(targets_only = TRUE))),
      info = paste("files_only =", fo)
    )
  }
})

test_with_dir("deprecation: find_project", {
  scratch <- "./scratch"
  dir.create(scratch)
  fp <- suppressWarnings(find_project(path = scratch))
  expect_null(fp)

  plan <- drake_plan(x = 1)
  make(plan)
  fp <- suppressWarnings(find_project(path = normalizePath(scratch)))
  expect_is(fp, "character")
  expect_equal(fp, getwd())
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
  skip_if_not_installed("lubridate")
  expect_warning(build_times(targets_only = TRUE), regexp = "deprecated")
  config <- drake_config(plan)
  expect_warning(
    predict_runtime(config, targets_only = TRUE),
    regexp = "deprecated"
  )
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
  expect_warning(make_imports(config))
  expect_warning(make_targets(config))
  expect_warning(make_with_config(config))
  expect_warning(migrate_drake_project())
  expect_warning(default_verbose())
  expect_warning(default_Makefile_args(2, 2))
  expect_warning(default_Makefile_args(0, 0))
  expect_warning(default_Makefile_command())
  expect_warning(Makefile_recipe())
  expect_warning(default_recipe_command())
  expect_warning(r_recipe_wildcard())
  expect_warning(parallelism_choices(TRUE))
  expect_warning(parallelism_choices(FALSE))
  expect_warning(shell_file())
  expect_warning(default_parallelism())
  expect_warning(read_drake_config())
  expect_warning(read_drake_graph())
  expect_warning(read_drake_plan())
  expect_warning(prune_drake_graph(config$graph, "small"))
})

test_with_dir("deprecated arguments", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl <- drake_plan(a = 1, b = a)
  con <- drake_config(plan = pl)
  expect_warning(drake::drake_plan(x = y, file_targets = TRUE))
  expect_warning(drake_plan(list = c(a = "1")), regexp = "deprecated")
  expect_warning(drake_build(a, config = con, meta = list()))
  expect_warning(make(drake_plan(x = 1), recipe_command = "123"))
  expect_warning(make(drake_plan(x = 1), hasty_build = "123"))
  expect_warning(loadd(x, graph = 123))
  expect_warning(drake_build("a", config = con, envir = 123))
  expect_warning(failed(upstream_only = TRUE))
  expect_error(expect_warning(loadd(list = "a", deps = TRUE)))
  expect_warning(loadd(imported_only = TRUE), regexp = "deprecated")
  expect_warning(cached(list = "x"), regexp = "deprecated")
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
  expect_true(inherits(suppressWarnings(recover_cache()), "storr"))
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
  skip_if_not_installed("curl")
  skip_if_not_installed("downloader")
  if (!curl::has_internet()) {
    skip("no internet connection")
  }
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

test_with_dir("deprecated check_plan()", {
  # Circular non-DAG plan
  x <- drake_plan(a = b, b = c, c = a)
  expect_error(tmp <- capture.output(suppressWarning(check_plan(x))))
})

test_with_dir("deprecated cache_ and target_namespaces() etc.", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- suppressWarnings(cache_namespaces())
  y <- suppressWarnings(target_namespaces())
  expect_true(all(y %in% x))
  expect_false(all(x %in% y))
  expect_warning(cleaned_namespaces(), regexp = "deprecated")
})

test_with_dir("deprecated drake_tip()", {
  expect_true(is.character(suppressWarnings(drake_tip())))
})

test_with_dir("former external functions that will become internal", {
  plan <- drake_plan(x = 1)
  make(plan)
  config <- drake_config(plan)
  expect_warning(analysis_wildcard(), regexp = "deprecated")
  expect_warning(cache_namespaces(), regexp = "deprecated")
  expect_warning(cache_path(), regexp = "deprecated")
  expect_warning(check_plan(plan = plan), regexp = "deprecated")
  expect_warning(dataset_wildcard(), regexp = "deprecated")
  expect_warning(drake_meta("x", config), regexp = "deprecated")
  expect_warning(drake_palette(), regexp = "deprecated")
  expect_warning(in_progress(), regexp = "deprecated")
  expect_warning(progress(no_imported_objects = TRUE), regexp = "deprecated")
  expect_warning(recover_cache(), regexp = "deprecated")
  expect_warning(target_namespaces(), regexp = "deprecated")
})

test_with_dir("analyses and summaries", {
  datasets <- drake_plan(small = simulate(5), large = simulate(50))
  methods <- drake_plan(
    regression1 = reg1(dataset__),
    regression2 = reg2(dataset__)
  )
  analyses <- expect_warning(
    plan_analyses(methods, datasets = datasets, sep = ".")
  )
  x <- weak_tibble(
    target = c(
      "regression1.small",
      "regression1.large",
      "regression2.small",
      "regression2.large"
    ),
    command = c(
      "reg1(small)",
      "reg1(large)",
      "reg2(small)",
      "reg2(large)")
  )
  equivalent_plans(x, analyses)
  analyses <- expect_warning(
    plan_analyses(methods, datasets = datasets)
  )
  x <- weak_tibble(
    target = c(
      "regression1_small",
      "regression1_large",
      "regression2_small",
      "regression2_large"
    ),
    command = c(
      "reg1(small)",
      "reg1(large)",
      "reg2(small)",
      "reg2(large)")
  )
  equivalent_plans(analyses, x)
  m2 <- drake_plan(regression1 = reg1(n), regression2 = reg2(n))
  out <- expect_warning(plan_analyses(m2, datasets = datasets))
  equivalent_plans(out, m2)
  no_analyses <- drake_plan(
    summ = summary(dataset__),
    coef = stats::coefficients(dataset__)
  )
  suppressWarnings(
    expect_error(
      plan_summaries(no_analyses, analyses, datasets)
    )
  )
  summary_types <- drake_plan(
    summ = summary(analysis__),
    coef = stats::coefficients(analysis__)
  )
  results <- expect_warning(
    plan_summaries(
      summary_types,
      analyses,
      datasets,
      gather = NULL,
      sep = "."
    )
  )
  x <- weak_tibble(
    target = c(
      "summ.regression1_small",
      "summ.regression1_large",
      "summ.regression2_small",
      "summ.regression2_large",
      "coef.regression1_small",
      "coef.regression1_large",
      "coef.regression2_small",
      "coef.regression2_large"
    ),
    command = c(
      "summary(regression1_small)",
      "summary(regression1_large)",
      "summary(regression2_small)",
      "summary(regression2_large)",
      "stats::coefficients(regression1_small)",
      "stats::coefficients(regression1_large)",
      "stats::coefficients(regression2_small)",
      "stats::coefficients(regression2_large)"
    )
  )
  equivalent_plans(results, x)
  results <- expect_warning(
    plan_summaries(summary_types, analyses, datasets, gather = NULL)
  )
  x <- weak_tibble(
    target = c(
      "summ_regression1_small",
      "summ_regression1_large",
      "summ_regression2_small",
      "summ_regression2_large",
      "coef_regression1_small",
      "coef_regression1_large",
      "coef_regression2_small",
      "coef_regression2_large"
    ),
    command = c(
      "summary(regression1_small)",
      "summary(regression1_large)",
      "summary(regression2_small)",
      "summary(regression2_large)",
      "stats::coefficients(regression1_small)",
      "stats::coefficients(regression1_large)",
      "stats::coefficients(regression2_small)",
      "stats::coefficients(regression2_large)"
    )
  )
  equivalent_plans(results, x)
  summary_types <- drake_plan(
    summ = summary(analysis__, dataset__),
    coef = stats::coefficients(analysis__)
  )
  results <- expect_warning(
    plan_summaries(
      summary_types,
      analyses,
      datasets,
      gather = c("list", "rbind")
    )
  )
  x <- weak_tibble(
    target = c(
      "summ_regression1_small",
      "summ_regression1_large",
      "summ_regression2_small",
      "summ_regression2_large",
      "coef_regression1_small",
      "coef_regression1_large",
      "coef_regression2_small",
      "coef_regression2_large"
    ),
    command = c(
      "summary(regression1_small, small)",
      "summary(regression1_large, large)",
      "summary(regression2_small, small)",
      "summary(regression2_large, large)",
      "stats::coefficients(regression1_small)",
      "stats::coefficients(regression1_large)",
      "stats::coefficients(regression2_small)",
      "stats::coefficients(regression2_large)"
    )
  )
  y <- results[-1:-2, ]
  row.names(x) <- row.names(y) <- NULL
  equivalent_plans(x, y)
  expect_true(grepl("^rbind\\(coef", results$command[1]))
  expect_true(grepl("^list\\(summ", results$command[2]))
  results <- expect_warning(
    plan_summaries(summary_types, analyses, datasets)
  )
  expect_true(all(grepl("^list\\(", results$command[1:2])))
  results <- expect_warning(
    plan_summaries(
      summary_types, analyses, datasets, gather = "my_bind"
    )
  )
  expect_true(all(grepl("^my_bind\\(", results$command[1:2])))
  expect_error(
    suppressWarnings(
      nope <- plan_summaries(
        summary_types,
        analyses,
        datasets,
        gather = rep("list", 37)
      )
    )
  )
  newtypes <- rbind(
    summary_types,
    drake_plan(
      other = myother(dataset__)
    )
  )
  expect_warning(
    s <- plan_summaries(
      newtypes,
      analyses,
      datasets,
      gather = NULL
    )
  )
  expect_equal(nrow(s), 8)
})
