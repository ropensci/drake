drake_context("deprecate")

test_with_dir("defunct functions", {
  skip_on_cran()
  suppressWarnings({
    expect_condition(
      drake_defunct(),
      regexp = "defunct"
    )
    expect_error(
      drake_defunct(),
      regexp = "drake_defunct"
    )
    expect_error(analyses(), regexp = "analyses")
    expect_error(drake::analyses(), regexp = "drake::analyses")
    expect_error(from_plan(), regexp = "defunct")
  })
})

test_with_dir("deprecation: target()", {
  skip_on_cran()
  expect_error(target(123), regexp = "user-side")
})

test_with_dir("deprecation: fetch_cache", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  dp <- drake_plan(x = 1)
  expect_warning(make(dp, fetch_cache = ""), regexp = "deprecated")
  expect_warning(drake_config(dp, fetch_cache = ""), regexp = "deprecated")
})

test_with_dir("deprecation: deps_targets() and knitr_deps()", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  config <- drake_config(
    drake_plan(x = 1),
    cache = storr::storr_environment(),
    session_info = FALSE
  )
  expect_warning(deps_targets("x", config), regexp = "deprecated")
  make_impl(config = config)
  expect_warning(
    dependency_profile(x, config, character_only = FALSE),
    regexp = "deprecated"
  )
  expect_warning(
    dependency_profile("x", config, character_only = TRUE),
    regexp = "deprecated"
  )
  skip_if_not_installed("knitr")
  load_mtcars_example()
  expect_warning(knitr_deps("report.Rmd"), regexp = "deprecated")
})

test_with_dir("deprecation: cache functions", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  plan <- drake_plan(x = 1)
  expect_error(expect_warning(tmp <- read_drake_meta()))
  expect_silent(make(plan, verbose = 0L, session_info = FALSE))
  expect_true(is.numeric(readd(x)))
  expect_equal(cached(), "x")
  cache <- drake_cache()
  expect_warning(short_hash(cache))
  expect_warning(long_hash(cache))
  expect_warning(default_short_hash_algo(cache))
  expect_warning(default_long_hash_algo(cache))
  expect_warning(available_hash_algos())
  expect_warning(new_cache(short_hash_algo = "123", long_hash_algo = "456"))
})

test_with_dir("deprecation: built", {
  skip_on_cran()
  plan <- drake_plan(x = 1)
  make(plan)
  config <- drake_config(plan)
  expect_warning(built(cache = NULL))
  expect_equal(
    sort(suppressWarnings(built())),
    sort(redisplay_keys(plan$target))
  )
  twopiece <- sort(
    c(
      suppressWarnings(built()),
      suppressWarnings(imported(files_only = FALSE))
    )
  )
  expect_equal(
    sort(cached()),
    sort(redisplay_keys(twopiece))
  )
  expect_warning(imported(files_only = TRUE))
})

test_with_dir("deprecation: imported", {
  skip_on_cran()
  expect_identical(
    suppressWarnings(imported(cache = NULL)),
    character(0)
  )
  for (fo in c(FALSE, TRUE)) {
    imp <- suppressWarnings(imported(files_only = fo))
    expect_equal(
      sort(imp),
      sort(setdiff(cached(targets_only = FALSE), cached(targets_only = TRUE))),
      info = paste("files_only =", fo)
    )
  }
})

test_with_dir("deprecation: find_project", {
  skip_on_cran()
  scratch <- "./scratch"
  dir.create(scratch)
  fp <- suppressWarnings(find_project(path = scratch))
  expect_null(fp)

  plan <- drake_plan(x = 1)
  make(plan)
  fp <- suppressWarnings(find_project(path = normalizePath(scratch)))
  expect_is(fp, "character")
})

test_with_dir("drake version checks in previous caches", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  # We need to be able to set the drake version
  # to check back compatibility.
  plan <- drake_plan(x = 1)
  expect_silent(make(plan, verbose = 0L, session_info = TRUE))
  x <- drake_cache()
  suppressWarnings(expect_error(drake_session(cache = NULL), regexp = "make"))
  expect_warning(drake_session(cache = x), regexp = "deprecated")
  skip_if_not_installed("lubridate")
  expect_warning(build_times(targets_only = TRUE), regexp = "deprecated")
  config <- drake_config(plan)
  expect_warning(
    predict_runtime_impl(config, targets_only = TRUE),
    regexp = "deprecated"
  )
})

test_with_dir("deprecated graphing functions", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  pl <- drake_plan(a = 1, b = 2)
  expect_warning(build_drake_graph(pl))
  con <- drake_config(plan = pl)
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
  skip_if_not_installed("ggraph")
  expect_warning(out <- vis_drake_graph_impl(config = con, direction = "LR"))
  expect_warning(out <- vis_drake_graph_impl(config = con, layout = "sugiyama"))
  expect_warning(out <- static_drake_graph(config = con))
  expect_true(inherits(out, "gg"))
  df <- drake_graph_info_impl(config = con)
  expect_warning(out <- render_static_drake_graph(df))
  expect_true(inherits(out, "gg"))
  expect_warning(find_cache(directory = "x"), regexp = "deprecated")
})

test_with_dir("deprecate misc utilities", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("lubridate")
  skip_if_not_installed("visNetwork")
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
  load_mtcars_example()
  expect_warning(config <- drake_config(my_plan, graph = 1, spec = 2))
  expect_warning(make_imports(config))
  expect_warning(make_targets(config))
  expect_warning(make_with_config(config))
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
  expect_warning(predict_load_balancing(config), regexp = "deprecated")
  expect_warning(tmp <- this_cache(), regexp = "deprecated")
  expect_warning(drake_cache_log_file(), regexp = "deprecated")
  expect_warning(get_trace("a", "b"), regexp = "deprecated")
})

test_with_dir("deprecated arguments", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_warning(
    pl <- drake_plan(a = 1, b = a, tidy_evaluation = TRUE),
    regexp = "deprecated"
  )
  con <- drake_config(plan = pl)
  expect_warning(drake::drake_plan(x = y, file_targets = TRUE))
  expect_warning(drake_plan(list = c(a = "1")), regexp = "deprecated")
  expect_warning(make(drake_plan(x = 1), recipe_command = "123"))
  expect_warning(make(drake_plan(x = 1), hasty_build = "123"))
  expect_warning(loadd(x, graph = 123))
  expect_warning(failed(upstream_only = TRUE))
  expect_error(expect_warning(loadd(list = "a", deps = TRUE)))
  expect_warning(loadd(imported_only = TRUE), regexp = "deprecated")
  expect_warning(cached(list = "x"), regexp = "deprecated")
  expect_warning(
    drake_config(plan = pl, ensure_workers = TRUE),
    regexp = "deprecated"
  )
  expect_warning(cached(verbose = 1L), regexp = "deprecated")
  expect_warning(
    drake_cache(console_log_file = "123"),
    regexp = "deprecated"
  )
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
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(cache_vers_check(NULL), character(0))
  expect_null(drake_cache())
  expect_true(
    inherits(
      suppressWarnings(recover_cache()),
      "refclass_decorated_storr"
    )
  )
  zip <- system.file(
    file.path("testing", "built_mtcars_example_v6.2.1.zip"),
    package = "drake",
    mustWork = TRUE
  )
  unzip(zip, exdir = ".", setTimes = TRUE)
  expect_warning(drake_cache(), regexp = "compatible")
  expect_warning(recover_cache(), regexp = "compatible")
  suppressWarnings(
    expect_error(drake_config(drake_plan(x = 1)), regexp = "compatible")
  )
  suppressWarnings(
    expect_error(make(drake_plan(x = 1)), regexp = "compatible")
  )
  expect_warning(make(drake_plan(x = 1), force = TRUE), regexp = "compatible")
  tmp <- drake_cache()
})

test_with_dir("deprecate the `force` argument", {
  skip_on_cran()
  expect_warning(tmp <- recover_cache(force = TRUE), regexp = "deprecated")
  expect_warning(load_mtcars_example(force = TRUE), regexp = "deprecated")
})

test_with_dir("timeout argument", {
  skip_on_cran()
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
      regexp = "old trigger interface in drake is deprecated"
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
  skip_on_cran()
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
  skip_on_cran()
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
  skip_if_not_installed("tibble")
  skip_if_not_installed("tidyr")
  if (!curl::has_internet()) {
    skip("no internet connection")
  }
  skip_if_offline()
  skip_if_not_installed("curl")
  url <- "https://wlandau.github.io/drake-examples/main.zip" # nolint
  tryCatch(
    mem <- curl::curl_fetch_memory(url),
    error = function(e) {
      skip("test URL unreachable")
    }
  )
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
  skip_on_cran()
  expect_warning(
    make(drake_plan(x = 1), session = "callr::r_vanilla"),
    regexp = "deprecated"
  )
})

test_with_dir("deprecated check_plan()", {
  skip_on_cran()
  # Circular non-DAG plan
  x <- drake_plan(a = b, b = c, c = a)
  expect_error(tmp <- capture.output(suppressWarning(check_plan(x))))
})

test_with_dir("deprecated cache_ and target_namespaces() etc.", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  x <- suppressWarnings(cache_namespaces())
  y <- suppressWarnings(target_namespaces())
  expect_true(all(y %in% x))
  expect_false(all(x %in% y))
  expect_warning(cleaned_namespaces(), regexp = "deprecated")
})

test_with_dir("deprecated drake_tip()", {
  skip_on_cran()
  expect_true(is.character(suppressWarnings(drake_tip())))
})

test_with_dir("former external functions that will become internal", {
  skip_on_cran()
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
  skip_on_cran()
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

test_with_dir("wildcards", {
  skip_on_cran()
  plan <- drake_plan(x = 1)
  expect_warning(map_plan(data.frame(x = 1), f), regexp = "deprecated")
  expect_warning(gather_plan(plan), regexp = "deprecated")
  expect_warning(gather_by(plan), regexp = "deprecated")
  expect_warning(reduce_plan(plan), regexp = "deprecated")
  expect_warning(reduce_by(plan), regexp = "deprecated")
  expect_warning(evaluate_plan(plan), regexp = "deprecated")
  expect_warning(expand_plan(plan), regexp = "deprecated")
})

test_with_dir("empty generative args", suppressWarnings({
  skip_on_cran()
  x <- drake_plan(a = 1, b = FUNCTION())
  equivalent_plans(evaluate_plan(x), x)
  equivalent_plans(evaluate_wildcard_rules(x, rules = NULL), x)
  equivalent_plans(expand_plan(x), x)
}))

test_with_dir("evaluate and expand", suppressWarnings({
  skip_on_cran()
  df <- drake_plan(data = simulate(center = MU, scale = SIGMA))
  m0 <- evaluate_plan(df, wildcard = "NULL", values = 1:2)
  equivalent_plans(m0, df)
  m1 <- evaluate_plan(df, rules = list(nothing = 1:2), expand = FALSE)
  equivalent_plans(m1, df)

  x <- expand_plan(df, values = c("rep1", "rep2"), sep = ".")
  y <- weak_tibble(
    target = c("data.rep1", "data.rep2"),
    command = rep("simulate(center = MU, scale = SIGMA)", 2)
  )
  equivalent_plans(x, y)

  x <- expand_plan(df, values = c("rep1", "rep2"))
  y <- weak_tibble(
    target = c("data_rep1", "data_rep2"),
    command = rep("simulate(center = MU, scale = SIGMA)", 2)
  )
  equivalent_plans(x, y)

  x1 <- expand_plan(df, values = c("rep1", "rep2"), rename = TRUE)
  x2 <- expand_plan(df, values = c("rep1", "rep2"), rename = FALSE)
  y2 <- weak_tibble(
    target = c("data", "data"),
    command = rep("simulate(center = MU, scale = SIGMA)", 2)
  )
  equivalent_plans(x1, y)
  equivalent_plans(x2, y2)

  x2 <- evaluate_plan(x, wildcard = "MU", values = 1:2, sep = ".")
  y <- weak_tibble(
    target = c("data_rep1.1", "data_rep1.2", "data_rep2.1", "data_rep2.2"),
    command = c(
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)",
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)"
    )
  )
  equivalent_plans(x2, y)

  x2 <- evaluate_plan(x, wildcard = "MU", values = 1:2)
  y <- weak_tibble(
    target = c("data_rep1_1", "data_rep1_2", "data_rep2_1", "data_rep2_2"),
    command = c(
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)",
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)"
    )
  )
  equivalent_plans(x2, y)

  x3 <- evaluate_plan(x2, wildcard = "SIGMA", values = letters[1:2],
                      expand = FALSE)
  y <- weak_tibble(
    target = c("data_rep1_1", "data_rep1_2", "data_rep2_1", "data_rep2_2"),
    command = c(
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)",
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)"
    )
  )
  equivalent_plans(x3, y)

  x3a <- evaluate_plan(x2, wildcard = "SIGMA", values = letters[1:2],
                       expand = FALSE, rename = TRUE)
  y <- weak_tibble(
    target = c(
      "data_rep1_1_a", "data_rep1_2_b", "data_rep2_1_a", "data_rep2_2_b"),
    command = c(
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)",
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)"
    )
  )
  equivalent_plans(x3a, y)

  x3b <- evaluate_plan(
    x2,
    wildcard = "SIGMA",
    values = letters[1:2],
    expand = FALSE,
    rename = TRUE,
    sep = "."
  )
  y <- weak_tibble(
    target = c(
      "data_rep1_1.a", "data_rep1_2.b", "data_rep2_1.a", "data_rep2_2.b"),
    command = c(
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)",
      "simulate(center = 1, scale = a)",
      "simulate(center = 2, scale = b)"
    )
  )
  equivalent_plans(x3b, y)

  x4 <- evaluate_plan(x, rules = list(MU = 1:2, SIGMA = c(0.1, 1)),
                      expand = FALSE)
  y <- weak_tibble(
    target = c("data_rep1", "data_rep2"),
    command = c(
      "simulate(center = 1, scale = 0.1)",
      "simulate(center = 2, scale = 1)"
    )
  )
  equivalent_plans(x4, y)

  x5 <- evaluate_plan(x, rules = list(MU = 1:2, SIGMA = c(0.1, 1, 10)))
  expect_equal(12L, nrow(x5))
  expect_equal(12L, length(unique(x5$target)))
  expect_equal(6L, length(unique(x5$command)))

  x6 <- evaluate_plan(df, rules = list(MU = 0:1, SIGMA = 1:2), sep = ".")
  y <- weak_tibble(
    target = c("data.0.1", "data.0.2", "data.1.1", "data.1.2"),
    command = c(
      "simulate(center = 0, scale = 1)",
      "simulate(center = 0, scale = 2)",
      "simulate(center = 1, scale = 1)",
      "simulate(center = 1, scale = 2)"
    )
  )
  equivalent_plans(x6, y)
}))

test_with_dir("evaluate_plan() and trace", suppressWarnings({
  skip_on_cran()
  plan <- drake_plan(
    top = 3,
    data = simulate(center = MU, scale = SIGMA),
    mus = c(MU, x),
    simple = 1,
    sigmas = c(SIGMA, y),
    cheap = 2
  )

  x <- evaluate_plan(
    plan, trace = TRUE, wildcard = "MU", values = 1:2, expand = FALSE)
  y <- weak_tibble(
    target = c(
      "top",
      "data",
      "mus",
      "simple",
      "sigmas",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = 1, scale = SIGMA)",
      "c(2, x)",
      1,
      "c(SIGMA, y)",
      2
    ),
    MU = as.character(c(NA, 1, 2, NA, NA, NA)),
    MU_from = as.character(c(NA, "data", "mus", NA, NA, NA))
  )
  equivalent_plans(x, y)

  x <- evaluate_plan(
    plan, trace = TRUE, wildcard = "SIGMA", values = 1:2, expand = FALSE)
  y <- weak_tibble(
    target = c(
      "top",
      "data",
      "mus",
      "simple",
      "sigmas",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = MU, scale = 1)",
      "c(MU, x)",
      1,
      "c(2, y)",
      2
    ),
    SIGMA = as.character(c(NA, 1, NA, NA, 2, NA)),
    SIGMA_from = as.character(c(NA, "data", NA, NA, "sigmas", NA))
  )
  equivalent_plans(x, y)

  x <- evaluate_plan(plan, trace = TRUE, wildcard = "MU", values = 1:2)
  y <- weak_tibble(
    target = c(
      "top",
      "data_1",
      "data_2",
      "mus_1",
      "mus_2",
      "simple",
      "sigmas",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = 1, scale = SIGMA)",
      "simulate(center = 2, scale = SIGMA)",
      "c(1, x)",
      "c(2, x)",
      1,
      "c(SIGMA, y)",
      2
    ),
    MU = as.character(c(NA, 1, 2, 1, 2, NA, NA, NA)),
    MU_from = as.character(c(NA, "data", "data", "mus", "mus", NA, NA, NA))
  )
  equivalent_plans(x, y)

  x <- evaluate_plan(
    plan, trace = TRUE, rules = list(MU = 1:2, SIGMA = 3:4), expand = FALSE)
  y <- weak_tibble(
    target = c(
      "top",
      "data",
      "mus",
      "simple",
      "sigmas",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = 1, scale = 3)",
      "c(2, x)",
      1,
      "c(4, y)",
      2
    ),
    MU = as.character(c(NA, 1, 2, NA, NA, NA)),
    MU_from = as.character(c(NA, "data", "mus", NA, NA, NA)),
    SIGMA = as.character(c(NA, 3, NA, NA, 4, NA)),
    SIGMA_from = as.character(c(NA, "data", NA, NA, "sigmas", NA))
  )
  equivalent_plans(x, y)

  x <- evaluate_plan(plan, trace = TRUE, rules = list(MU = 1:2, SIGMA = 3:4))
  y <- weak_tibble(
    target = c(
      "top",
      "data_1_3",
      "data_1_4",
      "data_2_3",
      "data_2_4",
      "mus_1",
      "mus_2",
      "simple",
      "sigmas_3",
      "sigmas_4",
      "cheap"
    ),
    command = c(
      3,
      "simulate(center = 1, scale = 3)",
      "simulate(center = 1, scale = 4)",
      "simulate(center = 2, scale = 3)",
      "simulate(center = 2, scale = 4)",
      "c(1, x)",
      "c(2, x)",
      1,
      "c(3, y)",
      "c(4, y)",
      2
    ),
    MU = as.character(c(NA, 1, 1, 2, 2, 1, 2, NA, NA, NA, NA)),
    MU_from = as.character(
      c(NA, rep("data", 4), rep("mus", 2), NA, NA, NA, NA)
    ),
    SIGMA = as.character(c(NA, 3, 4, 3, 4, NA, NA, NA, 3, 4, NA)),
    SIGMA_from = as.character(
      c(
        NA, rep(c("data_1", "data_2"), each = 2),
        NA, NA, NA, rep("sigmas", 2), NA
      )
    )
  )
  equivalent_plans(x, y)
}))

test_with_dir("make() with wildcard columns", suppressWarnings({
  skip_on_cran()
  plan <- evaluate_plan(
    drake_plan(x = rnorm(n__)),
    wildcard = "n__",
    values = 1:2,
    trace = TRUE
  )
  expect_equal(nrow(plan), 2)
  for (col in c("n__", "n___from")) {
    expect_true(col %in% colnames(plan))
  }
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  con <- drake_config(plan, cache = cache, session_info = FALSE)
  expect_true(all(plan$target %in% cached(cache = con$cache)))
}))

test_with_dir("unconventional wildcards", suppressWarnings({
  skip_on_cran()
  df <- drake_plan(data = simulate(center = .MU., scale = `{SIGMA}`)) # nolint
  x0 <- expand_plan(df, values = c("rep1", "rep2"))
  x <- evaluate_plan(
    x0, rules = list(.MU. = 1:2, "`{SIGMA}`" = c(0.1, 1)), expand = FALSE # nolint
  )
  y <- weak_tibble(
    target = c("data_rep1", "data_rep2"),
    command = c(
      "simulate(center = 1, scale = 0.1)",
      "simulate(center = 2, scale = 1)"
    )
  )
  equivalent_plans(x, y)
}))

test_with_dir("'columns' argument to evaluate_plan()", suppressWarnings({
  skip_on_cran()
  plan <- drake_plan(
    x = target(always, cpu = "any"),
    y = target(any, cpu = "always"),
    z = target(any, cpu = "any")
  )
  out <- weak_tibble(
    target = c("x_1", "x_2", "y_1", "y_2", "z"),
    command = c(1, 2, rep("any", 3)),
    cpu = c("any", "any", 1, 2, "any")
  )
  equivalent_plans(
    evaluate_plan(
      plan, wildcard = "always", values = 1:2, columns = c("command", "cpu")
    ),
    out
  )
  out <- weak_tibble(
    target = c("x", "y_1", "y_2", "z"),
    command = c("always", rep("any", 3)),
    cpu = c("any", 1, 2, "any")
  )
  equivalent_plans(
    evaluate_plan(
      plan, wildcard = "always", values = 1:2, columns = "cpu"
    ),
    out
  )
  out <- weak_tibble(
    target = c("x", "y", "z"),
    command = c(1, rep("any", 2)),
    cpu = c("any", 2, "any")
  )
  equivalent_plans(
    evaluate_plan(
      plan, wildcard = "always", values = 1:2, columns = c("command", "cpu"),
      expand = FALSE
    ),
    out
  )
  rules <- list(always = 1:2, any = 3:4)
  out <- weak_tibble(
    target = c(
      "x_1_3", "x_1_4", "x_2_3", "x_2_4", "y_1_3",
      "y_1_4", "y_2_3", "y_2_4", "z_3", "z_4"
    ),
    command = as.character(c(1, 1, 2, 2, 3, 4, 3, 4, 3, 4)),
    cpu = as.character(c(3, 4, 3, 4, 1, 1, 2, 2, 3, 4))
  )
  equivalent_plans(
    evaluate_plan(plan, rules = rules, columns = c("command", "cpu")),
    out
  )
}))

test_with_dir("issue 187 on Github (from Kendon Bell)", suppressWarnings({
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  test <- drake_plan(test = run_it(wc__))
  out <- evaluate_plan(test, rules = list(wc__ = list(1:4, 5:8, 9:12)))
  out2 <- weak_tibble(
    target = c("test_1.4", "test_5.8", "test_9.12"),
    command = c("run_it(1:4)", "run_it(5:8)", "run_it(9:12)")
  )
  equivalent_plans(out, out2)
}))

test_with_dir("conflicts in wildcard names/values", suppressWarnings({
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  plan <- drake_plan(a = 1, b = 2)
  rules1 <- list(plant = 1:2, seed = 3:4, plantseed = 5:6)
  rules2 <- list(
    plant = c("grow", "tall"),
    bean = c("legume", "stalk"),
    example = c("bean", "stalk")
  )
  expect_error(
    evaluate_plan(plan, rules = rules1), regexp = "wildcard name")
  expect_error(
    evaluate_plan(plan, rules = rules2), regexp = "replacement value")
}))

test_with_dir("bad 'columns' argument to evaluate_plan()", suppressWarnings({
  skip_on_cran()
  plan <- drake_plan(
    x = target("always", cpu = "any"),
    y = target("any", cpu = "always"),
    z = target("any", cpu = "any")
  )
  expect_error(
    evaluate_plan(plan, wildcard = "any", values = 1:2, columns = "target"),
    regexp = "argument of evaluate_plan"
  )
  expect_error(
    evaluate_plan(plan, wildcard = "any", values = 1:2, columns = "nobodyhere"),
    regexp = "not in the plan"
  )
  equivalent_plans(
    plan,
    evaluate_plan(plan, wildcard = "any", values = 1:2, columns = NULL)
  )
}))

test_with_dir("map_plan()", suppressWarnings({
  skip_on_cran()
  fn <- function(a, b) {
    a + b
  }
  args <- expand.grid(a = 1:2, b = 3:5)
  plan1 <- map_plan(args = args, fun = fn)
  args$id <- LETTERS[seq_len(nrow(args))]
  plan2 <- map_plan(args = args, fun = fn)
  args$x <- args$id
  args$id <- NULL
  plan3 <- map_plan(args = args, fun = fn, id = x)
  plan4 <- map_plan(args = args, fun = "fn", id = "x", character_only = TRUE)
  plan5 <- map_plan(args = args, fun = fn, id = x, trace = TRUE)
  expect_equal(deparse_lang_col(plan1$command), deparse_lang_col(plan2$command))
  equivalent_plans(plan2, plan3)
  equivalent_plans(plan3, plan4)
  equivalent_plans(weak_as_tibble(cbind(plan3, args)), plan5)
  cache <- storr::storr_environment()
  make(plan2, session_info = FALSE, cache = cache)
  expect_equal(
    vapply(
      args$x, readd, FUN.VALUE = integer(1), USE.NAMES = FALSE,
      cache = cache, character_only = TRUE
    ),
    as.integer(args$a + args$b)
  )
}))

test_with_dir("map_plan() onto a matrix", suppressWarnings({
  skip_on_cran()
  skip_if_not_installed("datasets")
  my_model_fit <- function(x1, x2) {
    lm(as.formula(paste("mpg ~", x1, "+", x2)), data = datasets::mtcars)
  }
  covariates <- setdiff(colnames(datasets::mtcars), "mpg")
  args <- t(combn(covariates, 2))
  colnames(args) <- c("x1", "x2")
  plan <- map_plan(args, "my_model_fit")
  cache <- storr::storr_environment()
  make(plan, cache = cache, session_info = FALSE)
  x <- readd(plan$target[1], character_only = TRUE, cache = cache)
  expect_true(is.numeric(stats::coefficients(x)))
}))

test_with_dir("map_plan() with symbols", suppressWarnings({
  skip_on_cran()
  skip_if_not_installed("datasets")
  my_model_fit <- function(x1, x2, data) {
    formula <- as.formula(paste("mpg ~", x1, "+", x1))
    lm(formula, data = data)
  }
  covariates <- setdiff(colnames(datasets::mtcars), "mpg")
  args <- t(combn(covariates, 2))
  colnames(args) <- c("x1", "x2")
  args <- weak_as_tibble(args)
  args$data <- rlang::syms(rep("mtcars", nrow(args)))
  plan <- map_plan(args, my_model_fit)
  cache <- storr::storr_environment()
  make(plan, verbose = 0L, cache = cache)
  x <- readd(plan$target[1], character_only = TRUE, cache = cache)
  expect_true(is.numeric(stats::coefficients(x)))
}))

test_with_dir("gather_plan()", suppressWarnings({
  skip_on_cran()
  df <- drake_plan(data = simulate(center = MU, scale = SIGMA))
  m0 <- evaluate_plan(df, wildcard = "NULL", values = 1:2)
  equivalent_plans(m0, df)
  m1 <- evaluate_plan(df, rules = list(nothing = 1:2), expand = FALSE)
  equivalent_plans(m1, df)
  x <- expand_plan(df, values = c("rep1", "rep2"))
  x6 <- gather_plan(x, append = FALSE)
  y <- sanitize_plan(
    weak_tibble(
      target = "target",
      command = "list(data_rep1 = data_rep1, data_rep2 = data_rep2)"
    )
  )
  equivalent_plans(x6, y)
  z <- gather_plan(x, append = TRUE)
  equivalent_plans(z, bind_plans(x, y))
  x7 <- gather_plan(
    x, target = "my_summaries", gather = "rbind", append = FALSE
  )
  y <- weak_tibble(
    target = "my_summaries",
    command = "rbind(data_rep1 = data_rep1, data_rep2 = data_rep2)"
  )
  equivalent_plans(x7, y)
}))

test_with_dir("reduce_plan()", suppressWarnings({
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  # Non-pairwise reduce
  x_plan <- evaluate_plan(
    drake_plan(x = VALUE),
    wildcard = "VALUE",
    values = 1:8
  )
  x <- reduce_plan(
    x_plan, target = "x_sum", pairwise = FALSE,
    begin = "", end = "", append = FALSE
  )
  x0 <- weak_tibble(
    target = "x_sum",
    command = paste0(x_plan$target, collapse = " + ")
  )
  equivalent_plans(x, x0)
  z <- reduce_plan(
    x_plan, target = "x_sum", pairwise = FALSE,
    begin = "", end = "", append = TRUE
  )
  z0 <- bind_plans(x_plan, x)
  equivalent_plans(z, z0)
  make(rbind(x_plan, x), session_info = FALSE)
  expect_equal(readd(x_sum), sum(1:8))
  clean(destroy = TRUE)

  # Pairwise reduce even number of targets
  x <- reduce_plan(x_plan, target = "x_sum", pairwise = TRUE)
  x0 <- weak_tibble(
    target = c(paste0("x_sum_", 1:6), "x_sum"),
    command = c(
      "x_1 + x_2", "x_3 + x_4", "x_5 + x_6", "x_7 + x_8",
      "x_sum_1 + x_sum_2", "x_sum_3 + x_sum_4",
      "x_sum_5 + x_sum_6"
    )
  )
  equivalent_plans(x, x0)
  x <- reduce_plan(
    x_plan, target = "x_sum", pairwise = FALSE,
    begin = "", end = ""
  )
  x0 <- weak_tibble(
    target = "x_sum",
    command = paste0(x_plan$target, collapse = " + ")
  )
  equivalent_plans(x, x0)
  x <- reduce_plan(x_plan, target = "x_sum", pairwise = TRUE)
  x0 <- weak_tibble(
    target = c(paste0("x_sum_", 1:6), "x_sum"),
    command = c(
      "x_1 + x_2", "x_3 + x_4", "x_5 + x_6", "x_7 + x_8",
      "x_sum_1 + x_sum_2", "x_sum_3 + x_sum_4",
      "x_sum_5 + x_sum_6"
    )
  )
  equivalent_plans(x, x0)
  make(rbind(x_plan, x), session_info = FALSE)
  expect_equal(readd(x_sum), sum(1:8))
  clean(destroy = TRUE)

  # Odd number of targets
  x_plan <- evaluate_plan(
    drake_plan(x = VALUE),
    wildcard = "VALUE",
    values = 1:9
  )
  x <- reduce_plan(x_plan, target = "x_sum", pairwise = TRUE)
  x0 <- weak_tibble(
    target = c(paste0("x_sum_", 1:7), "x_sum"),
    command = c(
      "x_1 + x_2", "x_3 + x_4", "x_5 + x_6", "x_7 + x_8",
      "x_9 + x_sum_1",
      "x_sum_2 + x_sum_3", "x_sum_4 + x_sum_5",
      "x_sum_6 + x_sum_7"
    )
  )
  equivalent_plans(x, x0)
  make(rbind(x_plan, x), session_info = FALSE)
  expect_equal(readd(x_sum), sum(1:9))
  clean(destroy = TRUE)

  # Arbitrary function in reduction
  x_plan <- evaluate_plan(
    drake_plan(x = VALUE),
    wildcard = "VALUE",
    values = 1:8
  )
  fun <- function(x, y) {
    x ^ 2 - 3 * y
  }
  x <- reduce_plan(x_plan, target = "x_sum", pairwise = TRUE,
                   begin = "fun(", op = ", ", end = ")")
  x0 <- weak_tibble(
    target = c(paste0("x_sum_", 1:6), "x_sum"),
    command = c(
      "fun(x_1, x_2)", "fun(x_3, x_4)", "fun(x_5, x_6)", "fun(x_7, x_8)",
      "fun(x_sum_1, x_sum_2)", "fun(x_sum_3, x_sum_4)",
      "fun(x_sum_5, x_sum_6)"
    )
  )
  equivalent_plans(x, x0)
  make(rbind(x_plan, x))
  out <- fun(
    fun(
      fun(1, 2),
      fun(3, 4)
    ),
    fun(
      fun(5, 6),
      fun(7, 8)
    )
  )
  expect_equal(readd(x_sum), out)
}))

test_with_dir("gather_by()", suppressWarnings({
  skip_on_cran()
  plan <- evaluate_plan(
    drake_plan(x = rnorm(m__), y = rexp(n__), z = 10),
    rules = list(
      m__ = 1:2,
      n__ = c("a", "b")
    ),
    trace = TRUE
  )
  x <- gather_by(plan, append = TRUE)
  new_row <- drake_plan(
    target = list(x_1 = x_1, x_2 = x_2, y_a = y_a, y_b = y_b, z = z)
  )
  y <- bind_plans(plan, new_row)
  equivalent_plans(x[, c("target", "command")], y)
  x <- gather_by(plan, append = TRUE, sep = ".")
  equivalent_plans(x[, c("target", "command")], y)
  z <- gather_by(plan, append = FALSE)
  equivalent_plans(z[, c("target", "command")], y[nrow(y), ])
  x <- gather_by(
    plan,
    n___from,
    prefix = "xyz",
    gather = "c",
    append = TRUE,
    sep = "."
  )
  y <- weak_tibble(
    target = c("xyz.y", "xyz.NA"),
    command = c("c(y_a = y_a, y_b = y_b)", "c(x_1 = x_1, x_2 = x_2, z = z)"),
    m__ = as.character(NA),
    m___from = as.character(NA),
    n__ = NA,
    n___from = c("y", NA)
  )
  y <- sanitize_plan(y)
  equivalent_plans(x, bind_plans(plan, y))
  x <- gather_by(plan, n___from, prefix = "xyz", gather = "c", append = TRUE)
  y <- weak_tibble(
    target = c("xyz_y", "xyz_NA"),
    command = c("c(y_a = y_a, y_b = y_b)", "c(x_1 = x_1, x_2 = x_2, z = z)"),
    m__ = as.character(NA),
    m___from = as.character(NA),
    n__ = NA,
    n___from = c("y", NA)
  )
  y <- sanitize_plan(y)
  expected <- bind_plans(plan, y)
  equivalent_plans(x[order(x$target), ], expected[order(expected$target), ])
  x <- gather_by(plan, m__, n__, prefix = "xyz", gather = "c", append = TRUE)
  y <- weak_tibble(
    target = c("xyz_1_NA", "xyz_2_NA", "xyz_NA_a", "xyz_NA_b", "xyz_NA_NA"),
    command = c(
      "c(x_1 = x_1)",
      "c(x_2 = x_2)",
      "c(y_a = y_a)",
      "c(y_b = y_b)",
      "c(z = z)"
    ),
    m__ = as.character(c(1, 2, NA, NA, NA)),
    m___from = as.character(NA),
    n__ = c(NA, NA, "a", "b", NA),
    n___from = as.character(NA)
  )
  y <- sanitize_plan(y)
  expected <- bind_plans(plan, y)
  equivalent_plans(x[order(x$target), ], expected[order(expected$target), ])
  plan$n___from <- c("x", "x", "y", "y", NA)
  x <- gather_by(
    plan,
    n___from,
    prefix = "xyz",
    gather = "c",
    append = TRUE,
    filter = n___from == "x"
  )
  new_row <- drake_plan(xyz_x = c(x_1 = x_1, x_2 = x_2))
  y <- bind_plans(plan, new_row)
  equivalent_plans(x[, c("target", "command")], y)
}))

test_with_dir("reduce_by()", suppressWarnings({
  skip_on_cran()
  plan <- evaluate_plan(
    drake_plan(x = rnorm(m__), y = rexp(n__), z = 10),
    rules = list(
      m__ = 1:4,
      n__ = c("a", "b")
    ),
    trace = TRUE
  )
  x <- reduce_by(plan, pairwise = FALSE, append = TRUE)
  new_row <- drake_plan(target = x_1 + x_2 + x_3 + x_4 + y_a + y_b + z)
  y <- bind_plans(plan, new_row)
  equivalent_plans(x[, c("target", "command")], y)
  z <- reduce_by(plan, pairwise = FALSE, append = FALSE)
  equivalent_plans(z[, c("target", "command")], y[nrow(y), ])
  x <- reduce_by(
    plan, m___from,
    prefix = "xyz",
    op = ", ",
    begin = "c(",
    end = ")",
    append = TRUE,
    sep = "."
  )
  y <- weak_tibble(
    target = c("xyz.1.x", "xyz.2.x", "xyz.x", "xyz.1.NA", "xyz.NA"),
    command = c(
      "c(x_1, x_2)",
      "c(x_3, x_4)",
      "c(xyz.1, xyz.2)",
      "c(y_a, y_b)",
      "c(z, xyz.1)"
    ),
    m__ = as.character(NA),
    m___from = c(rep("x", 3), rep(NA, 2)),
    n__ = as.character(NA),
    n___from = as.character(NA)
  )
  y <- sanitize_plan(y)
  expected <- bind_plans(plan, y)
  equivalent_plans(x[order(x$target), ], expected[order(expected$target), ])
  x <- reduce_by(
    plan, m___from,
    prefix = "xyz",
    op = ", ",
    begin = "c(",
    end = ")",
    append = TRUE
  )
  y <- weak_tibble(
    target = c("xyz_1_x", "xyz_2_x", "xyz_x", "xyz_1_NA", "xyz_NA"),
    command = c(
      "c(x_1, x_2)", "c(x_3, x_4)", "c(xyz_1, xyz_2)",
      "c(y_a, y_b)", "c(z, xyz_1)"
    ),
    m__ = as.character(NA),
    m___from = c(rep("x", 3), rep(NA, 2)),
    n__ = as.character(NA),
    n___from = as.character(NA)
  )
  y <- sanitize_plan(y)
  expected <- bind_plans(plan, y)
  equivalent_plans(x[order(x$target), ], expected[order(expected$target), ])
  x <- reduce_by(
    plan, m___from, prefix = "xyz", op = ", ", begin = "c(", end = ")",
    pairwise = FALSE, append = TRUE
  )
  y <- weak_tibble(
    target = c("xyz_x", "xyz_NA"),
    command = c("c(c(c(x_1, x_2), x_3), x_4)", "c(c(y_a, y_b), z)"),
    m__ = as.character(NA),
    m___from = c("x", NA),
    n__ = as.character(NA),
    n___from = as.character(NA)
  )
  y <- sanitize_plan(y)
  expected <- bind_plans(plan, y)
  equivalent_plans(x[order(x$target), ], expected[order(expected$target), ])
  x <- reduce_by(plan, m___from, n___from, append = TRUE)
  y <- weak_tibble(
    target = c(
      "target_1_x_NA",
      "target_2_x_NA",
      "target_x_NA",
      "target_NA_y"
    ),
    command = c(
      "x_1 + x_2",
      "x_3 + x_4",
      "target_1 + target_2",
      "y_a + y_b"
    ),
    m__ = as.character(NA),
    m___from = c(rep("x", 3), NA),
    n__ = as.character(NA),
    n___from = c(rep(NA, 3), "y")
  )
  y <- sanitize_plan(y)
  expected <- bind_plans(plan, y)
  equivalent_plans(x[order(x$target), ], expected[order(expected$target), ])
  x <- reduce_by(plan, m___from, n___from, pairwise = FALSE, append = TRUE)
  y <- weak_tibble(
    target = c(
      "target_x_NA",
      "target_NA_y",
      "target_NA_NA"
    ),
    command = c(
      "x_1 + x_2 + x_3 + x_4",
      "y_a + y_b",
      "z"
    ),
    m__ = as.character(NA),
    m___from = c("x", NA, NA),
    n__ = as.character(NA),
    n___from = c(NA, "y", NA)
  )
  y <- sanitize_plan(y)
  expected <- bind_plans(plan, y)
  equivalent_plans(x[order(x$target), ], expected[order(expected$target), ])
  plan$from <- c(rep("x", 4), rep("y", 2), NA)
  x <- reduce_by(
    plan,
    from,
    prefix = "xyz",
    append = TRUE,
    pairwise = FALSE,
    filter = from == "y"
  )
  new_row <- drake_plan(xyz_y = y_a + y_b)
  y <- bind_plans(plan, new_row)
  equivalent_plans(x[, c("target", "command")], y)
}))

test_with_dir("get_cache", {
  skip_on_cran()
  make(drake_plan(x = 1), session_info = FALSE)
  tmp1 <- expect_warning(get_cache(search = TRUE), regexp = "deprecated")
  tmp2 <- expect_warning(get_cache(search = FALSE), regexp = "deprecated")
  expect_true(inherits(tmp1, "refclass_decorated_storr"))
  expect_true(inherits(tmp2, "refclass_decorated_storr"))
})

test_with_dir("deprecated memory strategies", {
  skip_on_cran()
  plan <- drake_plan(x = 1)
  expect_warning(
    make(
      plan,
      cache = storr::storr_environment(),
      session_info = FALSE,
      memory_strategy = "memory"
    ),
    regexp = "preclean"
  )
})

test_with_dir("deprecated decorated storr methods", {
  skip_on_cran()
  cache <- new_cache()
  expect_silent(cache$reset_ht_hash())
  expect_equal(1, 1)
})

test_with_dir("config arg of make() (#1118)", {
  skip_on_cran()
  plan <- drake_plan(x = 1)
  config <- drake_config(
    plan,
    session_info = FALSE,
    cache = storr::storr_environment()
  )
  expect_warning(
    make(config = config),
    regexp = "deprecated"
  )
})

test_with_dir("progress(), running(), and failed()", {
  skip_on_cran()
  expect_warning(prg <- progress(x), regexp = "deprecated")
  expect_equal(nrow(prg), 0L)
  plan <- drake_plan(x = stop())
  expect_error(make(plan))
  expect_warning(prg <- progress(x), regexp = "deprecated")
  expect_warning(prg <- progress(progress = "done"), regexp = "deprecated")
  expect_warning(prg <- progress(), regexp = "deprecated")
  expect_warning(run <- running(), regexp = "deprecated")
  expect_warning(fld <- failed(), regexp = "deprecated")
})

test_with_dir("expose_imports() works", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  evalq(
    f <- function(x) {
      g(x)
    },
    envir = envir
  )
  evalq(
    g <- function(x) {
      digest(x)
    },
    envir = envir
  )
  plan <- drake_plan(
    x = f(1),
    y = digest::digest(x)
  ) # double-scoped functions stop the nesting.
  config <- drake_config(plan, envir = envir)
  n_nodes <- length(igraph::V(config$graph)$name)
  expect_true(n_nodes < 10)
  envir <- expect_warning(
    expose_imports(digest, envir = envir),
    message = "deprecated"
  )
  config <- drake_config(plan, envir = envir)
  n_nodes_new <- length(igraph::V(config$graph)$name)
  expect_true(n_nodes_new > n_nodes)
  make_impl(config = config)
  expect_is(readd(x), "character")
})

test_with_dir("move to caching = \"main\" at the top level", {
  skip_if_not_installed("future")
  plan <- drake_plan(x = 1)
  expect_warning(make(plan, caching = "master"), message = "deprecated")
})

test_with_dir("move to caching = \"main\" at the target level", {
  skip_if_not_installed("future")
  plan <- drake_plan(x = target(1, caching = "master"))
  expect_warning(
    make(plan, caching = "worker", parallelism = "future"),
    message = "deprecated"
  )
})
