drake_context("deprecation")

test_with_dir("pkgconfig::get_config(\"drake::strings_in_dots\")", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  old_strings_in_dots <- pkgconfig::get_config("drake::strings_in_dots")
  on.exit(
    pkgconfig::set_config("drake::strings_in_dots" = old_strings_in_dots)
  )
  cmd <- "readRDS('my_file.rds')"
  pkgconfig::set_config("drake::strings_in_dots" = "literals")
  expect_equal(command_dependencies(cmd), list(globals = "readRDS"))
  pkgconfig::set_config("drake::strings_in_dots" = "garbage")
  expect_equal(
    expect_warning(command_dependencies(cmd)),
    list(globals = "readRDS", file_in = "\"my_file.rds\"")
  )
})

test_with_dir("deprecation: examples", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_warning(load_basic_example())
})

test_with_dir("deprecation: future", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("future")
  expect_warning(backend())
})

test_with_dir("deprecation: make() and config() etc.", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_warning(default_system2_args(jobs = 1, verbose = FALSE))
  expect_warning(make(drake_plan(x = 1), return_config = TRUE,
    verbose = FALSE, session_info = FALSE))
  config <- expect_warning(config(drake_plan(x = 1)))
  expect_warning(deps_targets("x", config), regexp = "deprecated")
})

test_with_dir("deprecation: cache functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(x = 1)
  expect_error(expect_warning(tmp <- read_drake_meta(search = FALSE)))
  expect_silent(make(plan, verbose = FALSE, session_info = FALSE))
  expect_true(is.numeric(readd(x, search = FALSE)))
  expect_equal(cached(), "x")
  expect_warning(read_config())
  expect_warning(read_graph())
  expect_warning(read_plan())
  expect_true(expect_warning(is.list(
    tmp <- read_drake_meta(targets = NULL, search = FALSE))))
  expect_true(expect_warning(is.list(
    tmp <- read_drake_meta(targets = "x", search = FALSE))))
})

test_with_dir("drake_plan deprecation", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl1 <- expect_warning(drake::plan(x = 1, y = x))
  pl2 <- drake_plan(x = 1, y = x)
  pl3 <- expect_warning(plan_drake(x = 1, y = x))
  expect_warning(drake::plan())
  expect_warning(drake::plan_drake())
  expect_warning(drake::workflow())
  expect_warning(drake::workplan())
  expect_warning(drake::plan(x = y, file_targets = TRUE))
  expect_warning(drake::plan_drake(x = y, file_targets = TRUE))
  expect_warning(drake::workflow(x = y, file_targets = TRUE))
  expect_warning(drake::workplan(x = y, file_targets = TRUE))
  expect_warning(check(drake_plan(a = 1)))
  expect_warning(drake_plan(a = 'file', strings_in_dots = NULL)) # nolint
})

test_with_dir("drake version checks in previous caches", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  # We need to be able to set the drake version
  # to check back compatibility.
  plan <- drake_plan(x = 1)
  expect_silent(make(plan, verbose = FALSE))
  x <- get_cache()
  expect_warning(session())
  x$del(key = "initial_drake_version", namespace = "session")
  expect_false("initial_drake_version" %in% x$list(namespace = "session"))
  set_initial_drake_version(cache = x)
  expect_true("initial_drake_version" %in% x$list(namespace = "session"))
})

test_with_dir("generative templating deprecation", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_warning(drake::evaluate(drake_plan()))
  expect_warning(drake::expand(drake_plan()))
  expect_warning(drake::gather(drake_plan()))
  datasets <- drake_plan(
    small = simulate(5),
    large = simulate(50))
  methods <- drake_plan(
    regression1 = reg1(..dataset..), # nolint
    regression2 = reg2(..dataset..)) # nolint
  expect_warning(
    analyses <- analyses(methods, datasets = datasets))
  summary_types <- drake_plan(
    summ = summary(..analysis..), # nolint
    coef = stats::coefficients(..analysis..)) # nolint
  expect_warning(
    summaries(summary_types, analyses, datasets))
})

test_with_dir("deprecated graphing functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl <- drake_plan(a = 1, b = 2)
  expect_warning(build_graph(pl))
  expect_warning(build_drake_graph(pl, sanitize_plan = TRUE))
  con <- drake_config(plan = pl)
  skip_if_not_installed("visNetwork")
  expect_warning(out <- plot_graph(config = con))
  skip_if_not_installed("ggraph")
  expect_warning(out <- static_drake_graph(config = con))
  expect_true(inherits(out, "gg"))
  df <- drake_graph_info(config = con)
  expect_warning(out <- render_graph(df))
  expect_warning(out <- render_static_drake_graph(df))
  expect_true(inherits(out, "gg"))
})

test_with_dir("deprecated example(s)_drake functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("downloader")
  expect_warning(example_drake())
  expect_warning(examples_drake())
})

test_with_dir("deprecate misc utilities", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("visNetwork")
  expect_error(parallel_stages(1), regexp = "parallelism")
  expect_error(rate_limiting_times(1), regexp = "parallelism")
  expect_warning(as_file("x"))
  expect_warning(as_drake_filename("x"))
  expect_warning(drake_unquote("x", deep = TRUE))
  cache <- storr::storr_environment()
  expect_warning(configure_cache(cache, log_progress = TRUE))
  expect_warning(max_useful_jobs(config(drake_plan(x = 1))))
  expect_warning(deps(123))
  load_mtcars_example()
  config <- drake_config(my_plan)
  expect_warning(tmp <- dataframes_graph(config))
  expect_warning(migrate_drake_project())
  expect_null(warn_single_quoted_files(character(0), list()))
})

test_with_dir("deprecated arguments", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl <- drake_plan(a = 1, b = a)
  expect_warning(
    con <- drake_config(
      plan = pl,
      session_info = FALSE,
      imports_only = FALSE
    )
  )
  expect_warning(drake_build(a, config = con, meta = list()))
})

test_with_dir("old file API", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  old_strings_in_dots <- pkgconfig::get_config("drake::strings_in_dots")
  on.exit(
    pkgconfig::set_config("drake::strings_in_dots" = old_strings_in_dots)
  )
  pkgconfig::set_config("drake::strings_in_dots" = "filenames")
  expect_warning(x <- drake_plan(
    file.csv = write.csv(mtcars, file = "file.csv"),
    strings_in_dots = "literals",
    file_targets = TRUE
  ))
  expect_warning(y <- drake_plan(
    contents = read.csv('file.csv'), # nolint
    strings_in_dots = "filenames"
  ))
  z <- rbind(x, y)
  expect_warning(check_plan(z))
  expect_warning(make(z, session_info = FALSE) -> config)
  expect_equal(readd("'file.csv'"), readd("\"file.csv\""))
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

test_with_dir("plan set 1", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  old_strings_in_dots <- pkgconfig::get_config("drake::strings_in_dots")
  on.exit(
    pkgconfig::set_config("drake::strings_in_dots" = old_strings_in_dots)
  )
  pkgconfig::set_config("drake::strings_in_dots" = "filenames")
  for (tidy_evaluation in c(TRUE, FALSE)){
    expect_warning(x <- drake_plan(
      a = c,
      b = "c",
      list = c(c = "d", d = "readRDS('e')"),
      tidy_evaluation = tidy_evaluation,
      strings_in_dots = "filenames"
    ))
    y <- tibble(
      target = letters[1:4],
      command = c("c", "'c'",
      "d", "readRDS('e')"))
    expect_equal(x, y)
    expect_warning(check_plan(x))
  }
})

test_with_dir("force loading a non-back-compatible cache", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_null(assert_compatible_cache(NULL))
  expect_null(get_cache())
  expect_null(this_cache())
  expect_true(inherits(recover_cache(), "storr"))
  write_v4.3.0_project() # nolint
  suppressWarnings({
    expect_error(get_cache())
    expect_error(this_cache())
    expect_error(recover_cache())
  })
  expect_true(inherits(get_cache(force = TRUE), "storr"))
  expect_true(inherits(this_cache(force = TRUE), "storr"))
  expect_true(inherits(recover_cache(force = TRUE), "storr"))
  load_mtcars_example(force = TRUE)
  config <- drake_config(my_plan, force = TRUE)
  expect_true(length(outdated(config)) > 0)
  expect_error(
    expect_warning(
      make(my_plan, verbose = FALSE, session_info = FALSE),
      regexp = "inconvenience"
    ),
    regexp = "force"
  )
  make(my_plan, verbose = FALSE, force = TRUE)
  expect_equal(outdated(config), character(0))
  expect_true(length(cached()) > 0)
  clean()
  expect_true(length(cached()) == 0)
})

test_with_dir("old trigger interface", {
  skip_on_cran()
  for (old_trigger in suppressWarnings(triggers())){
    plan <- drake_plan(x = 1)
    plan$trigger <- old_trigger
    clean()
    expect_warning(
      config <- make(
        plan,
        session_info = FALSE,
        cache = storr::storr_environment()
      ),
      regexp = "old trigger interface is deprecated"
    )
    trigger <- diagnose(x, cache = config$cache)$trigger
    expect_true(is.list(trigger))
    if (identical(trigger$condition, TRUE)){
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
  expect_warning(default_hook(NULL), regexp = "deprecated")
  expect_warning(empty_hook(NULL), regexp = "deprecated")
  expect_warning(message_sink_hook(NULL), regexp = "deprecated")
  expect_warning(output_sink_hook(NULL), regexp = "deprecated")
  expect_warning(silencer_hook(NULL), regexp = "deprecated")
})
