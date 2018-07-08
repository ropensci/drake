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
  expect_warning(backend())
})

test_with_dir("deprecation: make() and config()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_warning(default_system2_args(jobs = 1, verbose = FALSE))
  expect_warning(make(drake_plan(x = 1), return_config = TRUE,
    verbose = FALSE, session_info = FALSE))
  expect_warning(config(drake_plan(x = 1)))
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
    coef = coefficients(..analysis..)) # nolint
  expect_warning(
    summaries(summary_types, analyses, datasets))
})

test_with_dir("deprecated graphing functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl <- drake_plan(a = 1)
  expect_warning(build_graph(pl))
  con <- drake_config(plan = pl)
  expect_warning(out <- plot_graph(config = con))
  df <- drake_graph_info(config = con)
  expect_warning(out <- render_graph(df))
})

test_with_dir("deprecated example(s)_drake functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_warning(example_drake())
  expect_warning(examples_drake())
})

test_with_dir("deprecate misc utilities", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  expect_true(is.character(readd("'file.csv'")))
  expect_error(is.character(`"file.csv"`))
  expect_silent(loadd("'file.csv'", verbose = FALSE))
  expect_true(is.character(`"file.csv"`))
  expect_equal(
    z,
    tibble::tibble(
      target = c("\"file.csv\"", "contents"),
      command = c("write.csv(mtcars, file = \"file.csv\")", "read.csv('file.csv')") # nolint
    )
  )
  expect_equal(sort(justbuilt(config)), sort(c("contents", "\"file.csv\"")))
})

test_with_dir("example template files (deprecated)", {
  expect_false(file.exists("slurm_batchtools.tmpl"))
  expect_warning(
    drake_batchtools_tmpl_file("slurm_batchtools.tmpl"),
    regexp = "deprecated"
  )
  expect_true(file.exists("slurm_batchtools.tmpl"))
})

test_with_dir("plan set 1", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
