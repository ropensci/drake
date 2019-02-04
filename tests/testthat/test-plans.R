drake_context("plans")

test_with_dir("drake_plan_source() in empty commands", {
  skip_on_cran()
  skip_if_not_installed("styler")
  plan <- data.frame(target = "x", command = "", stringsAsFactors = FALSE)
  out <- drake_plan_source(plan)
  expect_true(length(out) > 0L)
})

test_with_dir("duplicated target names", {
  expect_error(
    drake_plan(
      a = 1,
      a = 2,
      b = 1,
      b = 2,
      c = 3
    ),
    regexp = "duplicated target names"
  )
  expect_error(
    bind_plans(
      drake_plan(a = 1, b = 1, c = 1),
      drake_plan(a = 5, b = 2, d = 5)
    ),
    regexp = "duplicated target names"
  )
})

test_with_dir("warn about <- and -> in drake_plan()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_silent(tmp <- drake_plan())
  expect_silent(tmp <- drake_plan(a = 1, b = 2))
  expect_silent(
    tmp <- drake_plan(
      a = {
        x <- 1
        x
      }
    )
  )
  expect_silent(
    tmp <- drake_plan(
      a = x <- 1,
      b = 2
    )
  )
  expect_silent(
    tmp <- drake_plan(
      a = 1 -> x,
      b = 2
    )
  )
  expect_warning(
    tmp <- drake_plan(a = 1, b <- 2),
    regexp = "to assign targets to commands"
  )
  expect_warning(
    tmp <- drake_plan(a = 1, b -> 2),
    regexp = "to assign targets to commands"
  )
  expect_warning(
    tmp <- drake_plan(a <- 1, b -> 2),
    regexp = "to assign targets to commands"
  )
})

test_with_dir("File functions handle input", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(
    file_in(1, "x", "y"), c("1", "x", "y")
  )
  expect_equal(
    knitr_in(1, "x", "y"), c("1", "x", "y")
  )
  expect_equal(
    file_out(1, "x", "y"), c("1", "x", "y")
  )

  out <- analyze_code(quote(file_in(c("file1", "file2"))))
  out <- decode_deps_list(out)
  expect_equal(length(out), 1L)
  out <- sort(out$file_in)
  exp <- sort(c("file1", "file2"))
  expect_equal(out, exp)

  out <- analyze_code(quote(file_out(c("file1", "file2"))))
  out <- decode_deps_list(out)
  expect_equal(length(out), 1L)
  out <- sort(out$file_out)
  exp <- sort(c("file1", "file2"))
  expect_equal(out, exp)
})

test_with_dir("edge cases for plans", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  # empty plan
  equivalent_plans(
    drake_plan(),
    weak_tibble(
      target = character(0),
      command = character(0)
    )
  )
  # no target names
  equivalent_plans(
    drake_plan(a, b),
    weak_tibble(
      target = c("drake_target_1", "drake_target_2"),
      command = c("a", "b")
    )
  )
  o <- drake_plan(a, b)
  expect_equal(
    sort(unname(unclass(deparse_lang_col(o$command)))),
    sort(c("a", "b"))
  )
  # incomplete target names
  equivalent_plans(
    drake_plan(a = 1, b),
    weak_tibble(
      target = c("a", "drake_target_1"),
      command = c("1", "b")
    )
  )
  # multiple file outputs are okay
  equivalent_plans(
    drake_plan(
      a = file_out("file1", "file2")
    ),
    weak_tibble(
      target = "a",
      command = "file_out(\"file1\", \"file2\")"
    )
  )
  equivalent_plans(
    drake_plan(
      a = file_out(c("file1", "file2"))
    ),
    weak_tibble(
      target = "a",
      command = "file_out(c(\"file1\", \"file2\"))"
    )
  )
})

test_with_dir("plan set 2", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  for (tidy_evaluation in c(TRUE, FALSE)) {
    x <- drake_plan(
      a = c,
      b = "c",
      c = d,
      d = readRDS("e"),
      tidy_evaluation = tidy_evaluation
    )
    y <- weak_tibble(
      target = letters[1:4],
      command = c("c", "\"c\"", "d", "readRDS(\"e\")")
    )
    equivalent_plans(x, y)
  }
})

test_with_dir("drake_plan() trims outer whitespace in target names", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  for (tidy_evaluation in c(TRUE, FALSE)) {
    x <- sanitize_plan(weak_tibble(
      target = c(" a", "b \t\n"),
      command = 1:2
    ))
    y <- drake_plan(a = 1, b = 2, tidy_evaluation = tidy_evaluation)
    expect_equal(x$target, y$target)
  }
})

test_with_dir(
  "make() trims outer whitespace in target names", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- weak_tibble(target = c("a\n", "  b", "c ", "\t  d   "),
                  command = 1)
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
  expect_equal(sort(cached()), letters[1:4])
  stat <- c(a = "finished", b = "finished", c = "finished",
            d = "finished")
  expect_equal(progress(), stat)
  expect_warning({
    make(
      x,
      verbose = FALSE,
      targets = c("a", "nobody_home"),
      session_info = FALSE
    )
    con <- drake_config(
      x,
      verbose = FALSE,
      targets = c("a", "nobody_home"),
      session_info = FALSE
    )
  })
  expect_true(all(letters[1:4] %in% cached()))
  expect_true(all(letters[1:4] %in% con$plan$target))
})

test_with_dir("plans can start with bad symbols", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- weak_tibble(
    target = c("a'x'", "b'x'", "_a", "a^-.*"),
    command = 1)
  y <- drake_config(x)
  expect_equal(y$plan$target, c("a.x.", "b.x.", "X_a", "a...."))
})

test_with_dir("issue 187 on Github (from Kendon Bell)", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  test <- drake_plan(test = run_it(wc__))
  out <- evaluate_plan(test, rules = list(wc__ = list(1:4, 5:8, 9:12)))
  out2 <- weak_tibble(
    target = c("test_1.4", "test_5.8", "test_9.12"),
    command = c("run_it(1:4)", "run_it(5:8)", "run_it(9:12)")
  )
  equivalent_plans(out, out2)
})

test_with_dir("can use semicolons for multi-line commands", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(
    x = {a <- 1; a}, # nolint
    y = {
      b <- 2
      b
    }
  )
  make(plan, verbose = FALSE, session_info = FALSE)
  expect_false(any(c("a", "b") %in% ls()))
  expect_true(all(cached(x, y, search = FALSE)))
  expect_equal(cached(search = FALSE), c("x", "y"))
})

test_with_dir("can use braces for multi-line commands", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  small_plan <- drake_plan(
    small_target = {
      local_object <- 1 + 1
      2 + local_object
    }
  )
  make(small_plan, session_info = FALSE)
  expect_true("small_target" %in% cached())
  expect_false("local_object_target" %in% cached())
  expect_equal(readd(small_target), 4)
  expect_false("local_object" %in% ls())
})

test_with_dir("custom column interface", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  tidyvar <- 2
  x <- drake_plan(x = target(
    stop(!!tidyvar), worker = !!tidyvar, cpu = 4, custom = stop(), c2 = 5)
  )
  expect_true(is.list(x$custom))
  expect_true(is.language(x$custom[[1]]))
  x$custom <- safe_deparse(x$custom[[1]])
  y <- weak_tibble(
    target = "x",
    command = "stop(2)",
    worker = 2,
    cpu = 4,
    custom = "stop()",
    c2 = 5
  )
  equivalent_plans(x, y)
  plan <- drake_plan(
    x = target(
      command = 1 + !!tidyvar,
      trigger = trigger(condition = TRUE),
      user_column_1 = 1,
      user_column_2 = "some text"
    ),
    y = target(
      command = Sys.sleep("not a number"),
      col3 = "some text"
    ),
    z = rnorm(10)
  )
  plan0 <- weak_tibble(
    target = c("x", "y", "z"),
    command = c("1 + 2", "Sys.sleep(\"not a number\")", "rnorm(10)"),
    trigger = c("trigger(condition = TRUE)", "NA", "NA"),
    user_column_1 = c(1, NA, NA),
    user_column_2 = c("some text", NA, NA),
    col3 = c(NA, "some text", NA)
  )
  cn <- colnames(plan)
  expect_equal(cn, colnames(plan0))
  equivalent_plans(plan[, cn], plan0[, cn])
})

test_with_dir("bind_plans()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan1 <- drake_plan(a = 1, b = 2)
  plan2 <- drake_plan(c = 3, d = 4)
  plan3 <- drake_plan(e = 5, f = 6)
  plan4 <- drake_plan(
    a = 1,
    b = 2,
    c = 3,
    d = 4,
    e = 5,
    f = 6
  )
  equivalent_plans(bind_plans(plan1, plan2), plan4[1:4, ])
  plan5 <- drake_plan(
    z = target(
      command = download_data(),
      trigger = trigger(condition = TRUE)
    )
  )
  plan6 <- drake_plan(u = 3, v = 4, w = 5)
  out <- bind_plans(plan5, plan6)
  exp <- weak_tibble(
    target = c("z", "u", "v", "w"),
    command = c("download_data()", "3", "4", "5"),
    trigger = c("trigger(condition = TRUE)", rep(NA_character_, 3))
  )
  exp <- sanitize_plan(exp)
  equivalent_plans(out, exp)
  plan2$trigger <- c("trigger(condition = TRUE)", NA_character_)
  plan2 <- sanitize_plan(plan2)
  exp <- weak_tibble(
    target = letters[1:6],
    command = as.character(1:6),
    trigger = c(NA, NA, "trigger(condition = TRUE)", NA, NA, NA)
  )
  exp <- sanitize_plan(exp)
  equivalent_plans(bind_plans(plan1, plan2, plan3), exp)
  equivalent_plans(bind_plans(list(plan1, plan2, plan3)), exp)
  equivalent_plans(bind_plans(list(list(plan1, plan2, plan3))), exp)
  equivalent_plans(bind_plans(list(plan1, list(plan2, plan3))), exp)
  equivalent_plans(bind_plans(list(plan1, list(plan2, list(plan3)))), exp)
  equivalent_plans(
    bind_plans(list(list(plan1), list(plan2, list(plan3)))),
    exp
  )
  equivalent_plans(
    bind_plans(list(list(plan1), list(plan2), list(plan3))),
    exp
  )
})

test_with_dir("spaces in target names are replaced only when appropriate", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl <- drake_plan(a = x__, file_out("x__"))
  pl <- evaluate_plan(
    pl, wildcard = "x__", values = c("{b  \n  x; y}", "{a; x}")
  )
  expect_equal(
    sort(pl$target),
    sort(c(
      "a_.b.....x..y.",
      "a_.a..x.",
      "drake_target_1_.b.....x..y.",
      "drake_target_1_.a..x."
    ))
  )
})

test_with_dir("conflicts in wildcard names/values", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
})

test_with_dir("bad 'columns' argument to evaluate_plan()", {
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
})

test_with_dir("drake_plan_call() produces the correct calls", {
  skip_on_cran()
  skip_if_not_installed("styler")
  load_mtcars_example()
  my_plan$trigger <- "NA"
  my_plan$trigger[[4]] <- "trigger(condition = is_tuesday(), file = FALSE)"
  my_plan <- sanitize_plan(my_plan)
  my_plan$non_standard_column <- 1234
  new_plan <- eval(drake_plan_call(my_plan))
  expected <- my_plan
  equivalent_plans(
    new_plan[, sort(colnames(new_plan))],
    expected[, sort(colnames(expected))]
  )
})

test_with_dir("drake_plan_source()", {
  skip_on_cran()
  skip_if_not_installed("styler")
  skip_if_not_installed("prettycode")
  plan <- drake::drake_plan(
    small_data = download_data("https://some_website.com"),
    large_data_raw = target(
      command = download_data("https://lots_of_data.com"),
      trigger = trigger(
        change = time_last_modified("https://lots_of_data.com"),
        command = FALSE,
        depend = FALSE
      ),
      elapsed = 1e3
    )
  )
  x <- drake_plan_source(plan)
  y <- capture.output(print(x))
  expect_true(grepl("drake_plan", x[1]))
  expect_true(grepl("drake_plan", y[1]))
  writeLines(x, "script.R")
  plan2 <- source("script.R")$value
  equivalent_plans(plan, plan2)
})

test_with_dir("code_to_plan(), one target", {
  skip_on_cran()
  skip_if_not_installed("CodeDepends")
  writeLines("a <- 1", "script.R")
  plan <- code_to_plan("script.R")
  equivalent_plans(plan, weak_tibble(target = "a", command = "1"))
})

test_with_dir("plan_to_code()", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  skip_if_not_installed("tibble")
  expect_false(file.exists("report.md"))
  load_mtcars_example()
  plan0 <- my_plan
  path <- tempfile()
  plan_to_code(my_plan, path)
  source(path, local = TRUE)
  expect_true(is.numeric(coef_regression2_large))
  expect_true(file.exists("report.md"))
  skip_if_not_installed("CodeDepends")
  plan <- code_to_plan(path)
  equivalent_plans(plan[order(plan$target), ], plan0[order(plan0$target), ])
})

test_with_dir("plan_to_notebook()", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  skip_if_not_installed("tibble")
  expect_false(file.exists("report.md"))
  load_mtcars_example()
  plan0 <- my_plan
  path <- "my_notebook.Rmd"
  plan_to_notebook(my_plan, path)
  knitr::knit(path, quiet = TRUE)
  expect_true(is.numeric(coef_regression2_large))
  expect_true(file.exists("report.md"))
  skip_if_not_installed("CodeDepends")
  plan <- code_to_plan(path)
  equivalent_plans(plan[order(plan$target), ], plan0[order(plan0$target), ])
})

test_with_dir("can use from_plan() from within make()", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  caching <- scenario$caching
  plan <- drake_plan(my_target = target(from_plan("a"), a = "a_value"))
  cache <- new_cache()
  config <- drake_config(
    plan, cache = cache, jobs = jobs,
    parallelism = parallelism, caching = caching,
    log_progress = TRUE
  )
  make(config = config)
  expect_equal(justbuilt(config), "my_target")
  expect_equal("a_value", readd(my_target, cache = cache))
  make(config = config)
  expect_equal(justbuilt(config), character(0))
  plan <- drake_plan(my_target = target(from_plan("a"), a = "nope"))
  config <- drake_config(
    plan, cache = cache, jobs = jobs,
    parallelism = parallelism, caching = caching
  )
  make(config = config)
  expect_equal(justbuilt(config), character(0))
})

test_with_dir("commands and triggers can be character strings too", {
  config <- dbug()
  config$plan <- deparse_lang_cols(config$plan)
  for (col in colnames(config$plan)) {
    config$plan[[col]] <- unclass(config$plan[[col]])
  }
  testrun(config)
  expect_equal(sort(config$plan$target), sort(justbuilt(config)))
  expect_equal(outdated(config), character(0))
  testrun(config)
  expect_equal(character(0), sort(justbuilt(config)))
  expect_equal(outdated(config), character(0))
  config$plan$trigger <- "trigger(condition = TRUE)"
  testrun(config)
  expect_equal(sort(config$plan$target), sort(justbuilt(config)))
  testrun(config)
  expect_equal(sort(config$plan$target), sort(justbuilt(config)))
})

test_with_dir("printing large plans", {
  skip_if_not_installed("tibble")
  tmp <- capture.output({
    z <- seq_len(1e3)
    plan <- drake_plan(x = target(y, transform = map(y = !!z)))
    out <- print(plan)
    out <- print(plan[1:5, ])
    out <- print(as_drake_plan(plan, .force_df = TRUE))
    expect_true(is.data.frame(out))
  })
})

test_with_dir("drake_plan_source() with character columns", {
  skip_if_not_installed("styler")
  skip_if_not_installed("tibble")
  exp <- dbug_plan()
  config <- dbug()
  config$plan <- deparse_lang_cols(config$plan)
  for (col in colnames(config$plan)) {
    config$plan[[col]] <- unclass(config$plan[[col]])
  }
  config$plan$trigger <- "trigger(condition = TRUE)"
  exp$trigger <- lapply(config$plan$trigger, safe_parse)
  expect_equal(drake_plan_source(config$plan), drake_plan_source(exp))
})
