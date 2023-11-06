drake_context("plans")

test_with_dir("drake_plan_source() in empty commands", {
  skip_on_cran()
  skip_if_not_installed("styler")
  plan <- data.frame(target = "x", command = "", stringsAsFactors = FALSE)
  out <- drake_plan_source(plan)
  expect_true(length(out) > 0L)
})

test_with_dir("duplicated target names", {
  skip_if_not_installed("styler")
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
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
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
      a = 1 -> x, # nolint
      b = 2
    )
  )
  expect_warning(
    tmp <- drake_plan(a = 1, b <- 2),
    regexp = "to assign targets to commands"
  )
  expect_warning(
    tmp <- drake_plan(a = 1, b -> 2), # nolint
    regexp = "to assign targets to commands"
  )
  expect_warning(
    tmp <- drake_plan(a <- 1, b -> 2), # nolint
    regexp = "to assign targets to commands"
  )
})

test_with_dir("File functions handle input", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  expect_equal(
    file_in(1, "x", "y"), c("1", "x", "y")
  )
  expect_equal(
    knitr_in(1, "x", "y"), c("1", "x", "y")
  )
  expect_equal(
    file_out(1, "x", "y"), c("1", "x", "y")
  )

  out <- drake_deps(quote(file_in(c("file1", "file2"))))
  out <- select_nonempty(decode_deps_list(out))
  expect_equal(length(out), 1L)
  out <- sort(out$file_in)
  exp <- sort(c("file1", "file2"))
  expect_equal(out, exp)

  out <- drake_deps(quote(file_out(c("file1", "file2"))))
  out <- select_nonempty(decode_deps_list(out))
  expect_equal(length(out), 1L)
  out <- sort(out$file_out)
  exp <- sort(c("file1", "file2"))
  expect_equal(out, exp)
})

test_with_dir("edge cases for plans", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  # empty plan
  equivalent_plans(drake_plan(), empty_plan())
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
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  for (tidy_eval in c(TRUE, FALSE)) {
    x <- drake_plan(
      a = c,
      b = "c",
      c = d,
      d = readRDS("e"),
      tidy_eval = tidy_eval
    )
    y <- weak_tibble(
      target = letters[1:4],
      command = c("c", "\"c\"", "d", "readRDS(\"e\")")
    )
    equivalent_plans(x, y)
  }
})

test_with_dir("drake_plan() trims outer whitespace in target names", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  for (tidy_eval in c(TRUE, FALSE)) {
    x <- sanitize_plan(weak_tibble(
      target = c(" a", "b \t\n"),
      command = 1:2
    ))
    y <- drake_plan(a = 1, b = 2, tidy_eval = tidy_eval)
    expect_equal(x$target, y$target)
  }
})

test_with_dir("make() trims outer whitespace in target names", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  x <- weak_tibble(
    target = c("a\n", "  b", "c ", "\t  d   "),
    command = 1
  )
  expect_silent(make(x, verbose = 0L, session_info = FALSE))
  expect_equal(sort(cached()), letters[1:4])
  prog <- drake_progress()
  expect_equal(sort(prog$target), letters[1:4])
  expect_true(all(prog$progress == "done"))
  expect_warning({
    make(
      x,
      verbose = 0L,
      targets = c("a", "nobody_home"),
      session_info = FALSE
    )
    con <- drake_config(
      x,
      verbose = 0L,
      targets = c("a", "nobody_home"),
      session_info = FALSE
    )
  })
  expect_true(all(letters[1:4] %in% cached()))
  expect_true(all(letters[1:4] %in% names(con$spec)))
})

test_with_dir("plans can start with bad symbols", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  x <- weak_tibble(
    target = c("a'x'", "b'x'", "_a", "a^-.*"),
    command = 1)
  y <- expect_warning(drake_config(x), regexp = "trailing dot")
  out <- sort(c("a.x_", "b.x_", "X_a", "a..._"))
  expect_true(all(out %in% names(y$spec)))
})

test_with_dir("can use semicolons for multi-line commands", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  plan <- drake_plan(
    x = {a <- 1; a}, # nolint
    y = {
      b <- 2
      b
    }
  )
  make(plan, verbose = 0L, session_info = FALSE)
  expect_false(any(c("a", "b") %in% ls()))
  expect_true(all(c("x", "y") %in% cached()))
  expect_equal(cached(), c("x", "y"))
})

test_with_dir("can use braces for multi-line commands", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
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
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  tidyvar <- 2
  x <- drake_plan(x = target(
    stop(!!tidyvar), worker = !!tidyvar, cpu = 4, custom = list(123), c2 = 5)
  )
  expect_true(is.list(x$custom))
  expect_true(is.list(x$custom[[1]]))
  x$custom <- safe_deparse(x$custom[[1]])
  y <- weak_tibble(
    target = "x",
    command = "stop(2)",
    worker = 2,
    cpu = 4,
    custom = "list(123)",
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
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
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
  equivalent_plans(
    bind_plans(,drake_plan(x = 1, y = 2),,drake_plan(z = 3),,,), # nolint
    drake_plan(x = 1, y = 2, z = 3)
  )
})

test_with_dir("bind_plans() with unequal list columns (#1136)", {
  skip_on_cran()
  skip_if_not_installed("styler")
  plan1 <- drake_plan(
    data_raw = read_data()
  )
  plan2 <- drake_plan(
    data = target(
      download_data(),
      resources = list(ncpus = 1, partition = "cluster")
    )
  )
  out <- bind_plans(plan1, plan2)
  exp <- drake_plan(
    data_raw = read_data(),
    data = target(
      download_data(),
      resources = list(ncpus = 1, partition = "cluster")
    )
  )
  equivalent_plans(out, exp)
})

test_with_dir("spaces in target names are replaced only when appropriate", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  expect_warning(
    pl <- drake_plan(
      a_.b.....x..y. = {
        b
        x
        y
      },
      a_.a..x. = {
        a
        x
      },
      drake_target_1_.b.....x..y. = file_out("{b  \n  x; y}"),
      drake_target_1_.a..x. = file_out("{a; x}")
    ),
    regexp = "trailing dot"
  )
  expect_equal(
    sort(pl$target),
    sort(c(
      "a_.b.....x..y_",
      "a_.a..x_",
      "drake_target_1_.b.....x..y_",
      "drake_target_1_.a..x_"
    ))
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
  skip_if_not_installed("styler")
  writeLines("a <- 1", "script.R")
  plan <- code_to_plan("script.R")
  equivalent_plans(plan, weak_tibble(target = "a", command = "1"))
})

test_with_dir("plan_to_code()", {
  skip_on_cran()
  skip_if_not_installed("styler")
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
  plan <- code_to_plan(path)
  equivalent_plans(plan[order(plan$target), ], plan0[order(plan0$target), ])
})

test_with_dir("plan_to_notebook()", {
  skip_on_cran()
  skip_if_not_installed("styler")
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
  plan <- code_to_plan(path)
  equivalent_plans(plan[order(plan$target), ], plan0[order(plan0$target), ])
})

test_with_dir("commands and triggers can be character strings too", {
  skip_on_cran()
  skip_if_not_installed("styler")
  config <- dbug()
  config$plan <- deparse_lang_cols(config$plan)
  for (col in colnames(config$plan)) {
    config$plan[[col]] <- unclass(config$plan[[col]])
  }
  testrun(config)
  expect_equal(sort(config$plan$target), sort(justbuilt(config)))
  expect_equal(outdated_impl(config), character(0))
  testrun(config)
  expect_equal(character(0), sort(justbuilt(config)))
  expect_equal(outdated_impl(config), character(0))
  config$plan$trigger <- "trigger(condition = TRUE)"
  testrun(config)
  expect_equal(sort(config$plan$target), sort(justbuilt(config)))
  testrun(config)
  expect_equal(sort(config$plan$target), sort(justbuilt(config)))
})

test_with_dir("printing large plans", {
  skip_on_cran()
  skip_if_not_installed("styler")
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
  skip_on_cran()
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

test_with_dir("handle weird missing symbols", {
  skip_if_not_installed("styler")
  out <- drake_plan(
    ,a = 1,,b=f(x),,, # nolint
  )
  exp <- drake_plan(a = 1, b = f(x))
  equivalent_plans(out, exp)
})

# From Kendon Bell: https://github.com/ropensci/drake/issues/200
test_with_dir("drake_plan does tidy eval", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  my_variable <- 5
  plan1 <- drake_plan(a = !!my_variable)
  plan2 <- weak_tibble(target = "a", command = "5")
  equivalent_plans(plan1, plan2)
})

# From Alex Axthelm: https://github.com/ropensci/drake/issues/200
test_with_dir("drake_plan tidy eval can be customized and disabled", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  my_variable <- 5
  plan1 <- drake_plan(
    a = !!my_variable,
    b = !!my_variable + 1,
    c = target(1 + 1, custom = !!my_variable),
    tidy_eval = FALSE
  )
  plan1$custom <- unlist(lapply(plan1[["custom"]], rlang::expr_text))
  plan2 <- drake_plan(
    a = !!my_variable,
    b = !!my_variable + 1,
    c = target(1 + 1, custom = !!my_variable),
    tidy_eval = TRUE
  )
  plan1$command <- unclass(deparse_lang_col(plan1$command))
  plan2$command <- unclass(deparse_lang_col(plan2$command))
  expect_equal(plan1$target, plan2$target)
  expect_false(any(grepl("5", plan1$command)))
  expect_equal(plan2$command, c("5", "5 + 1", "1 + 1"))
  expect_false(any(grepl("5", plan1[["custom"]])))
  expect_equal(plan2$custom, c(NA, NA, 5))
})

# From Kendon Bell: https://github.com/ropensci/drake/issues/200
test_with_dir("make() does tidy eval in commands", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  plan <- drake_plan(
    little_b = "b",
    letter = !!little_b,
    tidy_eval = FALSE
  )
  make(plan)
  expect_equal(readd(letter), "b")
})

test_with_dir("stringsAsFactors can be TRUE", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  f <- function(x) {
    return(x)
  }
  myplan <- data.frame(target = "a", command = "f(\"helloworld\")",
    stringsAsFactors = TRUE)
  expect_true(is.factor(myplan$target))
  expect_true(is.factor(myplan$command))
  make(myplan, verbose = 0L, session_info = FALSE)
  expect_equal(readd(a), "helloworld")
})

test_with_dir("case sensitivity", {
  skip_on_cran()
  skip_if_not_installed("styler")
  plan <- drake_plan(
    a = 1,
    b = 2,
    B = A(),
    c = 15
  )
  A <- function() {
    1 + 1
  }
  expect_warning(
    config <- drake_config(
      plan,
      cache = storr::storr_environment(),
      session_info = FALSE
    ),
    regexp = "when converting to lowercase"
  )
})

test_with_dir("Strings stay strings, not symbols", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("styler")
  expect_silent(x <- drake_plan(a = "A"))
  expect_silent(make(x, verbose = 0L, session_info = FALSE))
})

test_with_dir("missing symbols get replaced (#1299)", {
  skip_if_not_installed("styler")
  plan <- drake_plan(x = NULL)
  expect_silent(make(plan, verbose = 0L, session_info = FALSE))
})

test_with_dir("Trailing slashes in file paths on Windows", {
  skip_on_cran()
  skip_if_not_installed("styler")
  skip_if_not_installed("knitr")
  dir.create("in")
  dir.create("knitr")
  writeLines("abc", "in/abc.txt")
  plan <- drake_plan(
    x = {
      file_in("in/")
      file_out("out/")
      dir.create("out")
      writeLines("123", "out/out.txt")
      123
    }
  )
  # Should produce warnings if the bug comes back:
  make(
    plan,
    cache = storr::storr_environment(),
    session_info = FALSE,
    log_progress = FALSE,
    verbose = 0L
  )
  expect_true(file.exists("out/out.txt"))
})

test_with_dir("supplied a plan instead of a config", {
  skip_on_cran()
  skip_if_not_installed("styler")
  skip_if_not_installed("visNetwork")
  plan <- drake_plan(x = 1)
  expect_error(vis_drake_graph_impl(plan), regexp = "must be a drake_config")
})

test_with_dir("warning when file_out() files not produced", {
  skip_on_cran()
  skip_if_not_installed("styler")
  plan <- drake_plan(
    x = {
      file.create(file_out("a"))
      file_out("b", "c")
    }
  )
  expect_warning(
    make(plan, cache = storr::storr_environment(), session_info = FALSE),
    regexp = "Missing files for target"
  )
})

test_with_dir("id_chr()", {
  skip_on_cran()
  skip_if_not_installed("styler")
  expect_error(id_chr(), regexp = "environment where drake builds targets")
  plan <- drake_plan(x = id_chr())
  make(plan)
  expect_equal(readd(x), "x")
})

test_with_dir("cancel() (#1131)", {
  skip_on_cran()
  skip_if_not_installed("styler")
  f <- function(x) {
    cancel()
    "x"
  }
  g <- function(x) f(x)
  plan <- drake_plan(y = g(1))
  make(plan)
  expect_equal(drake_progress()$progress, "cancelled")
  expect_error(suppressWarnings(readd(y)))
  config <- drake_config(plan)
  expect_equal(justbuilt(config), character(0))
  f <- function(x) {
    cancel(allow_missing = FALSE)
    "x"
  }
  make(plan)
  expect_equal(justbuilt(config), "y")
  f <- function(x) {
    cancel(allow_missing = FALSE)
    "y"
  }
  plan <- drake_plan(y = g(1), z = y)
  make(plan)
  expect_equal(justbuilt(config), "z")
  expect_equal(readd(y), "x")
  expect_equal(readd(z), "x")
})

test_with_dir("cancel_if(TRUE) (#1131)", {
  skip_on_cran()
  skip_if_not_installed("styler")
  f <- function(x) {
    cancel_if(TRUE)
    "x"
  }
  g <- function(x) f(x)
  plan <- drake_plan(y = g(1))
  make(plan)
  expect_error(suppressWarnings(readd(y)))
  config <- drake_config(plan)
  expect_equal(justbuilt(config), character(0))
  f <- function(x) {
    cancel_if(TRUE, allow_missing = FALSE)
    "x"
  }
  make(plan)
  expect_equal(justbuilt(config), "y")
  f <- function(x) {
    cancel_if(TRUE, allow_missing = FALSE)
    "y"
  }
  plan <- drake_plan(y = g(1), z = y)
  make(plan)
  expect_equal(justbuilt(config), "z")
  expect_equal(readd(y), "x")
  expect_equal(readd(z), "x")
})

test_with_dir("cancel_if(condition) (#1131)", {
  skip_on_cran()
  skip_if_not_installed("styler")
  f <- function(x) {
    cancel_if(x > 1)
    "x"
  }
  plan <- drake_plan(x = f(0), y = f(2))
  make(plan)
  expect_equal(cached(), "x")
})

test_with_dir("cancel_if(bad condition) (#1131)", {
  skip_on_cran()
  skip_if_not_installed("styler")
  plan <- drake_plan(x = cancel_if(1:2))
  expect_error(make(plan), regexp = "length 1 in cancel_if")
})

test_with_dir("cancel in incorrect context (#1131)", {
  skip_on_cran()
  skip_if_not_installed("styler")
  expect_error(cancel(), regexp = "where drake builds targets")
  expect_error(cancel_if(TRUE), regexp = "where drake builds targets")
})

test_with_dir("convert_trailing_dot() (#1147)", {
  skip_if_not_installed("styler")
  expect_equal(
    expect_warning(
      convert_trailing_dot(c("numeric_ids_.1.", "numeric_ids_.2."))
    ),
    c("numeric_ids_.1_", "numeric_ids_.2_")
  )
  expect_equal(
    expect_warning(
      convert_trailing_dot(c("numeric_ids_.1._", "numeric_ids_.2."))
    ),
    c("numeric_ids_.1._", "numeric_ids_.2_")
  )
  expect_equal(convert_trailing_dot(letters), letters)
})

test_with_dir("convert_trailing_dot() in plans (#1147)", {
  skip_if_not_installed("styler")
  n <- seq_len(2)
  ids <- rlang::syms(as.character(n))
  expect_warning(
    plan <- drake_plan(
      numeric_ids = target(
        rnorm(n),
        transform = map(
          n = !!n,
          ids = !!ids,
          .id = ids
        )
      )
    ),
    regexp = "trailing dot"
  )
  expect_equal(
    plan$target,
    c("numeric_ids_.1_", "numeric_ids_.2_")
  )
})

test_with_dir("trailing dots in imports (#1147)", {
  skip_if_not_installed("styler")
  expect_warning(
    assert_no_trailing_dot("x.", force = TRUE),
    regexp = "imports must not end with dots on Windows"
  )
})

test_with_dir("type_sum() S3 method for printing language columns", {
  skip_if_not_installed("styler")
  expect_equal(type_sum.expr_list("123"), "expr")
})

test_with_dir("illegal plan error message (#1334)", {
  skip_if_not_installed("styler")
  expect_error(make(plan = function(x) x), regexp = "drake plan")
})

test_with_dir("coerce to language with sanitize_command() (#1372)", {
  skip_if_not_installed("styler")
  skip_on_cran()
  library(drake)
  a <- c()
  b <- list(1L)
  plan <- drake_plan(
    x = !!a,
    y = !!b,
    z = !!mtcars
  )
  make(plan)
  expect_equal(readd(x), NULL)
  expect_equal(readd(y), list(1L))
  expect_equal(readd(z), mtcars)
})
