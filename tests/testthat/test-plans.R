drake_context("plans")

test_with_dir("duplicated targets", {
  expect_error(
    drake_plan(
      a = 1,
      a = 2,
      b = 1,
      b = 2,
      c = 3
    ),
    regexp = "Duplicated targets"
  )
  expect_error(
    bind_plans(
      drake_plan(a = 1, b = 1, c = 1),
      drake_plan(a = 5, b = 2, d = 5)
    ),
    regexp = "Duplicated targets"
  )
  expect_equal(
    bind_plans(
      drake_plan(a = 1, b = 1, c = 1),
      drake_plan(a = 1, b = 1, d = 5)
    ),
    drake_plan(
      a = 1,
      b = 1,
      c = 1,
      d = 5
    )
  )
  expect_equal(
    bind_plans(
      drake_plan(d = f(c, b)),
      drake_plan(c = f(a), a = 5),
      drake_plan(b = f(a), a = 5)
    ),
    drake_plan(
      d = f(c, b),
      c = f(a),
      a = 5,
      b = f(a)
    )
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
  expect_equal(
    code_dependencies(quote(file_out(c("file1", "file2")))),
    list(file_out = drake_quotes(c("file1", "file2"), single = FALSE))
  )
})

test_with_dir("edge cases for plans", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  # empty plan
  expect_equal(
    drake_plan(),
    tibble::tibble(
      target = character(0),
      command = character(0)
    )
  )
  # no target names
  expect_equal(
    drake_plan(a, b),
    tibble::tibble(
      target = c("drake_target_1", "drake_target_2"),
      command = c("a", "b")
    )
  )
  expect_equal(
    drake_plan(list = c("a", "b")),
    drake_plan(a, b)
  )
  # incomplete target names
  expect_equal(
    drake_plan(a = 1, b),
    tibble::tibble(
      target = c("a", "drake_target_1"),
      command = c("1", "b")
    )
  )
  # multiple file outputs are okay
  expect_equal(
    drake_plan(
      a = file_out("file1", "file2"),
      strings_in_dots = "literals"
    ),
    tibble::tibble(
      target = "a",
      command = "file_out(\"file1\", \"file2\")"
    )
  )
  expect_equal(
    drake_plan(
      a = file_out(c("file1", "file2")),
      strings_in_dots = "literals"
    ),
    tibble::tibble(
      target = "a",
      command = "file_out(c(\"file1\", \"file2\"))"
    )
  )
})

test_with_dir("plan set 2", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  for (tidy_evaluation in c(TRUE, FALSE)){
    x <- drake_plan(
      a = c,
      b = "c",
      list = c(c = "d", d = "readRDS('e')"),
      strings_in_dots = "literals",
      tidy_evaluation = tidy_evaluation
    )
    y <- tibble(
      target = letters[1:4],
      command = c("c", "\"c\"",
                  "d", "readRDS('e')"))
    expect_equal(x, y)
  }
})

test_with_dir("plan set 3", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  for (tidy_evaluation in c(TRUE, FALSE)){
  expect_warning(x <- drake_plan(
    a = c,
    b = "c",
    list = c(c = "d", d = "readRDS('e')"),
    strings_in_dots = "literals", file_targets = TRUE,
    tidy_evaluation = tidy_evaluation))
  y <- tibble::tibble(
    target = drake::drake_quotes(letters[1:4], single = TRUE),
    command = c("c", "\"c\"", "d", "readRDS('e')"))
  expect_equal(x, y)
  }
})

test_with_dir("drake_plan() trims outer whitespace in target names", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  for (tidy_evaluation in c(TRUE, FALSE)){
    x <- drake_plan(list = c(` a` = 1, `b \t\n` = 2),
                    tidy_evaluation = tidy_evaluation)
    y <- drake_plan(a = 1, b = 2, tidy_evaluation = tidy_evaluation)
    expect_equal(x$target, y$target)
  }
})

test_with_dir(
  "make() and check_plan() trim outer whitespace in target names", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- tibble(target = c("a\n", "  b", "c ", "\t  d   "),
                  command = 1)
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
  expect_equal(sort(cached()), letters[1:4])
  stat <- c(a = "finished", b = "finished", c = "finished",
            d = "finished")
  expect_equal(progress(), stat)
  expect_warning(
    con <- make(
      x,
      verbose = FALSE,
      targets = c("a", "nobody_home"),
      session_info = FALSE
    )
  )
  expect_true(all(letters[1:4] %in% cached()))
  expect_true(all(letters[1:4] %in% con$plan$target))
})

test_with_dir("make() plays nicely with tibbles", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("pillar")
  skip_if_not_installed("tibble")
  x <- tibble::tribble(~target, ~command, "nothing", 1)
  expect_silent(check_plan(x, verbose = FALSE))
  expect_silent(make(x, verbose = FALSE, session_info = FALSE))
})

test_with_dir("plans can have bad symbols", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  x <- tibble::tibble(
    target = c("a'x'", "b'x'", "_a", "a^", "a*", "a-"),
    command = 1)
  y <- drake_config(x)
  expect_equal(y$plan$target, c("a.x.", "b.x.", "X_a", "a."))
})

test_with_dir("issue 187 on Github (from Kendon Bell)", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  test <- drake_plan(test = run_it(wc__))
  out <- evaluate_plan(test, rules = list(wc__ = list(1:4, 5:8, 9:12)))
  out2 <- tibble::tibble(
    target = c("test_1.4", "test_5.8", "test_9.12"),
    command = c("run_it(1:4)", "run_it(5:8)", "run_it(9:12)")
  )
  expect_equal(out, out2)
})

test_with_dir("file names with weird characters do not get mangled", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  out <- tibble::tibble(
    target = c("\"is:a:file\"", "not:a:file"),
    command = as.character(1:2)
  )
  out2 <- sanitize_plan(out)
  out3 <- tibble::tibble(
    target = c("\"is:a:file\"", "not.a.file"),
    command = as.character(1:2)
  )
  expect_equal(out[1, ], out2[1, ])
  expect_false(identical(out[2, ], out2[2, ]))
  expect_equal(out2, out3)
})

test_with_dir("can use semicolons for multi-line commands", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan <- drake_plan(list = c(x = "a<-1; a", y = "b<-2\nb"))
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

test_with_dir("ignore() suppresses updates", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  cache <- storr::storr_environment()
  envir <- new.env(parent = globalenv())
  envir$arg <- 4

  # Without ignore()
  con <- make(
    plan = drake_plan(x = sqrt(arg)),
    envir = envir,
    cache = cache
  )
  expect_equal(justbuilt(con), "x")
  con$envir$arg <- con$envir$arg + 1
  con <- make_with_config(con)
  expect_equal(justbuilt(con), "x")

  # With ignore()
  con <- make(
    plan = drake_plan(x = sqrt( ignore(arg) + 123)),
    envir = envir,
    cache = cache
  )
  expect_equal(justbuilt(con), "x")
  con$envir$arg <- con$envir$arg + 1
  con <- make_with_config(con)
  expect_equal(justbuilt(con), character(0))

  con$envir$arg2 <- con$envir$arg + 1234
  con$plan <- drake_plan(x = sqrt( ignore  (arg2 ) + 123))
  con <- make_with_config(con)
  expect_equal(justbuilt(con), character(0))
})

test_with_dir("ignore() works on its own", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(ignore(), NULL)
  expect_equal(ignore(1234), 1234)
  expect_identical(ignore_ignore(digest::digest), digest::digest)
})

test_with_dir("standardized commands with ignore()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(standardize_command("sqrt(arg)"), "{\n sqrt(arg) \n}")
  expect_equal(
    standardize_command("f(sqrt( ignore(fun(arg) + 7) + 123))"),
    "{\n f(sqrt(ignore() + 123)) \n}"
  )
  expect_equal(
    standardize_command("f(sqrt( ignore  (fun(arg) + 7) + 123) )"),
    "{\n f(sqrt(ignore() + 123)) \n}"
  )
  expect_equal(
    standardize_command(" f (sqrt( drake::ignore(fun(arg) + 7) + 123 ))"),
    "{\n f(sqrt(ignore() + 123)) \n}"
  )
  expect_equal(
    standardize_command("\tf(sqrt( drake ::: ignore  (fun(arg) + 7) + 123))"),
    "{\n f(sqrt(ignore() + 123)) \n}"
  )
  expect_equal(
    standardize_command("function(x){(sqrt( ignore(fun(arg) + 7) + 123))}"),
    "{\n function(x) {\n    (sqrt(ignore() + 123))\n} \n}"
  )
  f <- function(x){
    (sqrt( ignore(fun(arg) + 7) + 123))
  }
  b <- body(ignore_ignore(f))
  for (a in names(attributes(b))){
    attr(b, a) <- NULL
  }
  expect_equal(b, quote({  (sqrt(ignore() + 123)) })) # nolint
})

test_with_dir("ignore() in imported functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  f <- function(x){
    (sqrt( ignore(sqrt(x) + 7) + 123))
  }
  plan <- drake_plan(x = f(1))
  cache <- storr::storr_environment()
  config <- make(plan, cache = cache)
  expect_equal(justbuilt(config), "x")
  expect_equal(readd(f, cache = cache), f)
  expect_equal(
    unname(readd(f, cache = cache, namespace = "kernels")[3]),
    "    (sqrt(ignore() + 123))"
  )
  f <- function(x){
    (sqrt( ignore(sqrt(x) + 8) + 123))
  }
  config <- make(plan, cache = cache)
  expect_equal(justbuilt(config), character(0))
  f <- function(x){
    (sqrt( ignore(sqrt(x) + 8) + 124))
  }
  config <- make(plan, cache = cache)
  expect_equal(justbuilt(config), "x")
})

test_with_dir("custom column interface", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  tidyvar <- 2
  x <- target(
    stop(!!tidyvar), worker = !!tidyvar, cpu = 4, custom = stop(), c2 = 5)
  y <- tibble::tibble(
    command = "stop(2)",
    worker = 2,
    cpu = 4,
    custom = "stop()",
    c2 = 5
  )
  expect_equal(x, y)
  x <- drake_plan(x = target(
    stop(!!tidyvar), worker = !!tidyvar, cpu = 4, custom = stop(), c2 = 5))
  y <- tibble::tibble(
    target = "x",
    command = "stop(2)",
    worker = 2,
    cpu = 4,
    custom = "stop()",
    c2 = 5
  )
  expect_equal(x, y)
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
    z = rnorm(10),
    strings_in_dots = "literals"
  )
  plan0 <- tibble::tibble(
    target = c("x", "y", "z"),
    command = c("1 + 2", "Sys.sleep(\"not a number\")", "rnorm(10)"),
    trigger = c("trigger(condition = TRUE)", NA, NA),
    user_column_1 = c(1, NA, NA),
    user_column_2 = c("some text", NA, NA),
    col3 = c(NA, "some text", NA)
  )
  cn <- colnames(plan)
  expect_equal(cn, colnames(plan0))
  expect_equal(plan[, cn], plan0[, cn])
})

test_with_dir("bind_plans()", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  plan1 <- drake_plan(x = 1, y = 2)
  plan2 <- drake_plan(
    z = target(
      command = download_data(),
      trigger = trigger(condition = TRUE)
    ),
    strings_in_dots = "literals"
  )
  plan3 <- bind_plans(plan1, plan2)
  plan4 <- tibble::tibble(
    target = c("x", "y", "z"),
    command = c("1", "2", "download_data()"),
    trigger = c(NA, NA, "trigger(condition = TRUE)")
  )
  expect_equal(plan3, plan4)
})

test_with_dir("spaces in target names are replaced only when appropriate", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  pl <- drake_plan(a = x__, file_out("x__")) %>%
    evaluate_plan(wildcard = "x__", values = c("b  \n  x y", "a x"))
  pl2 <- tibble::tibble(
    target = c(
      "a_b.....x.y", "a_a.x",
      "drake_target_1_b.....x.y", "drake_target_1_a.x"),
    command = c(
      "b  \n  x y", "a x", "file_out(\"b  \n  x y\")", "file_out(\"a x\")"
    )
  )
  expect_equal(pl, pl2)
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
    z = target("any", cpu = "any"),
    strings_in_dots = "literals"
  )
  expect_error(
    evaluate_plan(plan, wildcard = "any", values = 1:2, columns = "target"),
    regexp = "argument of evaluate_plan"
  )
  expect_error(
    evaluate_plan(plan, wildcard = "any", values = 1:2, columns = "nobodyhere"),
    regexp = "not in the plan"
  )
  expect_equal(
    plan,
    evaluate_plan(plan, wildcard = "any", values = 1:2, columns = NULL)
  )
})

test_with_dir("'columns' argument to evaluate_plan()", {
  plan <- drake_plan(
    x = target("always", cpu = "any"),
    y = target("any", cpu = "always"),
    z = target("any", cpu = "any"),
    strings_in_dots = "literals"
  )
  out <- tibble::tibble(
    target = c("x_1", "x_2", "y_1", "y_2", "z"),
    command = c(1, 2, rep("any", 3)),
    cpu = c("any", "any", 1, 2, "any")
  )
  expect_equal(
    evaluate_plan(
      plan, wildcard = "always", values = 1:2, columns = c("command", "cpu")
    ),
    out
  )
  out <- tibble::tibble(
    target = c("x", "y_1", "y_2", "z"),
    command = c("always", rep("any", 3)),
    cpu = c("any", 1, 2, "any")
  )
  expect_equal(
    evaluate_plan(
      plan, wildcard = "always", values = 1:2, columns = "cpu"
    ),
    out
  )
  out <- tibble::tibble(
    target = c("x", "y", "z"),
    command = c(1, rep("any", 2)),
    cpu = c("any", 2, "any")
  )
  expect_equal(
    evaluate_plan(
      plan, wildcard = "always", values = 1:2, columns = c("command", "cpu"),
      expand = FALSE
    ),
    out
  )
  rules <- list(always = 1:2, any = 3:4)
  out <- tibble::tibble(
    target = c(
      "x_1_3", "x_1_4", "x_2_3", "x_2_4", "y_1_3",
      "y_1_4", "y_2_3", "y_2_4", "z_3", "z_4"
    ),
    command = as.character(c(1, 1, 2, 2, 3, 4, 3, 4, 3, 4)),
    cpu = as.character(c(3, 4, 3, 4, 1, 1, 2, 2, 3, 4))
  )
  expect_equal(
    evaluate_plan(plan, rules = rules, columns = c("command", "cpu")),
    out
  )
})

test_with_dir("drake_plan_call() produces the correct calls", {
  skip_on_cran()
  load_mtcars_example()
  my_plan$trigger <- NA
  my_plan$trigger[4] <- "trigger(condition = is_tuesday(), file = FALSE)"
  my_plan$non_standard_column <- 1234
  pkgconfig::set_config("drake::strings_in_dots" = "literals")
  new_plan <- eval(drake_plan_call(my_plan))
  expected <- my_plan[, c("target", "command", "trigger")]
  expect_equal(new_plan, expected)
})

test_with_dir("drake_plan_source()", {
  skip_on_cran()
  skip_if_not_installed("styler")
  plan <- drake::drake_plan(
    small_data = download_data("https://some_website.com") %>%
      select_my_columns() %>%
      munge(),
    large_data_raw = target(
      command = download_data("https://lots_of_data.com") %>%
        select_top_columns(),
      trigger = trigger(
        change = time_last_modified("https://lots_of_data.com"),
        command = FALSE,
        depend = FALSE
      ),
      timeout = 1e3
    ),
    strings_in_dots = "literals"
  )
  x <- drake_plan_source(plan)
  y <- capture.output(print(x))
  expect_true(grepl("^drake_plan", x[1]))
  expect_true(grepl("^drake_plan", y[1]))
  writeLines(x, "script.R")
  plan2 <- source("script.R")$value
  expect_equal(plan, plan2)
})

test_with_dir("code_to_plan(), one target", {
  skip_on_cran()
  skip_if_not_installed("CodeDepends")
  writeLines("a <- 1", "script.R")
  plan <- code_to_plan("script.R")
  expect_equal(plan, tibble(target = "a", command = "1"))
})

test_with_dir("code_to_plan(), multiple targets", {
  skip_on_cran()
  skip_if_not_installed("CodeDepends")
  skip_if_not_installed("downloader")
  drake_example("code_to_plan")
  plan <- code_to_plan("code_to_plan/script.R")
  plan2 <- code_to_plan("code_to_plan/report.Rmd")
  expect_equal(plan, plan2)
  expect_equal(
    plan$target,
    c("data1", "data2", "summary1", "summary2", "discrepancy", "sum1", "sum2")
  )
  expect_equal(
    plan$command[1:4],
    c("rnorm(10)", "rnorm(20)", "mean(data1)", "median(data2)")
  )
  expect_equal(sum(!is.na(plan$trigger)), 1)
  expect_equal(plan$timeout, c(NA, NA, NA, NA, 100, NA, NA))
  config <- make(
    plan, cache = storr::storr_environment(), session_info = FALSE)
  config <- make(config = config)
  expect_equal(justbuilt(config), "discrepancy")
  expect_true(
    is.numeric(readd(discrepancy, cache = config$cache)))
})
