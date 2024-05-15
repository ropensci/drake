drake_context("files")

test_with_dir("responses to imported file", {
  config <- dbug()
  con2 <- drake_config(plan = config$plan[-1, ], envir = config$envir)
  expect_warning(runtime_checks(con2))
  testrun(config)
  expect_true(length(justbuilt(config)) > 0)
  testrun(config)
  nobuild(config)

  # check missing and then replace file exactly as before
  contents <- readRDS("input.rds")
  unlink("input.rds", force = TRUE)
  con3 <- drake_config(plan = config$plan, envir = config$envir)
  expect_warning(tmp <- runtime_checks(con3))
  saveRDS(contents, "input.rds")
  testrun(config)
  nobuild(config)
  final0 <- readd(final)

  # actually change file
  saveRDS(2:10, "input.rds")
  testrun(config)
  expect_equal(justbuilt(config), sort(c(
    "drake_target_1", "combined", "final", "myinput", "nextone")))
  expect_false(length(final0) == length(readd(final)))
})

test_with_dir("same with an imported directory", {
  skip_on_cran()
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  dir.create("inputdir")
  tmp <- file.copy("input.rds", "inputdir/input.rds")
  tmp <- file.remove("input.rds")
  plan <- dbug_plan()
  plan$command[plan$target == "myinput"][[1]] <- quote(
    readRDS(file.path(file_in("inputdir"), "input.rds"))
  )
  config <- drake_config(
    plan = plan,
    targets = plan$target,
    envir = envir,
    parallelism = scenario$parallelism,
    jobs = scenario$jobs,
    verbose = 0L,
    session_info = FALSE,
    log_progress = TRUE,
    caching = scenario$caching
  )
  config$plan <- plan
  testrun(config)
  final0 <- readd(final)

  # add another file to the directory
  saveRDS(2:10, "inputdir/otherinput.rds")
  testrun(config)
  expect_equal(justbuilt(config), "myinput")
  expect_equal(length(final0), length(readd(final)))

  # change the real input file
  saveRDS(2:10, "inputdir/input.rds")
  testrun(config)
  expect_equal(justbuilt(config), sort(c(
    "drake_target_1", "combined", "final", "myinput", "nextone")))
  expect_false(length(final0) == length(readd(final)))
})

test_with_dir("drake_config() memoizes against knitr files (#887)", {
  skip_on_cran()
  skip_if_not_installed("knitr")

  # Setup
  plan <- drake_plan(
    a = TRUE,
    b = TRUE,
    report_step = knitr_in("report1.Rmd", "report2.Rmd")
  )
  lines_a <- c(
    "---",
    "title: abc",
    "---",
    "",
    "```{r}",
    "readd(a)",
    "```"
  )
  lines_b <- c(
    "---",
    "title: abc",
    "---",
    "",
    "```{r}",
    "readd(b)",
    "```"
  )
  writeLines(lines_a, "report1.Rmd")
  writeLines(lines_a, "report2.Rmd")
  envir <- new.env(parent = globalenv())
  cache <- storr::storr_environment()
  for (i in 1:2) {
    config <- drake_config(
      plan,
      envir = envir,
      cache = cache,
      session_info = FALSE
    )
  }

  # Now switch `a` to `b` in the report.
  writeLines(lines_b, "report2.Rmd")
  config <- drake_config(
    plan,
    envir = envir,
    cache = cache,
    session_info = FALSE
  )
  deps <- deps_target_impl(report_step, config)
  expect_true("a" %in% deps$name)
  expect_true("b" %in% deps$name)

  # make() first so file times and hashes are in the cache.
  make_impl(config = config)
  writeLines(lines_b, "report1.Rmd")
  config <- drake_config(
    plan,
    envir = envir,
    cache = cache,
    session_info = FALSE
  )
  deps <- deps_target_impl(report_step, config)
  expect_false("a" %in% deps$name)
  expect_true("b" %in% deps$name)

  # check if things work if a knitr file is missing.
  unlink("report1.Rmd")
  expect_warning(
    config <- drake_config(
      plan,
      envir = envir,
      cache = cache,
      session_info = FALSE
    ),
    regexp = "Could not open"
  )
})

test_with_dir("good URL with an ETag", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("curl")
  url <- "https://raw.githubusercontent.com/ropensci/drake/932afcb4050f18351e1e25be3644d7ddb5c903a8/DESCRIPTION" # nolint
  tryCatch(
    mem <- curl::curl_fetch_memory(url),
    error = function(e) {
      skip("test URL unreachable")
    }
  )
  if (mem$status_code != 200) {
    skip("test URL unreachable")
  }
  plan <- drake_plan(
    x = file_in("https://raw.githubusercontent.com/ropensci/drake/932afcb4050f18351e1e25be3644d7ddb5c903a8/DESCRIPTION") # nolint
  )
  config <- drake_config(
    plan,
    cache = storr::storr_environment(),
    session_info = FALSE,
    log_progress = TRUE
  )
  make_impl(config = config)
  expect_equal(justbuilt(config), "x")
  etag <- config$cache$get(
    file_store("https://raw.githubusercontent.com/ropensci/drake/932afcb4050f18351e1e25be3644d7ddb5c903a8/DESCRIPTION") # nolint
  )
  expect_true(nzchar(etag))
  expect_equal(outdated_impl(config), character(0))
  make_impl(config = config)
  expect_equal(justbuilt(config), character(0))
})

test_with_dir("good URL with a timestamp", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("curl")
  url <- "https://nytimes.com"
  tryCatch(
    mem <- curl::curl_fetch_memory(url),
    error = function(e) {
      skip("test URL unreachable")
    }
  )
  if (mem$status_code != 200) {
    skip("test URL unreachable")
  }
  plan <- drake_plan(x = file_in("https://nytimes.com"))
  config <- drake_config(
    plan,
    cache = storr::storr_environment(),
    session_info = FALSE,
    log_progress = TRUE
  )
  make_impl(config = config)
  expect_equal(justbuilt(config), "x")
  mtime <- config$cache$get(file_store("https://nytimes.com"))
  expect_true(nzchar(mtime))
})

test_with_dir("bad URL", {
  skip("unreliable test")
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("curl")
  plan <- drake_plan(
    x = file_in("https://aklsdjflkjsiofjlekjsiolkjiufhalskdjf")
  )
  config <- drake_config(
    plan,
    cache = storr::storr_environment(),
    session_info = FALSE,
    log_progress = TRUE
  )
  tryCatch(
    mem <- curl::curl_fetch_memory("http://httpbin.org/basic-auth/user/passwd"),
    error = function(e) {
      skip("test URL unreachable")
    }
  )
  expect_error(
    make_impl(config = config),
    "could not access url|resolve host|HTTP code 407"
  )
  expect_equal(justbuilt(config), character(0))
  expect_error(
    make_impl(config = config),
    "could not access url|resolve host|HTTP code 407"
  )
  expect_equal(justbuilt(config), character(0))
})

test_with_dir("authentication", {
  skip_on_cran()
  skip_if_offline()
  skip_if_not_installed("curl")
  url <- "http://httpbin.org/basic-auth/user/passwd" # nolint
  tryCatch(
    mem <- curl::curl_fetch_memory(url),
    error = function(e) {
      skip("test URL unreachable")
    }
  )
  plan <- drake_plan(x = file_in("http://httpbin.org/basic-auth/user/passwd"))
  expect_error(make(plan), regexp = "could not access url")
  handles <- list(`http://httpbin.org/basic-auth` = curl::new_handle())
  tryCatch(
    mem <- curl::curl_fetch_memory("http://httpbin.org/basic-auth/user/passwd"),
    error = function(e) {
      skip("test URL unreachable")
    }
  )
  expect_error(
    make(plan, curl_handles = handles),
    regexp = "could not access url"
  )
  # Make sure we get the most specific URL.
  handles <- list(
    `http://httpbin.org/basic-auth` = curl::new_handle(),
    `http://httpbin.org/basic-auth/user` = curl::new_handle(),
    `http://WRONG` = curl::new_handle()
  )
  handles[[2]] <- curl::handle_setopt(
    handles[[2]],
    username = "user",
    password = "passwd"
  )
  expect_error(
    make(plan, curl_handles = handles),
    regexp = "no ETag or Last-Modified for url|code 407|could not access url"
  )
})

test_with_dir("assert_useful_headers()", {
  skip_on_cran()
  expect_error(
    assert_useful_headers(list(), "xyz"),
    regexp = "no ETag or Last-Modified for url"
  )
})

test_with_dir("responses to intermediate file", {
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  plan <- dbug_plan()
  plan$command[[6]] <- quote({
    readRDS(file_in("intermediatefile.rds")) +
    readRDS(file_in("out2.rds"))
  })
  command1 <- quote({
    saveRDS(combined, file_out("intermediatefile.rds"))
    saveRDS(combined + 1, file_out("out2.rds"))
  })
  command2 <- quote({
    saveRDS(combined, "intermediatefile.rds")
    saveRDS(combined + 1, "out2.rds")
    file_out("intermediatefile.rds", "out2.rds")
  })
  for (command in c(command1, command2)) {
    plan$command[[1]] <- command
    config <- drake_config(plan = plan, targets = plan$target,
      envir = envir, parallelism = scenario$parallelism,
      jobs = scenario$jobs, verbose = 1L,
      session_info = FALSE,
      log_progress = TRUE,
      caching = scenario$caching
    )
    config$plan <- plan
    testrun(config)
    expect_equal(justbuilt(config), sort(config$plan$target))
    expect_equal(outdated_impl(config), character(0))
    final0 <- readd(final)
    val <- readRDS("intermediatefile.rds")
    val2 <- readRDS("out2.rds")
    expect_equal(val + 1, val2)

    # actually change a file
    for (file in c("intermediatefile.rds", "out2.rds")) {
      saveRDS(sum(val) + 100, file)
      testrun(config)
      expect_equal(justbuilt(config), "drake_target_1")
      expect_equal(final0, readd(final))
      expect_equal(val, readRDS("intermediatefile.rds"))
      expect_equal(val2, readRDS("out2.rds"))
    }
    # break a file
    for (file in c("intermediatefile.rds", "out2.rds")) {
      unlink(file, force = TRUE)
      testrun(config)
      expect_equal(justbuilt(config), "drake_target_1")
      expect_equal(final0, readd(final))
      expect_equal(val, readRDS("intermediatefile.rds"))
      expect_equal(val2, readRDS("out2.rds"))
    }

    # change what intermediatefile.rds is supposed to be
    cmd <- safe_deparse(config$plan$command[[1]])
    cmd <- gsub("combined,", "combined + 5,", cmd)
    config$plan$command[[1]] <- parse(text = cmd)[[1]]
    testrun(config)
    expect_equal(
      sort(justbuilt(config)),
      sort(c("drake_target_1", "final"))
    )
    expect_equal(final0 + 5, readd(final))
    expect_equal(val + 5, readRDS("intermediatefile.rds"))
    expect_equal(val2, readRDS("out2.rds"))

    # change what out2.rds is supposed to be
    cmd <- safe_deparse(config$plan$command[[1]])
    cmd <- gsub("1", "2", cmd)
    config$plan$command[[1]] <- parse(text = cmd)[[1]]
    testrun(config)
    expect_equal(
      sort(justbuilt(config)),
      sort(c("drake_target_1", "final"))
    )
    expect_equal(final0 + 6, readd(final))
    expect_equal(val + 5, readRDS("intermediatefile.rds"))
    expect_equal(val2 + 1, readRDS("out2.rds"))
    clean(destroy = TRUE)
  }
})

test_with_dir("same with a directory", {
  skip_on_cran()
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  dbug_files()
  dir.create("scratch")
  plan <- dbug_plan()
  plan$command[[1]] <- quote({
    file_out("scratch")
    saveRDS(combined, "scratch/intermediatefile.rds")
    saveRDS(combined + 1, "scratch/out2.rds")
  })
  plan$command[[6]] <- quote({
    file_in("scratch")
    readRDS("scratch/intermediatefile.rds") +
      readRDS("scratch/out2.rds")
  })
  config <- drake_config(
    plan = plan,
    targets = plan$target,
    envir = envir,
    parallelism = scenario$parallelism,
    jobs = scenario$jobs,
    verbose = 1L,
    session_info = FALSE,
    log_progress = TRUE,
    caching = scenario$caching
  )
  config$plan <- plan
  testrun(config)
  expect_equal(justbuilt(config), sort(config$plan$target))
  expect_equal(outdated_impl(config), character(0))
  final0 <- readd(final)
  val <- readRDS("scratch/intermediatefile.rds")
  val2 <- readRDS("scratch/out2.rds")
  expect_equal(as.integer(val) + 1L, as.integer(val2))

  # actually change a file
  for (file in c("scratch/intermediatefile.rds", "scratch/out2.rds")) {
    saveRDS(sum(val) + 100, file)
    testrun(config)
    expect_equal(justbuilt(config), "drake_target_1")
    expect_equal(final0, readd(final))
    expect_equal(val, readRDS("scratch/intermediatefile.rds"))
    expect_equal(val2, readRDS("scratch/out2.rds"))
  }

  # break a file
  for (file in c("scratch/intermediatefile.rds", "scratch/out2.rds")) {
    unlink(file, force = TRUE)
    testrun(config)
    expect_equal(justbuilt(config), "drake_target_1")
    expect_equal(final0, readd(final))
    expect_equal(val, readRDS("scratch/intermediatefile.rds"))
    expect_equal(val2, readRDS("scratch/out2.rds"))
  }

  # change what intermediatefile.rds is supposed to be
  cmd <- safe_deparse(config$plan$command[[1]])
  cmd <- gsub("combined,", "combined + 5,", cmd)
  config$plan$command[[1]] <- parse(text = cmd)[[1]]
  testrun(config)
  expect_equal(
    sort(justbuilt(config)),
    sort(c("drake_target_1", "final"))
  )
  expect_equal(final0 + 5, readd(final))
  expect_equal(val + 5, readRDS("scratch/intermediatefile.rds"))
  expect_equal(val2, readRDS("scratch/out2.rds"))

  # change what out2.rds is supposed to be
  cmd <- safe_deparse(config$plan$command[[1]])
  cmd <- gsub("1", "2", cmd)
  config$plan$command[[1]] <- parse(text = cmd)[[1]]
  testrun(config)
  expect_equal(
    sort(justbuilt(config)),
    sort(c("drake_target_1", "final"))
  )
  expect_equal(final0 + 6, readd(final))
  expect_equal(val + 5, readRDS("scratch/intermediatefile.rds"))
  expect_equal(val2 + 1, readRDS("scratch/out2.rds"))
  clean(destroy = TRUE)
})


test_with_dir("imported files in imported functions", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  scenario <- get_testing_scenario()
  envir <- eval(parse(text = scenario$envir))
  envir <- dbug_envir(envir)
  eval(
    parse(text = "j <- function(x) {
      file_in(\"a.rds\", \"b.rds\")
      x + 2 + c + readRDS(file_in(\"c.rds\"))
    }"),
    envir = envir
  )
  dbug_files()
  for (file in paste0(letters[1:3], ".rds")) {
    saveRDS(1, file)
  }
  load_mtcars_example() # for report.Rmd
  config <- drake_config(dbug_plan(), envir = envir, verbose = 0L)
  config$plan <- dbug_plan()
  testrun(config)
  for (file in paste0(letters[1:2], ".rds")) {
    saveRDS(2, file)
    testrun(config)
    expect_equal(sort(justbuilt(config)), sort(c("nextone", "yourinput")))
  }
  saveRDS(129837, file = "a.rds")
  testrun(config)
  expect_equal(sort(justbuilt(config)), sort(c("nextone", "yourinput")))
  saveRDS(2, "c.rds")
  testrun(config)
  expect_equal(sort(justbuilt(config)), sort(c(
    "nextone", "yourinput", "combined", "drake_target_1", "final"
  )))
})

test_with_dir("codeless knitr report", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("knitr")
  file <- "codeless.Rmd"
  path <- system.file(
    file.path("testing", "knitr", file),
    package = "drake", mustWork = TRUE
  )
  expect_true(file.copy(
    from = path,
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ))
  expect_true(file.exists(file))
  deps <- deps_code(quote(knitr_in("codeless.Rmd")))
  expect_equal(deps$name, "codeless.Rmd")
  expect_equal(deps$type, "knitr_in")
  expect_silent(
    make(
      drake_plan(x = knitr_in("codeless.Rmd")),
      session_info = FALSE,
      cache = storr::storr_environment(),
      verbose = 0L
    )
  )
})

test_with_dir("bad knitr report", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  file <- "bad.Rmd"
  path <- system.file(
    file.path("testing", "knitr", file),
    package = "drake", mustWork = TRUE
  )
  expect_true(file.copy(
    from = path,
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  ))
  expect_true(file.exists(file))
  expect_warning(deps_code(quote(knitr_in("bad.Rmd"))))
  expect_warning(
    make(
      drake_plan(
        x = knitr_in("bad.Rmd")
      ),
      session_info = FALSE,
      cache = storr::storr_environment(),
      verbose = 0L
    ),
    regexp = "Could not parse"
  )
})

test_with_dir("empty cases", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  expect_equal(get_tangled_frags(NULL), character(0))
  expect_silent(tmp <- analyze_knitr_file(NULL, NULL))
})

test_with_dir("deps_knitr() works", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("knitr")
  expect_true(!nrow(deps_knitr(character(0))))
  files <- system.file(
    file.path("testing", "knitr", c("nested.Rmd", "test.Rmd")),
    package = "drake", mustWork = TRUE
  )
  expect_true(all(file.copy(
    from = files,
    to = getwd(),
    recursive = TRUE,
    overwrite = TRUE
  )))
  ans <- sort(c(
    "inline_dep", paste0("target", seq_len(18)),
    paste0("\"file", seq_len(6), "\""),
    "input.txt", "output.txt", "nested.Rmd", "nested"
  ))
  out <- deps_knitr("test.Rmd")
  out2 <- deps_knitr(file_store("test.Rmd"))
  expect_equal(out, out2)
  expect_equal(sort(out$name), ans)
  expect_false(file.exists("test.md"))
  expect_warning(x <- deps_knitr("report.Rmd"))
  expect_warning(expect_equal(x$name, sort(
    deps_knitr(reencode_path("report.Rmd"))$name)))
  expect_true(!nrow(x))
  load_mtcars_example()
  expect_warning(
    w <- deps_code("funct(knitr_in(report.Rmd))"),
    regexp = "must be literal strings"
  )
  x <- deps_knitr("report.Rmd")
  real_deps <- c(
    "small", "coef_regression2_small", "large"
  )
  expect_equal(sort(w$name), sort(c("funct")))
  expect_equal(sort(x$name), sort(real_deps))
})

test_with_dir("knitr file deps from commands and functions", {
  skip_on_cran() # CRAN gets essential tests only (check time limits).
  skip_if_not_installed("knitr")
  load_mtcars_example()
  expect_equal(
    sort(deps_code("knitr_in(\"report.Rmd\")")$name),
    sort(c("coef_regression2_small", "large", "small", "report.Rmd"))
  )
  f <- function(x) {
    knit(x)
  }
  expect_equal(deps_code(f)$name, "knit")
})
