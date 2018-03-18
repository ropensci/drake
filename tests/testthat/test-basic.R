drake_context("basic")

test_with_dir("basic example works", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  caching <- scenario$caching

  load_basic_example(envir = e)
  my_plan <- e$my_plan
  config <- drake_config(my_plan, envir = e,
    jobs = jobs, parallelism = parallelism,
    verbose = FALSE, caching = caching)

  expect_equal(max_useful_jobs(config), 8)
  expect_false(file.exists("Makefile"))
  expect_equal(max_useful_jobs(imports = "files",
    config = config), 8)
  expect_true(max_useful_jobs(imports = "all",
    config = config) >= 8)
  expect_equal(max_useful_jobs(imports = "none",
    config = config), 8)

  dats <- c("small", "large")
  config$targets <- dats
  con <- testrun(config)

  expect_true(is.list(dependency_profile(
    target = "small", config = con)))
  expect_equal(parallelism == "Makefile", file.exists("Makefile"))

  # Should probably check the actual build times in the labels.
  tmp1 <- dataframes_graph(config = config,
    make_imports = FALSE)
  tmp2 <- dataframes_graph(config = config)
  expect_true(is.data.frame(tmp1$nodes))
  expect_true(is.data.frame(tmp2$nodes))

  expect_equal(sort(justbuilt(con)), sort(dats))
  remove_these <- intersect(dats, ls(config$envir))
  rm(list = remove_these, envir = config$envir)
  config$targets <- config$plan$target
  con <- testrun(config)
  jb <- justbuilt(con)
  expect_true("\"report.md\"" %in% jb)
  expect_false(any(dats %in% jb))

  # Check that file is not rehashed.
  # Code coverage should cover every line of file_hash().
  expect_true(is.character(file_hash(
    target = "\"report.Rmd\"", config = con, size_cutoff = -1)))

  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)

  expect_equal(outdated(config), character(0))
  expect_equal(max_useful_jobs(config = config), 1)
  expect_equal(max_useful_jobs(imports = "files",
    config = config), 1)
  expect_true(max_useful_jobs(imports = "all",
    config = config) >= 8)
  expect_equal(max_useful_jobs(imports = "none",
    config = config), 0)

  e$reg2 <- function(d) {
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(
    sort(outdated(config = config)),
    sort(c("\"report.md\"", "coef_regression2_large",
      "coef_regression2_small", "regression2_large", "regression2_small",
      "summ_regression2_large", "summ_regression2_small")))
  expect_equal(max_useful_jobs(config = config), 4)
  expect_equal(max_useful_jobs(config = config,
    from_scratch = TRUE), 8)
  expect_equal(max_useful_jobs(imports = "files",
    config = config), 4)
  expect_true(max_useful_jobs(imports = "all",
    config = config) >= 8)
  expect_equal(max_useful_jobs(imports = "none",
    config = config), 4)

  testrun(config)
  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(sort(outdated(config = config)),
    character(0))

  # Take this opportunity to test tidyselect API. Saves test time that way.
  # loadd() # nolint
  e <- new.env(parent = globalenv())
  coefs <- sort(c("coef_regression1_large", "coef_regression1_small",
             "coef_regression2_large", "coef_regression2_small"))
  loadd(starts_with("coef"), envir = e)
  expect_equal(sort(ls(envir = e)), coefs)

  # build_times() # nolint
  all_times <- build_times()
  expect_true(nrow(all_times) >= nrow(config$plan))
  some_times <- build_times(starts_with("coef"))
  expect_equal(sort(some_times$item), coefs)

  # clean() # nolint
  x <- sort(cached())
  expect_true(all(coefs %in% x))
  clean(starts_with("coef"))
  expect_equal(sort(cached()), setdiff(x, coefs))

  # knitr file deps
  # Included here instead of test-knitr.R because report.md already exists.
  # Saves time that way.
  # But we can remove it all when we deprecate the single-quoted stuff
  # and users know to rely on knitr_input().
  x <- drake_plan(
    a = knitr::knit(knitr_in("report.Rmd")), # nolint
    b = knitr::knit(knitr_in("report.md")), # nolint
    c = knitr::knit("nonfile"),
    d = rmarkdown::render("report.Rmd"), # nolint
    e = rmarkdown::render("report.md"), # nolint
    f = rmarkdown::render("nonfile"),
    strings_in_dots = "literals"
  )
  suppressWarnings(con <- drake_config(plan = x))
  for (target in c("a", "d")){
    expect_true("small" %in% dependencies(targets = target, config = con))
  }
  for (target in c("b", "c", "e", "f")){
    expect_false("small" %in% dependencies(targets = target, config = con))
  }
})
