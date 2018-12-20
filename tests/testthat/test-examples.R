drake_context("examples")

test_with_dir("examples are listed and written", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("downloader")
  x <- drake_examples()
  expect_true(is.character(x) & length(x) > 0)
  example <- "main"
  expect_false(file.exists(example))
  drake_example(example)
  expect_true(file.exists(example))
  expect_true(file.info(example)$isdir)
  unlink(example, recursive = TRUE, force = TRUE)
  expect_warning(drake_example(example, destination = getwd()))
})

test_with_dir("example template files", {
  expect_true(is.character(drake_hpc_template_files()))
  expect_true(length(drake_hpc_template_files()) > 0)
  expect_false(file.exists("slurm_batchtools.tmpl"))
  expect_silent(drake_hpc_template_file("slurm_batchtools.tmpl"))
  expect_true(file.exists("slurm_batchtools.tmpl"))
})

test_with_dir("mtcars example works", {
  scenario <- get_testing_scenario()
  e <- eval(parse(text = scenario$envir))
  jobs <- scenario$jobs
  parallelism <- scenario$parallelism
  caching <- scenario$caching

  load_mtcars_example(envir = e)
  expect_warning(
    load_mtcars_example(envir = e, overwrite = TRUE),
    regexp = "Overwriting"
  )
  my_plan <- e$my_plan
  config <- drake_config(my_plan, envir = e,
                         jobs = jobs, parallelism = parallelism,
                         verbose = FALSE, caching = caching)
  expect_false(file.exists("Makefile"))

  dats <- c("small", "large")
  config$targets <- dats
  con <- testrun(config)

  expect_true(is.list(dependency_profile(
    target = "small", config = con)))
  expect_equal(parallelism == "Makefile", file.exists("Makefile"))

  expect_equal(sort(justbuilt(con)), sort(dats))
  remove_these <- intersect(dats, ls(config$envir))
  rm(list = remove_these, envir = config$envir)
  config$targets <- config$plan$target
  con <- testrun(config)
  jb <- justbuilt(con)
  expect_true("report" %in% jb)
  expect_false(any(dats %in% jb))

  # Check that file is not rehashed.
  # Code coverage should cover every line of file_hash().
  expect_true(is.character(file_hash(
    target = reencode_path("report.Rmd"), config = con, size_cutoff = -1)))
  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(outdated(config), character(0))

  # Change an imported function
  e$reg2 <- function(d) {
    d$x3 <- d$x ^ 3
    lm(y ~ x3, data = d)
  }
  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  to_build <- sort(c(
    "report", "coef_regression2_large",
    "coef_regression2_small", "regression2_large", "regression2_small",
    "summ_regression2_large", "summ_regression2_small"
  ))
  expect_equal(sort(outdated(config = config)), to_build)
  testrun(config)
  expect_equal(sort(justbuilt(config)), to_build)
  config <- drake_config(
    my_plan, envir = e, jobs = jobs, parallelism = parallelism,
    verbose = FALSE)
  expect_equal(sort(outdated(config = config)), character(0))

  # Take this opportunity to test tidyselect API. Saves test time that way.
  # loadd() # nolint
  coefs <- sort(
    c(
      "coef_regression1_large",
      "coef_regression1_small",
      "coef_regression2_large",
      "coef_regression2_small"
    )
  )
  if (exists_tidyselect()) {
    e <- new.env(parent = globalenv())
    expect_error(loadd(not_a_target, envir = e))
    expect_equal(ls(envir = e), character(0))
    for (i in 1:2) {
      loadd(starts_with("coef"), envir = e)
      expect_equal(sort(ls(envir = e)), coefs)
      rm(list = coefs, envir = e)
    }

    # build_times() # nolint
    skip_if_not_installed("lubridate")
    all_times <- build_times()
    expect_true(nrow(all_times) >= nrow(config$plan))
    some_times <- build_times(starts_with("coef"))
    expect_equal(sort(some_times$item), coefs)

    # clean() # nolint
    x <- sort(cached())
    expect_true(all(coefs %in% x))
    clean(starts_with("coef"))
    expect_equal(sort(cached()), setdiff(x, coefs))
  }

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
  for (target in c("a")) {
    expect_true("small" %in% dependencies(targets = target, config = con))
  }
  for (target in c("b", "c")) {
    expect_false("small" %in% dependencies(targets = target, config = con))
  }

  # clean_mtcars_example() # nolint
  for (file in c(".drake", "report.Rmd")) {
    expect_true(file.exists(file))
  }
  clean_mtcars_example()
  for (file in c(".drake", "report.Rmd")) {
    expect_false(file.exists(file))
  }
})
