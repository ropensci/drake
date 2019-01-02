drake_context("knitr")

test_with_dir("codeless knitr report", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  expect_equal(
    deps_code(quote(knitr_in("codeless.Rmd"))),
    list(knitr_in = file)
  )
  expect_silent(
    tmp <- make(
      drake_plan(x = knitr_in("codeless.Rmd")),
      session_info = FALSE,
      cache = storr::storr_environment(),
      verbose = FALSE
    )
  )
})

test_with_dir("bad knitr report", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
    tmp <- make(
      drake_plan(
        x = knitr_in("bad.Rmd")
      ),
      session_info = FALSE,
      cache = storr::storr_environment(),
      verbose = FALSE
    ),
    regexp = "dependencies could not be extracted"
  )
})

test_with_dir("empty cases", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  expect_equal(safe_get_tangled_frags(NULL), character(0))
  expect_silent(tmp <- analyze_knitr_file(NULL, NULL))
})

test_with_dir("knitr_deps() works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  out <- knitr_deps("test.Rmd")
  expect_equal(sort(clean_dependency_list(out)), ans)
  expect_false(file.exists("test.md"))
  expect_warning(x <- sort(clean_dependency_list(knitr_deps("report.Rmd"))))
  expect_warning(expect_equal(x, sort(
    clean_dependency_list(knitr_deps(encode_path("report.Rmd"))))))
  expect_equal(x, character(0))
  load_mtcars_example()
  w <- clean_dependency_list(deps_code("funct(knitr_in(report.Rmd))"))
  x <- knitr_deps("report.Rmd")
  real_deps <- c(
    "small", "coef_regression2_small", "large"
  )
  expect_equal(sort(w), sort(c("funct")))
  expect_equal(sort(clean_dependency_list(x)), sort(real_deps))
})

test_with_dir("knitr file deps from commands and functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  load_mtcars_example()
  expect_equal(sort(
    clean_dependency_list(deps_code("knitr_in(\"report.Rmd\")"))), sort(c(
    "coef_regression2_small", "large", "small", "report.Rmd"
  )))
  f <- function(x) {
    knit(x)
  }
  expect_equal(clean_dependency_list(deps_code(f)), "knit")
})
