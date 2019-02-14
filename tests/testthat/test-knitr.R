drake_context("knitr")

test_with_dir("codeless knitr report", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  expect_equal(deps$target, "codeless.Rmd")
  expect_equal(deps$type, "knitr_in")
  expect_silent(
    make(
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
    make(
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

test_with_dir("deps_knitr() works", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
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
  expect_equal(sort(out$target), ans)
  expect_false(file.exists("test.md"))
  expect_warning(x <- deps_knitr("report.Rmd"))
  expect_warning(expect_equal(x$target, sort(
    clean_dependency_list(deps_knitr(encode_path("report.Rmd"))))))
  expect_true(!nrow(x))
  load_mtcars_example()
  w <- deps_code("funct(knitr_in(report.Rmd))")
  x <- deps_knitr("report.Rmd")
  real_deps <- c(
    "small", "coef_regression2_small", "large"
  )
  expect_equal(sort(w$target), sort(c("funct")))
  expect_equal(sort(x$target), sort(real_deps))
})

test_with_dir("knitr file deps from commands and functions", {
  skip_on_cran() # CRAN gets whitelist tests only (check time limits).
  skip_if_not_installed("knitr")
  load_mtcars_example()
  expect_equal(
    sort(deps_code("knitr_in(\"report.Rmd\")")$target),
    sort(c("coef_regression2_small", "large", "small", "report.Rmd"))
  )
  f <- function(x) {
    knit(x)
  }
  expect_equal(deps_code(f)$target, "knit")
})
