justbuilt <- function(config) {
  sort(drake_progress(progress = "done", cache = config$cache)$target)
}

nobuild <- function(config) {
  assert_pkg("testthat")
  testthat::expect_true(length(justbuilt(config)) < 1)
}

equivalent_plans <- function(out, exp) {
  assert_pkg("testthat")
  out <- deparse_lang_cols(out)
  exp <- deparse_lang_cols(exp)
  out <- out[order(out$target), ]
  exp <- exp[order(exp$target), ]
  for (col in lang_cols(out)) {
    testthat::expect_equal(
      unname(unclass(out[[col]])),
      unname(unclass(exp[[col]]))
    )
  }
  for (col in setdiff(colnames(out), lang_cols(out))) {
    testthat::expect_equal(
      unname(out[[col]]),
      unname(exp[[col]])
    )
  }
}
