drake_context <- function(x) {
  assert_pkg("testthat")
  ctx <- paste0(get_testing_scenario_name(), ": ", x)
  testthat::context(ctx)
}

test_with_dir <- function(desc, ...) {
  assert_pkg("testthat")
  old <- Sys.getenv("drake_warn_subdir")
  Sys.setenv(drake_warn_subdir = "false")
  Sys.setenv(drake_session_info = "false")
  on.exit(Sys.setenv(drake_warn_subdir = old))
  while (file.exists(new <- tempfile())) {
    # Should not reach this part of the loop.
    Sys.sleep(0.01) # nocov
  }
  dir.create(new)
  with_dir(new, {
    opts <- list(
      clustermq.scheduler = "multicore",
      drake_clean_menu = FALSE
    )
    with_options(new = opts, {
      set_test_backend()
      testthat::test_that(desc = desc, ...)
    })
  })
  invisible()
}

# From withr https://github.com/r-lib/withr, copyright RStudio, GPL (>=2)
with_dir <- function(new, code) {
  old <- setwd(new) # nolint
  on.exit(setwd(old)) # nolint
  force(code)
}
