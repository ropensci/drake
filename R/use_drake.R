#' @title Use drake in a project
#' `r lifecycle::badge("questioning")`
#' @description Add top-level R script files to use `drake`
#'   in your data analysis project. For details, read
#'   `https://books.ropensci.org/drake/projects.html`
#' @details Files written:
#'   1. `make.R`: a suggested main R script for batch mode.
#'   2. `_drake.R`: a configuration R script for
#'     the `r_*()` functions documented at # nolint
#'     `https://books.ropensci.org/drake/projects.html#safer-interactivity`. # nolint
#' Remarks:
#'   - There is nothing magical about the name, `make.R`.
#'     You can call it whatever you want.
#'   - Other supporting scripts, such as `R/packages.R`,
#'     `R/functions.R`, and `R/plan.R`, are not included.
#'   - You can find examples at
#'     `https://github.com/wlandau/drake-examples`
#'     and download examples with [`drake_example()`]
#'     (e.g. `drake_example("main")`).
#' @export
#' @param open Logical, whether to open `make.R` for editing.
#' @examples
#' \dontrun{
#' # use_drake(open = FALSE) # nolint
#' }
use_drake <- function(open = interactive()) {
  # Covered in tests/testthat/test-always-skipped.R.
  # Reason: https://github.com/r-lib/usethis/issues/347
  # nocov start
  assert_pkg("usethis")
  usethis::use_directory("R")
  scripts <- c(
    "make.R",
    "_drake.R",
    file.path("R", "packages.R"),
    file.path("R", "functions.R"),
    file.path("R", "plan.R")
  )
  for (script in scripts) {
    usethis::use_template(
      file.path("usedrake", script),
      save_as = script,
      package = "drake",
      open = open
    )
  }
  invisible()
  # nocov end
}
