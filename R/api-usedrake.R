#' @title Use drake in a project
#' @description Add top-level R script files to use `drake`
#'   in your data analysis project. For details, read
#'   <https://ropenscilabs.github.io/drake-manual/projects.html>
#' @details Files written:
#'   1. `make.R`: a suggested master R script for batch mode.
#'   2. `_drake.R`: a configuration R script for
#'     the [`r_*()`](https://ropensci.github.io/drake/reference/r_make.html) functions documented at # nolint
#'     <https://ropenscilabs.github.io/drake-manual/projects.html#safer-interactivity>. # nolint
#' Remarks:
#'   - There is nothing magical about the name, `make.R`.
#'     You can call it whatever you want.
#'   - Other supporting scripts, such as `R/packages.R`,
#'     `R/functions.R`, and `R/plan.R`, are not included.
#'     You can find examples at
#'     <https://github.com/wlandau/drake-examples/tree/master/main/R>
#'     and download examples with [`drake_example()`]
#'     (e.g. `drake_example("main")`).
#' @export
#' @param open Logical, whether to open `make.R` for editing.
#' @examples
#' \dontrun{
#' drake::test_with_dir("Contain side-effects", {
#' use_drake(open = FALSE)
#' })
#' }
use_drake <- function(open = interactive()) {
  assert_pkg("usethis")
  usethis::use_template(
    file.path("usedrake", "_drake.R"),
    save_as = "_drake.R",
    package = "drake",
    open = FALSE
  )
  usethis::use_template(
    file.path("usedrake", "make.R"),
    save_as = "make.R",
    package = "drake",
    open = open
  )
  invisible()
}
