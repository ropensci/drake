#' @title Write an example `_drake.R` script to the current working directory.
#' @export
#' @description A `_drake.R` file is required for [r_make()] and friends.
#'   See the [r_make()] help file for details.
#' @return Nothing.
#' @param code R code to put in `_drake.R` in the current working directory.
#'   If `NULL`, an example script is written.
#' @examples
#' \dontrun{
#' isolate_example("contain side-effects", {
#' drake_script({
#'   library(drake)
#'   plan <- drake_plan(x = 1)
#'   drake_config(plan, lock_cache = FALSE)
#' })
#' cat(readLines("_drake.R"), sep = "\n")
#' r_make()
#' })
#' }
drake_script <- function(code = NULL) {
  code <- substitute(code)
  if (length(code)) {
    text <- parse_drake_script_code(code)
  } else {
    text <- example_drake_script()
  }
  writeLines(text, "_drake.R")
}

example_drake_script <- function() {
  path <- system.file(
    file.path("templates", "drake_script", "example_drake.R"),
    package = "drake",
    mustWork = TRUE
  )
  readLines(path)
}

parse_drake_script_code <- function(code) {
  if (length(code) > 1L && safe_deparse(code[[1]]) == "`{`") {
    vcapply(code[-1], safe_deparse)
  } else {
    safe_deparse(code)
  }
}
