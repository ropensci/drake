#' @title Show history
#' @description See the history of your targets:
#'   what you ran, when you ran it, and where
#'   it lives. Optionally show the settings
#'   under which you ran things (could be slow)
#'   and whether each target is up to date (also could be slow).
#' @export
#' @return A data frame of target history.
#' @param config A list returned from `drake_config()`.
#' @param show_args Logical, whether to include atomic arguments
#'   to function calls in [drake_plan()] commands.
#'   Could be slow because this requires parsing and analyzing
#'   lots of R code.
#' @param show_status Logical, whether to show the status of each target
#'   based on the results of `outdated()`. Could be slow.
#' @inheritParams outdated
drake_history <- function(
  config,
  show_args = FALSE,
  show_status = FALSE,
  make_imports = TRUE,
  do_prework = TRUE
) {
  if (!has_history(config)) {
    config$history <- default_history_queue(config$cache_path)
  }
  config$history$list()
}

default_history_queue <- function(cache_path) {
  assert_pkg("txtq", version = "0.1.2")
  cache_dir <- dirname(cache_path)
  history_path <- file.path(cache_dir, ".drake_history")
  txtq::txtq(history_path)
}

has_history <- function(config) {
  if (is.null(config$history)) {
    return(FALSE)
  }
  inherits(config$history, "R6_txtq")
}
