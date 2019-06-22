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
  namespace <- ifelse(show_cleaned, "meta", config$cache$default_namespace)
}
