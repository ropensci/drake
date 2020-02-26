#' @title `drake_deps_ht` helper
#' @keywords internal
#' @description Static code analysis.
#' @return A `drake_deps_ht` object.
#' @inheritParams drake_deps
#' @examples
#' if (FALSE) { # stronger than roxygen dontrun
#' expr <- quote({
#'   a <- base::list(1)
#'   b <- seq_len(10)
#'   file_out("abc")
#'   file_in("xyz")
#'   x <- "123"
#'   loadd(abc)
#'   readd(xyz)
#' })
#' drake_deps_ht(expr)
#' }
drake_deps_ht <- function(expr, exclude = character(0), restrict = NULL) {
  results <- new_drake_deps_ht()
  locals <- ht_new_from_list(ignored_symbols_list)
  ht_set(locals, exclude)
  walk_code(expr, results, locals, restrict)
  results
}

#' @title `drake_deps_ht` constructor
#' @keywords internal
#' @description List of class `drake_deps_ht`.
#' @return A `drake_deps_ht` object.
#' @inheritParams drake_deps
#' @examples
#' if (FALSE) { # stronger than roxygen dontrun
#' new_drake_deps_ht()
#' }
new_drake_deps_ht <- function(
  globals = ht_new(hash = TRUE),
  namespaced = ht_new(hash = FALSE),
  strings = ht_new(hash = FALSE),
  loadd = ht_new(hash = FALSE),
  readd = ht_new(hash = FALSE),
  file_in = ht_new(hash = FALSE),
  file_out = ht_new(hash = FALSE),
  knitr_in = ht_new(hash = FALSE)
) {
  out <- list(
    globals = globals,
    namespaced = namespaced,
    strings = strings,
    loadd = loadd,
    readd = readd,
    file_in = file_in,
    file_out = file_out,
    knitr_in = knitr_in
  )
  class(out) <- c("drake_deps_ht", "drake")
  out
}

drake_validate.drake_deps_ht <- function(x) {
  lapply(x, assert_environment)
  out_fields <- names(x)
  exp_fields <- c(
    "globals",
    "namespaced",
    "strings",
    "loadd",
    "readd",
    "file_in",
    "file_out",
    "knitr_in"
  )
  stopifnot(identical(out_fields, exp_fields))
}

#' @export
print.drake_deps_ht <- function(x, ...) {
  cat("drake_deps_ht\n")
  utils::str(unclass(x), no.list = TRUE)
}
