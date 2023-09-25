#' @title `drake_deps` helper
#' @keywords internal
#' @description Static code analysis.
#' @return A `drake_deps` object.
#' @param expr An R expression
#' @param exclude Character vector of the names of symbols to exclude
#'   from the code analysis.
#' @param restrict Optional character vector of allowable names of globals.
#'   If `NULL`, all global symbols are detectable. If a character vector,
#'   only the variables in `restrict` will count as global variables.
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
#' drake_deps(expr)
#' }
drake_deps <- function(expr, exclude = character(0), restrict = NULL) {
  ternary(
    is.function(expr) || is.language(expr),
    drake_deps_impl(expr, exclude, restrict),
    new_drake_deps()
  )
}

drake_deps_impl <- function(expr, exclude = character(0), restrict = NULL) {
  results <- drake_deps_ht(expr, exclude, restrict)
  results <- lapply(results, ht_list)
  do.call(new_drake_deps, results)
}

#' @title `drake_deps` constructor
#' @keywords internal
#' @description List of class `drake_deps`.
#' @return A `drake_deps` object.
#' @param globals Global symbols found in the expression
#' @param namespaced Namespaced objects, e.g. `rmarkdown::render`.
#' @param strings Miscellaneous strings.
#' @param loadd Targets selected with [loadd()].
#' @param readd Targets selected with [readd()].
#' @param file_in Literal static file paths enclosed in [file_in()].
#' @param file_out Literal static file paths enclosed in [file_out()].
#' @param knitr_in Literal static file paths enclosed in [knitr_in()].
#' @examples
#' if (FALSE) { # stronger than roxygen dontrun
#' new_drake_deps()
#' }
new_drake_deps <- function(
  globals = character(0),
  namespaced = character(0),
  strings = character(0),
  loadd = character(0),
  readd = character(0),
  file_in = character(0),
  file_out = character(0),
  knitr_in = character(0)
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
  class(out) <- c("drake_deps", "drake")
  out
}

validate_drake_deps <- function(x) {
  stopifnot(inherits(x, "drake_deps"))
  stopifnot(inherits(x, "drake"))
  out_fields <- names(x)
  exp_fields <- names(formals(new_drake_deps))
  for (field in exp_fields) {
    stopifnot(is.character(x[[field]]))
  }
  stopifnot(identical(sort(out_fields), sort(exp_fields)))
}

#' @export
print.drake_deps <- function(x, ...) {
  cat("drake_deps\n")
  str0(x)
}
