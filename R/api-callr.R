default_drake_source <- "_drake.R"

r_drake <- function(source, d_fn, d_args, r_fn, r_args) {
  assert_pkg("callr")
  r_args$func <- function(source, d_fn, d_args) {
    d_args$config <- source(source)$value
    do.call(d_fn, d_args)
  }
  source <- source %||% getOption("drake_source") %||% default_drake_source
  r_assert_source(source)
  r_args$args <- list(source = source, d_fn = d_fn, d_args = d_args)
  r_args$show <- r_args$show %||% TRUE
  r_fn <- r_fn %||% callr::r
  do.call(r_fn, r_args)
}

r_assert_source <- function(source) {
  if (file.exists(source)) {
    return()
  }
  stop(
    "File ", shQuote(source), " does not exist.\n",
    "Functions r_make() and friends need an R script file\n",
    "to configure your drake project.\n",
    "The configuration R script can be\n",
    "  1. The file you supply to the ", shQuote(source), " argument, or\n",
    "  2. The file you supply via options(drake_source = \"file.R\"), or\n",
    "  3. A file called ", shQuote("_drake.R"), " (default).\n",
    "Read more: \n",
    "https://ropenscilabs.github.io/drake-manual/projects.html#safer-interactivity", # nolint
    call. = FALSE
  )
}

#' @title Experimental: reproducible R session management for drake functions
#' @description A word of caution: [r_make()] and friends are still
#'   new and experimental.
#'
#'   `drake` searches your environment
#'   to detect dependencies, so functions like [make()], [outdated()], etc.
#'   are designed to run in fresh clean R sessions. Wrappers [r_make()],
#'   [r_outdated()], etc. run reproducibly even if your current R session
#'   is old and stale. 
#' @details [r_outdated()] runs the four steps below.
#'   [r_make()] etc. are similar.
#'   1. Launch a new `callr::r()` session.
#'   2. In that fresh session, run the R script from the `source` argument.
#'     This script loads packages, functions, global options, etc.
#'     and returns a [drake_config()] object.
#'   3. In that same session, run [outdated()]
#'     with the `config` argument from step 2.
#'   4. Return the result back to master process
#'     (e.g. your interactive R session).
#' @export
#' @seealso [make()]
#' @param ... Arguments to the inner function. For example, if you want to call
#'   [r_vis_drake_graph()], the inner function is [vis_drake_graph()], and
#'   `selfcontained` is an example argument you could supply to the ellipsis.
#' @param source Path to an R script file that
#'   loads packages, functions, etc. and returns a [drake_config()] object.
#'   There are 3 ways to set this path.
#'   1. Pass an explicit file path.
#'   2. Call `options(drake_source = "path_to_your_script.R")`.
#'   3. Just create a file called "_drake.R" in your working directory
#'     and supply nothing to `source`.
#' @param r_fn A `callr` function such as `callr::r` or `callr::r_bg`.
#'   Example: `r_make(r_fn = callr::r)`.
#' @param r_args List of arguments to `r_fn`, not including `func` or `args`.
#'   Example: `r_make(r_fn = callr::r_bg, r_args = list(stdout = "stdout.log"))`.
#' @examples
#' \dontrun{
#' test_with_dir("quarantine side effects", {
#' writeLines(
#'   c(
#'     "library(drake)",
#'     "load_mtcars_example()",
#'     "drake_config(my_plan)"
#'   ),
#'   "_drake.R" # default value of the `source` argument
#' )
#' cat(readLines("_drake.R"), sep = "\n")
#' r_outdated()
#' r_make()
#' r_outdated()
#' })
#' }
r_make <- function(source = NULL, r_fn = NULL, r_args = list()) {
  invisible(r_drake(source, drake::make, list(), r_fn, r_args))
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_drake_build <- function(..., source = NULL, r_fn = NULL, r_args = list()) {
  r_drake(source, drake::drake_build, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_outdated <- function(..., source = NULL, r_fn = NULL, r_args = list()) {
  r_drake(source, drake::outdated, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_missed <- function(..., source = NULL, r_fn = NULL, r_args = list()) {
  r_drake(source, drake::missed, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_drake_graph_info <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  r_drake(source, drake::drake_graph_info, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_vis_drake_graph <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  assert_pkg("visNetwork")
  requireNamespace("visNetwork")
  r_drake(source, drake::vis_drake_graph, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_sankey_drake_graph <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  assert_pkg("networkD3")
  requireNamespace("networkD3")
  r_drake(source, drake::sankey_drake_graph, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_drake_ggraph <- function(..., source = NULL, r_fn = NULL, r_args = list()) {
  r_drake(source, drake::drake_ggraph, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_predict_runtime <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  assert_pkg("lubridate")
  requireNamespace("lubridate")
  r_drake(source, drake::predict_runtime, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_predict_workers <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  r_drake(source, drake::predict_workers, list(...), r_fn, r_args)
}
