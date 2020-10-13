#' @title Launch a drake function in a fresh new R process
#' `r lifecycle::badge("stable")`
#' @description The `r_*()` functions, such as `r_make()`,
#'   enhance reproducibility by launching a `drake` function in
#'   a separate R process.
#' @details `drake` searches your environment
#'   to detect dependencies, so functions like [make()], [outdated()], etc.
#'   are designed to run in fresh clean R sessions. Wrappers [r_make()],
#'   [r_outdated()], etc. run reproducibly even if your current R session
#'   is old and stale.
#'
#'  [r_outdated()] runs the four steps below.
#'   [r_make()] etc. are similar.
#'   1. Launch a new `callr::r()` session.
#'   2. In that fresh session, run the R script from the `source` argument.
#'     This script loads packages, functions, global options, etc.
#'     and calls [drake_config()] at the very end. [drake_config()]
#'     is the preprocessing step of [make()], and it accepts
#'     all the same arguments as [make()] (e.g. `plan` and `targets`).
#'   3. In that same session, run [outdated()]
#'     with the `config` argument from step 2.
#'   4. Return the result back to main process
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
#'   Example:
#'   `r_make(r_fn = callr::r_bg, r_args = list(stdout = "stdout.log"))`.
#' @examples
#' \dontrun{
#' isolate_example("quarantine side effects", {
#' if (requireNamespace("knitr", quietly = TRUE)) {
#' writeLines(
#'   c(
#'     "library(drake)",
#'     "load_mtcars_example()",
#'     "drake_config(my_plan, targets = c(\"small\", \"large\"))"
#'   ),
#'   "_drake.R" # default value of the `source` argument
#' )
#' cat(readLines("_drake.R"), sep = "\n")
#' r_outdated()
#' r_make()
#' r_outdated()
#' }
#' })
#' }
r_make <- function(source = NULL, r_fn = NULL, r_args = list()) {
  invisible(r_drake(source, drake::make_impl, list(), r_fn, r_args))
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
#' @inheritParams drake_build
r_drake_build <- function(
  target,
  character_only = FALSE,
  ...,
  source = NULL,
  r_fn = NULL,
  r_args = list()
) {
  d_args <- list(...)
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  d_args$target <- target
  d_args$character_only <- TRUE
  r_drake(source, drake::drake_build_impl, d_args, r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_outdated <- function(..., source = NULL, r_fn = NULL, r_args = list()) {
  r_drake(source, drake::outdated_impl, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
#' @inheritSection recoverable Recovery
r_recoverable <- function(..., source = NULL, r_fn = NULL, r_args = list()) {
  r_drake(source, drake::recoverable_impl, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_missed <- function(..., source = NULL, r_fn = NULL, r_args = list()) {
  r_drake(source, drake::missed_impl, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
#' @inheritParams deps_target
r_deps_target <- function(
  target,
  character_only = FALSE,
  ...,
  source = NULL,
  r_fn = NULL,
  r_args = list()
) {
  d_args <- list(...)
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  d_args$target <- target
  d_args$character_only <- TRUE
  r_drake(source, drake::deps_target_impl, d_args, r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_drake_graph_info <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  r_drake(source, drake::drake_graph_info_impl, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_vis_drake_graph <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  assert_pkg("visNetwork")
  requireNamespace("visNetwork")
  r_drake(source, drake::vis_drake_graph_impl, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_sankey_drake_graph <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  assert_pkg("networkD3")
  requireNamespace("networkD3")
  r_drake(source, drake::sankey_drake_graph_impl, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_drake_ggraph <- function(..., source = NULL, r_fn = NULL, r_args = list()) {
  assert_pkg("ggraph")
  requireNamespace("ggraph")
  r_drake(source, drake::drake_ggraph_impl, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_text_drake_graph <- function(
  ...,
  source = NULL,
  r_fn = NULL,
  r_args = list()
) {
  assert_pkg("txtplot")
  args <- list(...)
  args$crayon <- getOption("crayon.enabled") %|||% TRUE
  out <- r_drake(
    source,
    function(..., crayon) {
      with_options(
        list(crayon.enabled = crayon),
        drake::text_drake_graph_impl(...)
      )
    },
    args,
    r_fn,
    r_args
  )
  invisible(out)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_predict_runtime <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  assert_pkg("lubridate")
  requireNamespace("lubridate")
  r_drake(source, drake::predict_runtime_impl, list(...), r_fn, r_args)
}

#' @rdname r_make
#' @export
#' @inheritParams r_make
r_predict_workers <- function(
  ..., source = NULL, r_fn = NULL, r_args = list()
) {
  r_drake(source, drake::predict_workers_impl, list(...), r_fn, r_args)
}

r_drake <- function(source, d_fn, d_args, r_fn, r_args) {
  assert_pkg("callr")
  r_args$func <- function(source, d_fn, d_args) {
    d_args$config <- source(source)$value
    do.call(d_fn, d_args)
  }
  source <- source %|||% getOption("drake_source") %|||% default_drake_source
  r_assert_source(source)
  r_args$args <- list(source = source, d_fn = d_fn, d_args = d_args)
  r_fn <- r_fn %|||% callr::r
  if ("show" %in% names(formals(r_fn))) {
    r_args$show <- r_args$show %|||% TRUE
  }
  if ("env" %in% names(formals(r_fn))) {
    r_args$env <- r_args$env %|||% callr::rcmd_safe_env()
    r_args$env <- c(r_args$env, PROCESSX_NOTIFY_OLD_SIGCHLD = "true")
  }
  do.call(r_fn, r_args)
}

default_drake_source <- "_drake.R"

r_assert_source <- function(source) {
  if (file.exists(source)) {
    return()
  }
  stop0(
    "file ", source, " does not exist.\n",
    "Functions r_make() and friends need _drake.R or similar script file.\n",
    "https://books.ropensci.org/drake/projects.html#safer-interactivity" # nolint
  )
}
