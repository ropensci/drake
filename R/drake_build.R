#' @title Build/process a single target or import.
#' `r lifecycle::badge("questioning")`
#' @description Not valid for dynamic branching.
#' @export
#' @seealso [drake_debug()]
#' @return The value of the target right after it is built.
#' @param target Name of the target.
#' @param config Deprecated 2019-12-22.
#' @param ... Arguments to [make()], such as the plan and environment.
#' @param meta Deprecated.
#' @inheritParams loadd
#' @inheritParams readd
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' # This example is not really a user-side demonstration.
#' # It just walks through a dive into the internals.
#' # Populate your workspace and write 'report.Rmd'.
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' out <- drake_build(small, my_plan)
#' # Now includes `small`.
#' cached()
#' head(readd(small))
#' # `small` was invisibly returned.
#' head(out)
#' }
#' })
#' }
drake_build <- function(
  target,
  ...,
  meta = NULL,
  character_only = FALSE,
  replace = FALSE,
  config = NULL
) {
}

#' @title Internal function with a drake_config() argument
#' @export
#' @keywords internal
#' @description Not a user-side function.
#' @inheritParams outdated
#' @param config A [drake_config()] object.
drake_build_impl <- function(
  target,
  config = NULL,
  meta = NULL,
  character_only = FALSE,
  replace = FALSE
) {
  deprecate_arg(meta)
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  assert_static(target, config, "drake_build()")
  deps <- deps_memory(targets = target, config = config)
  for (dep in deps) {
    if (replace || !exists(dep, envir = config$envir_targets)) {
      value <- config$cache$get(dep)
      assign(dep, value, envir = config$envir_targets, inherits = FALSE)
    }
  }
  config$ht_dynamic <- ht_new()
  meta <- drake_meta_(target = target, config = config)
  announce_build(target = target, config = config)
  build <- try_build(target = target, meta = meta, config = config)
  conclude_build(build = build, config = config)
  invisible(build$value)
}

body(drake_build) <- config_util_body(drake_build_impl)

#' @title Run a single target's command in debug mode.'
#' `r lifecycle::badge("questioning")`
#' @description Not valid for dynamic branching.
#'   `drake_debug()` loads a target's dependencies
#'   and then runs its command in debug mode (see `browser()`,
#'   `debug()`, and `debugonce()`). This function does not
#'   store the target's value in the cache
#'   (see `https://github.com/ropensci/drake/issues/587`).
#' @export
#' @seealso [drake_build()]
#' @return The value of the target right after it is built.
#' @inheritParams drake_build
#' @param verbose Logical, whether to print out the target
#'   you are debugging.
#' @examples
#' \dontrun{
#' isolate_example("Quarantine side effects.", {
#' if (suppressWarnings(require("knitr"))) {
#' # This example is not really a user-side demonstration.
#' # It just walks through a dive into the internals.
#' # Populate your workspace and write 'report.Rmd'.
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' # out <- drake_debug(small, my_plan)
#' # `small` was invisibly returned.
#' # head(out)
#' }
#' })
#' }
drake_debug <- function(
  target = NULL,
  ...,
  character_only = FALSE,
  replace = FALSE,
  verbose = TRUE,
  config = NULL
) {
}

drake_debug_impl <- function(
  target = NULL,
  config = NULL,
  character_only = FALSE,
  replace = FALSE,
  verbose = TRUE
) {
  # Tested in tests/testthat/test-always-skipped.R.
  # nocov start
  if (is.null(config)) {
    stop0("drake_debug() needs a drake_config() in config.")
  }
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  if (!length(target)) {
    target <- utils::head(drake::failed(cache = config$cache), n = 1)
  }
  assert_static(target, config, "drake_debug()")
  if (verbose) {
    cli_msg("Building target", target, "in debug mode.")
  }
  deps <- deps_memory(targets = target, config = config)
  for (dep in deps) {
    if (replace || !exists(dep, envir = config$envir_targets)) {
      value <- config$cache$get(dep)
      assign(dep, value, envir = config$envir_targets, inherits = FALSE)
    }
  }
  config$spec[[target]]$command_build <- cds_preprocess_command(
    debug_command(config$spec[[target]]$command)
  )
  config$ht_dynamic <- ht_new()
  meta <- drake_meta_(target = target, config = config)
  announce_build(target = target, config = config)
  build <- try_build(target = target, meta = meta, config = config)
  assert_output_files(target = target, meta = build$meta, config = config)
  handle_build_exceptions(target = target, meta = build$meta, config = config)
  invisible(build$value)
  # nocov end
}

body(drake_debug) <- config_util_body(drake_debug_impl)

debug_command <- function(command) {
  if (is.character(command)) {
    debug_command_char(command)
  } else {
    . <- NULL
    out <- safe_deparse(command, backtick = TRUE)
    out <- debug_command_char(out)
    parse(text = out)[[1]]
  }
}

debug_command_char <- function(command) {
  paste0("drake::debug_and_run(function() {\n", command, "\n})")
}

#' @title Run a function in debug mode.
#' `r lifecycle::badge("stable")`
#' @description Internal function for [drake_debug()]. Not for general use.
#' @keywords internal
#' @seealso [drake_debug()]
#' @export
#' @return The return value of `f`.
#' @param f A function.
debug_and_run <- function(f) {
  # Tested in tests/testthat/test-always-skipped.R.
  # nocov start
  debug(f)
  f()
  # nocov end
}
