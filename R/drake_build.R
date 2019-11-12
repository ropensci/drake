#' @title Build/process a single target or import.
#' \lifecycle{maturing}
#' @description Not valid for dynamic branching.
#' @export
#' @seealso [drake_debug()]
#' @return The value of the target right after it is built.
#' @param target Name of the target.
#' @param meta Deprecated.
#' @param config Internal configuration list.
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
#' # Create the master internal configuration list.
#' config <- drake_config(my_plan)
#' out <- drake_build(small, config = config)
#' # Now includes `small`.
#' cached()
#' head(readd(small))
#' # `small` was invisibly returned.
#' head(out)
#' # If you previously called make(),
#' # `config` is just read from the cache.
#' make(my_plan, verbose = FALSE)
#' config <- drake_config(my_plan)
#' result <- drake_build(small, config = config)
#' head(result)
#' }
#' })
#' }
drake_build <- function(
  target,
  config = NULL,
  meta = NULL,
  character_only = FALSE,
  envir = NULL,
  jobs = 1,
  replace = FALSE
) {
  config$logger$minor("begin drake_build()", target = target)
  on.exit(
    config$logger$minor("end drake_build()", target = target),
    add = TRUE
  )
  if (!is.null(meta)) {
    warning(
      "drake_build() is exclusively user-side now, ",
      "so we can afford to compute `meta` on the fly. ",
      "Thus, the `meta` argument is deprecated."
    )
  }
  if (!is.null(envir)) {
    warning(
      "The envir argument of drake_build() is deprecated ",
      "Create a `drake_config()` object and use the `config` ",
      "argument of drake_build() instead."
    )
  }
  if (!character_only) {
    target <- as.character(substitute(target))
  }
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
}

#' @title Run a single target's command in debug mode.'
#' \lifecycle{maturing}
#' @description Not valid for dynamic branching.
#'   `drake_debug()` loads a target's dependencies
#'   and then runs its command in debug mode (see `browser()`,
#'   `debug()`, and `debugonce()`). This function does not
#'   store the target's value in the cache
#'   (see <https://github.com/ropensci/drake/issues/587>).
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
#' # Create the master internal configuration list.
#' config <- drake_config(my_plan)
#' out <- drake_build(small, config = config)
#' # Now includes `small`.
#' cached()
#' head(readd(small))
#' # `small` was invisibly returned.
#' head(out)
#' # If you previously called make(),
#' # `config` is just read from the cache.
#' make(my_plan, verbose = FALSE)
#' result <- drake_build(small, config = config)
#' head(result)
#' }
#' })
#' }
drake_debug <- function(
  target = NULL,
  config = NULL,
  character_only = FALSE,
  envir = NULL,
  jobs = 1,
  replace = FALSE,
  verbose = TRUE
) {
  # Tested in tests/testthat/test-always-skipped.R.
  # nocov start
  if (is.null(config)) {
    stop(
      "In `drake_debug()`, you must supply a `drake_config()` ",
      "object to the `config` argument.",
      call. = FALSE
    )
  }
  if (!is.null(envir)) {
    warning("the `envir` argument of drake_debug() is deprecated")
  }
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  if (!length(target)) {
    target <- utils::head(drake::failed(cache = config$cache), n = 1)
  }
  if (verbose) {
    message("Building target `", target, "` in debug mode.")
  }
  deps <- deps_memory(targets = target, config = config)
  for (dep in deps) {
    if (replace || !exists(dep, envir = config$envir_targets)) {
      value <- config$cache$get(dep)
      assign(dep, value, envir = config$envir_targets, inherits = FALSE)
    }
  }
  config$layout[[target]]$command_build <- cdl_preprocess_command(
    debug_command(config$layout[[target]]$command)
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

debug_command <- function(command) {
  if (is.character(command)) {
    debug_command_char(command)
  } else {
    . <- NULL
    out <- safe_deparse(command)
    out <- debug_command_char(out)
    parse(text = out)[[1]]
  }
}

debug_command_char <- function(command) {
  paste0("drake::debug_and_run(function() {\n", command, "\n})")
}

#' @title Run a function in debug mode.
#' \lifecycle{stable}
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
