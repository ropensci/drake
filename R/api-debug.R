#' @title Run a single target's command in debug mode.
#' @description `drake_debug()` loads a target's dependencies
#'   and then runs its command in debug mode (see `browser()`,
#'   `debug()`, and `debugonce()`). This function does not
#'   store the target's value in the cache
#'   (see <https://github.com/ropensci/drake/issues/587>).
#' @export
#' @seealso [drake_build()]
#' @return The value of the target right after it is built.
#' @inheritParams drake_build
#' @param verbose logical, whether to print out the target
#'   you are debugging.
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
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
#' result <- drake_build(small)
#' head(result)
#' })
#' }
drake_debug <- function(
  target = NULL,
  config = NULL,
  character_only = FALSE,
  envir = parent.frame(),
  jobs = 1,
  replace = FALSE,
  verbose = TRUE
) {
  # Tested in tests/testthat/test-always-skipped.R.
  # nocov start
  force(envir)
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
  loadd(
    list = target,
    deps = TRUE,
    envir = config$eval,
    cache = config$cache,
    graph = config$graph,
    jobs = jobs,
    replace = replace,
    tidyselect = FALSE
  )
  config$layout[[target]]$command_build <- preprocess_command(
    debug_command(config$layout[[target]]$command)
  )
  meta <- drake_meta_(target = target, config = config)
  announce_build(target = target, meta = meta, config = config)
  build <- build_target(target = target, meta = meta, config = config)
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
    out <- wide_deparse(command)
    out <- debug_command_char(out)
    parse(text = out)[[1]]
  }
}

debug_command_char <- function(command) {
  paste0("drake::debug_and_run(function() {\n", command, "\n})")
}

#' @title Run a function in debug mode.
#' @description Internal function for [drake_debug()]. Not for general use.
#' @keywords internal
#' @seealso [drake_debug()]
#' @export
#' @return The return value of `f`.
#' @param f a function
debug_and_run <- function(f) {
  # Tested in tests/testthat/test-always-skipped.R.
  # nocov start
  debug(f)
  f()
  # nocov end
}
