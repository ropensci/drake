#' @title Run a single target's command in debug mode.
#' @description Also load the target's dependencies beforehand.
#' @export
#' @seealso drake_build
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
  config = drake::read_drake_config(envir = envir, jobs = jobs),
  character_only = FALSE,
  envir = parent.frame(),
  jobs = 1,
  replace = FALSE,
  verbose = TRUE
){
  # Tested in tests/testthat/test-always-skipped.R.
  # nocov start
  if (!character_only){
    target <- as.character(substitute(target))
  }
  if (!length(target)){
    target <- utils::head(drake::failed(cache = config$cache), n = 1)
  }
  if (verbose){
    message("Building target `", target, "` in debug mode.")
  }
  loadd(
    list = target,
    deps = TRUE,
    envir = envir,
    cache = config$cache,
    graph = config$graph,
    jobs = jobs,
    replace = replace
  )
  index <- which(config$plan$target == target)
  config$plan$command[[index]] <- debug_command(config$plan$command[[index]])
  build_store(target = target, config = config)
  # nocov end
}

debug_command <- function(command){
  if (is.character(command)){
    debug_command_char(command)
  } else {
    . <- NULL
    out <- rlang::expr_text(command) %>%
      debug_command_char() %>%
      parse(text = .)
    out[[1]]
  }
}

debug_command_char <- function(command){
  paste0("drake::debug_and_run(function() {\n", command, "\n})")
}

#' @title Run a function in debug mode.
#' @description Internal function for [drake_debug()]. Not for general use.
#' @keywords internal
#' @seealso drake_debug
#' @export
#' @return the return value of `f`
#' @param f a function
debug_and_run <- function(f){
  # Tested in tests/testthat/test-always-skipped.R.
  # nocov start
  debug(f)
  f()
  # nocov end
}
