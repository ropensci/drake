#' @title Build/process a single target or import.
#' @description Also load the target's dependencies beforehand.
#' @export
#' @seealso [drake_debug()]
#' @return The value of the target right after it is built.
#' @param target name of the target
#' @param meta list of metadata that tell which
#'   targets are up to date (from [drake_meta()]).
#' @param config internal configuration list
#' @inheritParams loadd
#' @inheritParams readd
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
drake_build <- function(
  target,
  config = NULL,
  meta = NULL,
  character_only = FALSE,
  envir = NULL,
  jobs = 1,
  replace = FALSE
) {
  if (!is.null(meta)) {
    warning(
      "drake_build() is exclusively user-side now, ",
      "so we can affort to compute `meta` on the fly. ",
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
  loadd(
    list = target,
    deps = TRUE,
    envir = config$eval,
    cache = config$cache,
    jobs = jobs,
    replace = replace,
    tidyselect = FALSE,
    config = config
  )
  meta <- drake_meta_(target = target, config = config)
  announce_build(target = target, meta = meta, config = config)
  build <- build_target(target = target, meta = meta, config = config)
  conclude_build(build = build, config = config)
}
