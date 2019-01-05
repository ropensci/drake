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
  envir = parent.frame(),
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
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  force(envir)
  if (is.null(config)) {
    config <- drake::read_drake_config(envir = envir, jobs = jobs)
    config$envir <- envir
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
  meta <- drake_meta(target = target, config = config)
  announce_build(target = target, meta = meta, config = config)
  build <- build_target(target = target, meta = meta, config = config)
  conclude_build(build = build, config = config)
}

announce_build <- function(target, meta, config) {
  set_progress(
    target = target,
    value = "in progress",
    config = config
  )
  console(imported = meta$imported, target = target, config = config)
}

build_target <- function(target, meta, config) {
  if (identical(config$garbage_collection, TRUE)) {
    on.exit(gc())
  }
  retries <- 0
  layout <- config$layout[[target]] %||% list()
  max_retries <- as.numeric(layout$retries %||NA% config$retries)
  while (retries <= max_retries) {
    build <- with_seed_timeout(
      target = target,
      meta = meta,
      config = config
    )
    if (!inherits(build$meta$error, "error")) {
      return(build)
    }
    retries <- retries + 1
    console_retry(target = target, retries = retries, config = config)
  }
  build
}

conclude_build <- function(build, config) {
  target <- build$target
  value <- build$value
  meta <- build$meta
  assert_output_files(target = target, meta = meta, config = config)
  handle_build_exceptions(target = target, meta = meta, config = config)
  store_outputs(target = target, value = value, meta = meta, config = config)
  assign_to_envir(target = target, value = value, config = config)
  invisible(value)
}

assert_output_files <- function(target, meta, config) {
  deps <- config$layout[[target]]$deps_build
  if (!length(deps$file_out)) {
    return()
  }
  files <- unique(as.character(deps$file_out))
  files <- decode_path(files, config)
  missing_files <- files[!file.exists(files)]
  if (length(missing_files)) {
    drake_warning(
      "Missing files for target ", target, ":\n",
      multiline_message(missing_files),
      config = config
    )
  }
}
