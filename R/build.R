#' @title Build/process a single target or import.
#' @export
#' @keywords internal
#' @description For internal use only.
#' the only reason this function is exported
#' is to set up parallel socket (PSOCK) clusters
#' without much of a fuss.
#' @return The value of the target right after it is built.
#' @param target name of the target
#' @param meta list of metadata that tell which
#'   targets are up to date (from [drake_meta()]).
#' @param config internal configuration list
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # This example is not really a user-side demonstration.
#' # It just walks through a dive into the internals.
#' # Populate your workspace and write 'report.Rmd'.
#' load_basic_example() # Get the code with drake_example("basic").
#' # Create the master internal configuration list.
#' config <- drake_config(my_plan)
#' # Optionally, compute metadata on 'small',
#' # including a hash/fingerprint
#' # of the dependencies. If meta is not supplied,
#' # drake_build() computes it automatically.
#' meta <- drake_meta(target = "small", config = config)
#' # Should not yet include 'small'.
#' cached()
#' # Build 'small'.
#' # Equivalent to just drake_build(target = "small", config = config).
#' drake_build(target = "small", config = config, meta = meta)
#' # Should now include 'small'
#' cached()
#' readd(small)
#' })
#' }
drake_build <- function(target, config, meta = NULL){
  # The environment should have been pruned by now.
  # For staged parallelism, this was already done in bulk
  # for the whole stage.
  # Most of these steps require access to the cache.
  config$hook({
    start <- proc.time()
    if (is.null(meta)){
      meta <- drake_meta(target = target, config = config)
    }
    announce_build(target = target, meta = meta, config = config)
    if (meta$imported) {
      value <- process_import(target = target, config = config)
    } else {
      # build_target() does not require access to the cache.
      # A custom future-based job scheduler could build with different steps
      # to write the output to the master process before caching it.
      value <- build_target(
        target = target,
        meta = meta,
        start = start,
        config = config
      )
    }
    conclude_build(
      target = target,
      value = value,
      meta = meta,
      start = start,
      config = config
    )
  })
}

announce_build <- function(target, meta, config){
  set_progress(
    target = target,
    value = "in progress",
    config = config
  )
  console(imported = meta$imported, target = target, config = config)
}

conclude_build <- function(target, value, meta, start, config){
  check_processed_file(target)
  store_target(target = target, value = value, meta = meta,
    start = start, config = config)
  set_progress(
    target = target,
    value = ifelse(inherits(value, "error"), "failed", "finished"),
    config = config
  )
  handle_error(target = target, value = value, config = config)
  invisible(value)
}

build_target <- function(target, meta, start, config) {
  command <- get_evaluation_command(target = target, config = config)
  seed <- list(seed = config$seed, target = target) %>%
    seed_from_object
  run_command(
    target = target, command = command, config = config, seed = seed)
}

check_processed_file <- function(target){
  if (!is_file(target)){
    return()
  }
  if (!file.exists(drake::drake_unquote(target))){
    warning(
      "File ", target, " was built or processed,\n",
      "but the file itself does not exist.",
      call. = FALSE
    )
  }
}

process_import <- function(target, config) {
  if (is_file(target)) {
    return(NA)
  } else if (target %in% ls(config$envir, all.names = TRUE)) {
    value <- config$envir[[target]]
  } else {
    value <- tryCatch(
      flexible_get(target),
      error = function(e)
        console(imported = NA, target = target, config = config))
  }
  value
}

# We may just want to have a warning here.
handle_error <- function(target, value, config){
  if (!inherits(value, "error")){
    return()
  }
  if (config$verbose){
    text <- paste("fail", target)
    finish_console(text = text, pattern = "fail", verbose = config$verbose)
  }
  stop(
    "Target '", target, "' failed to build. ",
    "Use diagnose(", target,
    ") to retrieve diagnostic information.",
    call. = FALSE
  )
}
