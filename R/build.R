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
  if (is.null(meta)){
    meta <- drake_meta(target = target, config = config)
  }
  config$hook(
    build_in_hook(
      target = target,
      meta = meta,
      config = config
    )
  )
}

drake_build_worker <- function(target, meta_list, config){
  drake_build(
    target = target,
    meta = meta_list[[target]],
    config = config
  )
}

build_in_hook <- function(target, meta, config) {
  start <- proc.time()
  set_progress(
    target = target,
    value = "in progress",
    config = config
  )
  console(imported = meta$imported, target = target, config = config)
  if (meta$imported) {
    value <- imported_target(target = target,
      config = config)
  } else {
    value <- build_target(target = target,
      config = config)
    check_built_file(target)
  }
  store_target(target = target, value = value, meta = meta,
    start = start, config = config)
  handle_error(target = target, value = value)
  invisible(value)
}

build_target <- function(target, config) {
  command <- get_evaluation_command(target = target, config = config)
  seed <- list(seed = config$seed, target = target) %>%
    seed_from_object
  run_command(
    target = target, command = command, config = config, seed = seed)
}

check_built_file <- function(target){
  if (!is_file(target)){
    return()
  }
  if (!file.exists(drake::drake_unquote(target))){
    warning(
      "File target ", target, " was built,\n",
      "but the file itself does not exist.",
      call. = FALSE
    )
  }
}

imported_target <- function(target, config) {
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
handle_error <- function(target, value){
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
