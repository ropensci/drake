#' @title Build/process a single target or import.
#' @export
#' @description For internal use only.
#' the only reason this function is exported
#' is to set up parallel socket (PSOCK) clusters
#' without much of a fuss.
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
#' load_basic_example() # Get the code with drake_example("basic").
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
){
  if (!is.null(meta)){
    warning(
      "drake_build() is exclusively user-side now, ",
      "so we can affort to compute `meta` on the fly. ",
      "Thus, the `meta` argument is deprecated."
    )
  }
  if (!character_only){
    target <- as.character(substitute(target))
  }
  if (is.null(config)){
    config <- read_drake_config(envir = envir, jobs = jobs)
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
  build_and_store(target = target, config = config)
}

build_and_store <- function(target, config, meta = NULL){
  # The environment should have been pruned by now.
  # For staged parallelism, this was already done in bulk
  # for the whole stage.
  # Most of these steps require access to the cache.
  config$hook({
    if (is.null(meta)){
      meta <- drake_meta(target = target, config = config)
    }
    meta$start <- proc.time()
    announce_build(target = target, meta = meta, config = config)
    build <- just_build(target = target, meta = meta, config = config)
    conclude_build(
      target = target,
      value = build$value,
      meta = build$meta,
      config = config
    )
  })
}

just_build <- function(target, meta, config){
  if (meta$imported) {
    process_import(target = target, meta = meta, config = config)
  } else {
    # build_target() does not require access to the cache.
    # A custom future-based job scheduler could build with different steps
    # to write the output to the master process before caching it.
    build_target(
      target = target,
      meta = meta,
      config = config
    )
  }
}

announce_build <- function(target, meta, config){
  set_progress(
    target = target,
    value = "in progress",
    config = config
  )
  console(imported = meta$imported, target = target, config = config)
}

conclude_build <- function(target, value, meta, config){
  check_processed_file(target)
  store_target(target = target, value = value, meta = meta, config = config)
  handle_build_error(target = target, meta, config = config)
  invisible(value)
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

build_target <- function(target, meta, config){
  retries <- 0
  max_retries <- drake_plan_override(
    target = target,
    field = "retries",
    config = config
  ) %>%
    as.numeric
  while (retries <= max_retries){
    build <- one_build(
      target = target,
      meta = meta,
      config = config
    )
    if (!inherits(build$meta$error, "error")){
      return(build)
    }
    write(
      x = paste0(
        "Error building target ", target, ": ", build$meta$error$message),
      file = stderr()
    )
    retries <- retries + 1
    console_retry(target = target, retries = retries, config = config)
  }
  build
}

process_import <- function(target, meta, config) {
  if (is_file(target)) {
    value <- NA
  } else if (target %in% ls(config$envir, all.names = TRUE)) {
    value <- config$envir[[target]]
  } else {
    value <- tryCatch(
      flexible_get(target),
      error = function(e)
        console(imported = NA, target = target, config = config))
  }
  list(value = value, meta = meta)
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
