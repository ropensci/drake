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
  }
  store_target(target = target, value = value, meta = meta,
    start = start, config = config)
  invisible(value)
}

build_target <- function(target, config) {
  command <- get_evaluation_command(target = target, config = config)
  seed <- list(seed = config$seed, target = target) %>%
    seed_from_object
  value <- run_command(
    target = target, command = command, config = config, seed = seed
  )
  check_built_file(target)
  value
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

flexible_get <- function(target) {
  stopifnot(length(target) == 1)
  parsed <- parse(text = target) %>%
    as.call %>%
    as.list
  lang <- parsed[[1]]
  is_namespaced <- length(lang) > 1
  if (!is_namespaced)
    return(get(target))
  stopifnot(deparse(lang[[1]]) %in% c("::", ":::"))
  pkg <- deparse(lang[[2]])
  fun <- deparse(lang[[3]])
  get(fun, envir = getNamespace(pkg))
}

# Turn a command into an anonymous function
# call to avoid side effects that could interfere
# with parallelism.
functionize <- function(command) {
  paste0("(function(){\n", command, "\n})()")
}
