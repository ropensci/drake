#' @title Internal function \code{drake_build}
#' @export
#' @keywords internal
#' @description Function to build a target or import.
#' For internal use only.
#' the only reason this function is exported
#' is to set up PSOCK clusters efficiently.
#' @return The value of the target right after it is built.
#' @param target name of the target
#' @param meta list of metadata that tell which
#' targets are up to date (from \code{drake:::meta()}).
#' @param config internal configuration list
#' @examples
#' \dontrun{
#' test_with_dir("Quarantine side effects.", {
#' # This example is not really a user-side demonstration.
#' # It just walks through a dive into the internals.
#' # Populate your workspace and write 'report.Rmd'.
#' load_basic_example()
#' # Create the master internal configuration list.
#' config <- drake_config(my_plan)
#' # Compute metadata on 'small', including a hash/fingerprint
#' # of the dependencies.
#' meta <- drake:::meta(target = "small", config = config)
#' # Should not yet include 'small'.
#' cached()
#' # Build 'small'
#' drake_build(target = "small", meta = meta, config = config)
#' # Should now include 'small'
#' cached()
#' readd(small)
#' })
#' }
drake_build <- function(target, meta, config){
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
  meta <- finish_meta(
    target = target, meta = meta, config = config)
  config$cache$set(key = target, value = "in progress",
    namespace = "progress")
  console(imported = meta$imported, target = target, config = config)
  if (meta$imported) {
    value <- imported_target(target = target, meta = meta,
      config = config)
  } else {
    value <- build_target(target = target,
      meta = meta, config = config)
  }
  store_target(target = target, value = value, meta = meta,
    start = start, config = config)
  value
}

build_target <- function(target, meta, config) {
  command <- get_command(target = target, config = config) %>%
    functionize
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

imported_target <- function(target, meta, config) {
  if (is_file(target)) {
    return(meta$file)
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
