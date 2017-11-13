#' @title Internal function drake_build
#' @export
#' @description Function to build a target or import.
#' For internal use only.
#' the only reason this function is exported
#' is to set up PSOCK clusters efficiently.
#' @return The value of the target right after it is built.
#' @param target name of the target
#' @param meta_list list of metadata that tell which
#' targets are up to date
#' @param config internal configuration list
#' @examples
#' \dontrun{
#' # This example is not really a user-side demonstration.
#' # It just walks through a dive into the internals.
#' # Populate your workspace and write 'report.Rmd'.
#' load_basic_example()
#' # Create the master internal configuration list.
#' config <- drake_config(my_plan)
#' # Compute metadata on 'small', including a hash/fingerprint
#' # of the dependencies.
#' meta_list <- list(
#'   small = drake:::meta(target = "small", config = config)
#' )
#' # Should not yet include 'small'.
#' cached()
#' # Build 'small'
#' drake_build(target = "small", meta_list = meta_list, config = config)
#' # Should now include 'small'
#' cached()
#' readd(small)
#' }
drake_build <- function(target, meta_list, config){
  config$hook(
    build_in_hook(
      target = target,
      meta_list = meta_list,
      config = config
    )
  )
}

build_in_hook <- function(target, meta_list, config) {
  start <- proc.time()
  meta <- finish_meta(
    target = target, meta = meta_list[[target]], config = config)
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
  build_time <- (proc.time() - start) %>%
    runtime_entry(target = target, imported = meta$imported)
  store_target(target = target, value = value, meta = meta,
    build_time = build_time, config = config)
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
  if (!file.exists(eply::unquote(target))){
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

store_target <- function(target, value, meta, build_time, config) {
  config$cache$set(key = target, value = build_time,
    namespace = "build_times")
  config$cache$set(key = target, value = meta$command,
    namespace = "commands")
  config$cache$set(key = target, value = meta$depends,
    namespace = "depends")
  config$cache$set(key = target, value = meta$imported,
    namespace = "imported")
  config$cache$set(key = target, value = "finished",
    namespace = "progress")
  if (is_file(target)) {
    store_file(target = target, meta = meta,
      config = config)
  } else if (is.function(value)) {
    store_function(target = target, value = value,
      meta = meta, config = config)
  } else {
    store_object(target = target, value = value,
      config = config)
  }
  hash <- config$cache$get_hash(key = target, namespace = "readd")
  config$cache$driver$set_hash(
    key = target, namespace = config$cache$default_namespace, hash = hash)
}

store_object <- function(target, value, config) {
  config$cache$set(key = target, value = "object",
    namespace = "type")
  storr_hash <- config$cache$set(
    key = target, value = value, namespace = "readd")
  config$cache$driver$set_hash(
    key = target, namespace = "kernels", hash = storr_hash)
}

store_file <- function(target, meta, config) {
  config$cache$set(key = target, value = "file",
    namespace = "type")
  config$cache$set(key = target, value = file.mtime(eply::unquote(target)),
    namespace = "mtimes")
  hash <- ifelse(
    meta$imported,
    meta$file,
    rehash_file(target = target, config = config)
  )
  storr_hash <- config$cache$set(
    key = target, value = hash, namespace = "readd")
  config$cache$driver$set_hash(
    key = target, namespace = "kernels", hash = storr_hash)
}

store_function <- function(target, value, meta, config
){
  config$cache$set(key = target, value = "function",
    namespace = "type")
  config$cache$set(key = target, value = value, namespace = "readd")
  # Unfortunately, vectorization is removed, but this is for the best.
  string <- deparse(unwrap_function(value))
  if (meta$imported){
    string <- c(string, meta$depends)
  }
  config$cache$set(key = target, value = string,
    namespace = "kernels")
}

# Turn a command into an anonymous function
# call to avoid side effects that could interfere
# with parallelism.
functionize <- function(command) {
  paste0("(function(){\n", command, "\n})()")
}

log_attempts <- Vectorize(function(targets, config){
  config$cache$set(
    key = targets, value = targets, namespace = "attempts")
  invisible()
},
"targets")
