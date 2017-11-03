#' @title Internal function build
#' @export
#' @description Function to build a target.
#' For internal use only.
#' the only reason this function is exported
#' is to set up PSOCK clusters efficiently.
#' @param target name of the target
#' @param hash_list list of hashes that tell which
#' targets are up to date
#' @param config internal configuration list
build <- function(target, hash_list, config){
  config$hook(
    build_in_hook(
      target = target,
      hash_list = hash_list,
      config = config
    )
  )
}

build_in_hook <- function(target, hash_list, config) {
  start <- proc.time()
  hashes <- hash_list[[target]]
  config$cache$set(key = target, value = "in progress",
    namespace = "progress")
  console(imported = hashes$imported, target = target, config = config)
  if (hashes$imported) {
    value <- imported_target(target = target, hashes = hashes,
      config = config)
  } else {
    value <- build_target(target = target,
      hashes = hashes, config = config)
  }
  build_time <- (proc.time() - start) %>%
    runtime_entry(target = target, imported = hashes$imported)
  store_target(target = target, value = value, hashes = hashes,
    build_time = build_time, config = config)
  value
}

build_target <- function(target, hashes, config) {
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

imported_target <- function(target, hashes, config) {
  if (is_file(target)) {
    return(hashes$file)
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

store_target <- function(target, value, hashes, build_time, config) {
  config$cache$set(key = target, value = build_time,
    namespace = "build_times")
  config$cache$set(key = target, value = hashes$command,
    namespace = "commands")
  config$cache$set(key = target, value = hashes$depends,
    namespace = "depends")
  config$cache$set(key = target, value = hashes$imported,
    namespace = "imported")
  config$cache$set(key = target, value = "finished",
    namespace = "progress")
  if (is_file(target)) {
    store_file(target = target, hashes = hashes,
      config = config)
  } else if (is.function(value)) {
    store_function(target = target, value = value,
      hashes = hashes, config = config)
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
  hash <- config$cache$set(
    key = target, value = value, namespace = "readd")
  config$cache$driver$set_hash(
    key = target, namespace = "reproducibly_tracked", hash = hash)
}

store_file <- function(target, hashes, config) {
  config$cache$set(key = target, value = "file",
    namespace = "type")
  config$cache$set(key = target, value = file.mtime(eply::unquote(target)),
    namespace = "file_modification_times")
  hash <- ifelse(
    hashes$imported,
    hashes$file,
    rehash_file(target = target, config = config)
  )
  for (namespace in c("readd", "reproducibly_tracked")){
    config$cache$set(key = target, value = hash, namespace = namespace)
  }
}

store_function <- function(target, value, hashes, config
){
  config$cache$set(key = target, value = "function",
    namespace = "type")
  config$cache$set(key = target, value = value, namespace = "readd")
  # Unfortunately, vectorization is removed, but this is for the best.
  string <- deparse(unwrap_function(value))
  if (hashes$imported){
    string <- c(string, hashes$depends)
  }
  config$cache$set(key = target, value = string,
    namespace = "reproducibly_tracked")
}

# Turn a command into an anonymous function
# call to avoid side effects that can interfere
# with parallelism
functionize <- function(command) {
  paste0("(function(){\n", command, "\n})()")
}
