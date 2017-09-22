build <- function(target, hash_list, config) {
  hashes <- hash_list[[target]]
  config$cache$set(key = target, value = "in progress",
    namespace = "progress")
  imported <- !(target %in% config$plan$target)
  console(imported = imported, target = target, config = config)
  if (imported) {
    value <- imported_target(target = target, hashes = hashes,
      config = config)
  } else {
    time <- system.time({
      value <- build_target(target = target,
        hashes = hashes, config = config)
    })
    config$cache$set(key = target, value = time,
      namespace = "build_times")
  }
  store_target(target = target, value = value, hashes = hashes,
    imported = imported, config = config)
  config$cache$set(key = target, value = hashes$depends,
    namespace = "depends")
  config$cache$set(key = target, value = "finished",
    namespace = "progress")
  value
}

build_target <- function(target, hashes, config) {
  command <- get_command(target = target, config = config) %>%
    functionize
  value <- eval(parse(text = command), envir = config$envir)
  check_built_file(target)
  value
}

check_built_file <- function(target){
  if (!is_file(target)){
    return()
  }
  if (!file.exists(eply::unquote(target))){
    warning("File target ", target, " was built,\n",
      "but the file itself does not exist.")
  }
}

imported_target <- function(target, hashes, config) {
  if (is_file(target))
    return(hashes$file)
  else if (target %in% ls(config$envir, all.names = TRUE))
    value <- config$envir[[target]]
  else
    value <- tryCatch(
      flexible_get(target),
      error = function(e)
        console(imported = NA, target = target, config = config))
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

store_target <- function(target, value, hashes, imported, config) {
  if (is_file(target))
    store_file(target, hashes = hashes, imported = imported,
      config = config) else if (is.function(value))
    store_function(target = target, value = value,
      imported = imported, hashes = hashes, config = config)
  else
    store_object(target = target, value = value,
      imported = imported, config = config)
  config$cache$set(key = target, value = hashes$depends,
    namespace = "depends")
}

store_object <- function(target, value, imported, config) {
  config$cache$set(key = target, value = list(type = "object",
    value = value, imported = imported))
}

store_file <- function(target, hashes, imported, config) {
  hash <- ifelse(imported, hashes$file, rehash_file(target))
  config$cache$set(key = target, value = file.mtime(eply::unquote(target)),
    namespace = "filemtime")
  config$cache$set(key = target, value = list(type = "file",
    value = hash, imported = imported))
}

store_function <- function(target, value, hashes, imported,
  config) {
  config$cache$set(key = target, value = value, namespace = "functions")
  # Unfortunately, vectorization is removed, but this is for the best.
  string <- deparse(unwrap_function(value))
  config$cache$set(key = target, value = list(type = "function",
    value = string, imported = imported,
    depends = hashes$depends)) # for nested functions
}

# bugfix: turn a command into an anonymous function
# call to avoid side effects that can interfere
# with parallelism
functionize <- function(command) {
  paste0("(function(){\n", command, "\n})()")
}
