store_target <- function(target, value, meta, config) {
  config$cache$set(key = target, value = meta$command,
                   namespace = "commands")
  config$cache$set(key = target, value = meta$depends,
                   namespace = "depends")
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
  config$cache$set(key = target, value = meta, namespace = "meta")
}

store_object <- function(target, value, config) {
  hash <- config$cache$set(
    key = target,
    value = value,
    namespace = config$cache$default_namespace
  )
  config$cache$driver$set_hash(
    key = target,
    namespace = "kernels",
    hash = hash
  )
}

store_file <- function(target, meta, config) {
  config$cache$set(
    key = target,
    value = file.mtime(drake::drake_unquote(target)),
    namespace = "mtimes"
  )
  value <- ifelse(
    meta$imported,
    meta$file,
    rehash_file(target = target, config = config)
  )
  hash <- config$cache$set(
    key = target,
    value = value,
    namespace = config$cache$default_namespace
  )
  config$cache$driver$set_hash(
    key = target,
    namespace = "kernels",
    hash = hash
  )
}

store_function <- function(target, value, meta, config){
  config$cache$set(
    key = target,
    value = value,
    namespace = config$cache$default_namespace
  )
  # Unfortunately, vectorization is removed, but this is for the best.
  string <- deparse(unwrap_function(value))
  if (meta$imported){
    string <- c(string, meta$depends)
  }
  config$cache$set(key = target, value = string,
                   namespace = "kernels")
}

store_build_time <- function(target, start, meta, config){
  build_time <- (proc.time() - start) %>%
    runtime_entry(target = target, imported = meta$imported)
  config$cache$set(key = target, value = build_time,
                   namespace = "build_times")
}
