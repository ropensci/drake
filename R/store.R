store_target <- function(target, value, meta, start, config) {
  # Need complete up-to-date metadata
  # to store targets properly.
  meta <- finish_meta(
    target = target,
    meta = meta,
    config = config
  )
  if (is_file(target)) {
    store_file(
      target = target,
      meta = meta,
      config = config
    )
  } else if (is.function(value)) {
    store_function(
      target = target,
      value = value,
      meta = meta,
      config = config
    )
  } else {
    store_object(
      target = target,
      value = value,
      config = config
    )
  }
  # Add build times to metadata at the last minute
  # so that the timing information takes caching operations
  # into account.
  meta <- append_times_to_meta(
    target = target,
    start = start,
    meta = meta,
    config = config
  )
  config$cache$set(key = target, value = meta, namespace = "meta")
  set_progress(
    target = target,
    value = "finished",
    config = config
  )
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
  # meta$file should have a file hash at this point.
  hash <- config$cache$set(
    key = target,
    value = meta$file,
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
  config$cache$set(
    key = target,
    value = string,
    namespace = "kernels"
  )
}
