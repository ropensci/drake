store_target <- function(target, value, meta, start, config) {
  # Need complete up-to-date metadata
  # to store targets properly.
  # Files must have up-to-date modification times and hashes,
  # and any metadata postponed due to custom triggers
  # must be collected now.
  meta <- finish_meta(
    target = target,
    meta = meta,
    config = config
  )
  if (inherits(value, "error")){
    store_error(
      target = target,
      value = value,
      meta = meta,
      config = config
    )
  } else if (is_file(target)) {
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
      meta = meta,
      config = config
    )
  }
}

finalize_storage <- function(target, meta, config, progress){
   meta <- append_times_to_meta(
    target = target,
    start = start,
    meta = meta,
    config = config
  )
  config$cache$set(key = target, value = meta, namespace = "meta")
  set_progress(
    target = target,
    value = progress,
    config = config
  )
}

store_object <- function(target, value, meta, config) {
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
  finalize_storage(
    target = target,
    meta = meta,
    config = config,
    progress = "finished"
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
  finalize_storage(
    target = target,
    meta = meta,
    config = config,
    progress = "finished"
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
  finalize_storage(
    target = target,
    meta = meta,
    config = config,
    progress = "finished"
  )
}

store_error <- function(target, config){
  config$cache$set(
    key = target,
    value = value,
    namespace = "errors"
  )
  text <- paste("fail", target)
  if (config$verbose){
    finish_console(text = text, pattern = "fail", verbose = config$verbose)
  }
  finalize_storage(
    target = target,
    meta = meta,
    config = config,
    progress = "failed"
  )
  # We may actually want the option to allow failures
  stop(
    "Target '", target, "' failed to build. ",
    "Use diagnose(", target,
    ") to retrieve diagnostic information.",
    call. = FALSE
  )
}
