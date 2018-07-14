store_outputs <- function(target, value, meta, config){
  # Failed targets need to stay invalidated,
  # even when `config$keep_going` is `TRUE`.
  if (inherits(meta$error, "error")){
    return()
  }
  meta <- finish_meta(
    target = target,
    meta = meta,
    config = config
  )
  store_single_output(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
  for (file in meta$output_files){
    meta$name <- file
    meta$mtime <- file.mtime(drake::drake_unquote(file))
    meta$file <- safe_rehash_file(target = file, config = config)
    store_single_output(
      target = file,
      value = meta$file,
      meta = meta,
      config = config
    )
  }
}

store_single_output <- function(target, value, meta, config) {
  if (is_file(target)) {
    store_object(
      target = target,
      value = meta$file,
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
  finalize_storage(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
}

finalize_storage <- function(target, value, meta, config){
  meta <- finalize_times(
    target = target,
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

store_object <- function(target, value, meta, config) {
  config$cache$set(
    key = target,
    value = value
  )
  config$cache$duplicate(
    key_src = target,
    key_dest = target,
    namespace_src = config$cache$default_namespace,
    namespace_dest = "kernels"
  )
}

store_function <- function(target, value, meta, config){
  config$cache$set(
    key = target,
    value = value,
    namespace = config$cache$default_namespace
  )
  # For the kernel of an imported function,
  # we ignore any code wrapped in ignore().
  if (meta$imported){
    value <- ignore_ignore(value)
  }
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

store_failure <- function(target, meta, config){
  set_progress(
    target = target,
    value = "failed",
    config = config
  )
  subspaces <- intersect(c("messages", "warnings", "error"), names(meta))
  set_in_subspaces(
    key = target,
    values = meta[subspaces],
    subspaces = subspaces,
    namespace = "meta",
    cache = config$cache
  )
}
