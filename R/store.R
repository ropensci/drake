store_outputs <- function(target, outputs, value, meta, config){
  store_target(target = target, value = value, meta = meta, config = config)
  for (output in setdiff(outputs, target)){
    new_meta <- concomitant_output_meta(
      output = output,
      meta = meta,
      config = config
    )
    store_target(
      target = output,
      value = new_meta$file,
      meta = new_meta,
      config = config
    )
  }
}

store_target <- function(target, value, meta, config) {
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
  for (field in c("messages", "warnings", "error")){
    set_in_subspace(
      key = target,
      value = meta[[field]],
      subspace = field,
      namespace = "meta",
      cache = config$cache
    )
  }
}
