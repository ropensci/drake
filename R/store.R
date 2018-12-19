store_outputs <- function(target, value, meta, config) {
  # Failed targets need to stay invalidated,
  # even when `config$keep_going` is `TRUE`.
  if (inherits(meta$error, "error")) {
    return()
  }
  if (!meta$imported) {
    console_store(target = target, config = config)
  }
  layout <- config$layout[[target]]
  if (is.null(meta$command)) {
    meta$command <- layout$command_standardized
  }
  if (is.null(meta$dependency_hash)) {
    meta$dependency_hash <- dependency_hash(target = target, config = config)
  }
  if (is.null(meta$input_file_hash)) {
    meta$input_file_hash <- input_file_hash(target = target, config = config)
  }
  if (!is.null(meta$trigger$change)) {
    config$cache$set(
      key = target, value = meta$trigger$value, namespace = "change"
    )
    meta$trigger$value <- NULL
  }
  store_output_files(layout$deps_build$file_out, meta, config)
  if (length(file_out) || is.null(file_out)) {
    meta$output_file_hash <- output_file_hash(
      target = target, config = config)
  }
  meta$name <- target
  store_single_output(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
}

store_single_output <- function(target, value, meta, config) {
  if (meta$isfile) {
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
  finalize_storage(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
}

finalize_storage <- function(target, value, meta, config) {
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

store_file <- function(target, meta, config) {
  store_object(
    target = target,
    value = safe_rehash_file(target = target, config = config),
    meta = meta,
    config = config
  )
}

store_output_files <- function(files, meta, config) {
  meta$isfile <- TRUE
  for (file in files) {
    meta$name <- file
    meta$mtime <- file.mtime(decode_path(file))
    meta$isfile <- TRUE
    store_single_output(
      target = file,
      meta = meta,
      config = config
    )
  }
}

store_function <- function(target, value, meta, config) {
  config$cache$set(
    key = target,
    value = value,
    namespace = config$cache$default_namespace
  )
  # For the kernel of an imported function,
  # we ignore any code wrapped in ignore().
  if (meta$imported) {
    value <- ignore_ignore(value)
  }
  # Unfortunately, vectorization is removed, but this is for the best.
  string <- deparse(unwrap_function(value))
  if (meta$imported) {
    string <- c(string, meta$dependency_hash)
  }
  config$cache$set(
    key = target,
    value = string,
    namespace = "kernels"
  )
}

store_failure <- function(target, meta, config) {
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
