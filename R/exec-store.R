store_outputs <- function(target, value, meta, config) {
  # Failed targets need to stay invalidated,
  # even when `config$keep_going` is `TRUE`.
  if (inherits(meta$error, "error")) {
    return()
  }
  log_msg("store", target, config = config)
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
      key = target,
      value = meta$trigger$value,
      namespace = "change",
      use_cache = FALSE
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
  set_progress(
    target = target,
    meta = meta,
    value = "done",
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
    meta = meta,
    config = config
  )
}

finalize_storage <- function(target, meta, config) {
  meta <- finalize_times(
    target = target,
    meta = meta,
    config = config
  )
  config$cache$set(
    key = target,
    value = meta,
    namespace = "meta",
    use_cache = FALSE
  )
  if (!meta$imported && !is_encoded_path(target)) {
    console_time(target, meta, config)
  }
}

store_object <- function(target, value, meta, config) {
  config$cache$set(key = target, value = value, use_cache = FALSE)
}

store_file <- function(target, meta, config) {
  if (meta$imported) {
    value <- storage_hash(target = target, config = config)
  } else {
    value <- rehash_storage(target = target, config = config)
  }
  store_object(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
}

store_output_files <- function(files, meta, config) {
  meta$isfile <- TRUE
  for (file in files) {
    meta$name <- file
    meta$mtime <- storage_mtime(decode_path(file, config))
    meta$isfile <- TRUE
    store_single_output(
      target = file,
      meta = meta,
      config = config
    )
  }
}

store_function <- function(target, value, meta, config) {
  if (meta$imported) {
    value <- standardize_imported_function(value)
    value <- c(value, meta$dependency_hash)
  }
  store_object(target, value, meta, config)
}

store_failure <- function(target, meta, config) {
  set_progress(
    target = target,
    meta = meta,
    value = "failed",
    config = config
  )
  fields <- intersect(c("messages", "warnings", "error"), names(meta))
  meta <- meta[fields]
  config$cache$set(
    key = target,
    value = meta,
    namespace = "meta",
    use_cache = FALSE
  )
}
