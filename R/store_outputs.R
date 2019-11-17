store_outputs <- function(target, value, meta, config) {
  if (inherits(meta$error, "error")) {
    return()
  }
  config$logger$minor("store", target = target)
  store_triggers(target, meta, config)
  meta$name <- target
  store_item(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
  set_progress(
    target = target,
    value = "done",
    config = config
  )
}

store_triggers <- function(target, meta, config) {
  if (is_subtarget(target, config)) {
    return()
  }
  if (!is.null(meta$trigger$change)) {
    config$cache$set(
      key = target,
      value = meta$trigger$value,
      namespace = "change",
      use_cache = FALSE
    )
  }
  store_output_files(config$layout[[target]]$deps_build$file_out, meta, config)
}

store_output_files <- function(files, meta, config) {
  meta$isfile <- TRUE
  for (file in files) {
    meta$name <- file
    meta$mtime <- storage_mtime(config$cache$decode_path(file))
    meta$isfile <- TRUE
    store_item(
      target = file,
      value = NULL,
      meta = meta,
      config = config
    )
  }
}

store_item <- function(target, value, meta, config) {
  class(target) <- output_type(value = value, meta = meta)
  hash <- store_item_impl(target, value, meta, config)
  store_meta(
    target = target,
    value = value,
    meta = meta,
    hash = hash,
    config = config
  )
}

output_type <- function(value, meta) {
  if (meta$isfile) {
    return("file")
  }
  if (is.function(value)) {
    return("function")
  }
  if (is_storr(value)) {
    return("storr")
  }
  "object"
}

is_storr <- function(value) {
  inherits(value, "refclass_decorated_storr") ||
    inherits(value, "storr")
}

store_item_impl <- function(target, value, meta, config) {
  UseMethod("store_item_impl")
}

store_item_impl.file <- function(target, value = NULL, meta, config) {
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

store_item_impl.function <- function(target, value, meta, config) {
  if (meta$imported) {
    value <- standardize_imported_function(value)
    value <- c(value, meta$dependency_hash)
  }
  store_object(target, value, meta, config)
}

standardize_imported_function <- function(fun) {
  fun <- unwrap_function(fun)
  str <- safe_deparse(fun) # Because the function body still has attributes.
  if (any(grepl("ignore", str, fixed = TRUE))) {
    fun <- ignore_ignore(fun)
    str <- safe_deparse(fun) # Worth it: ignore_ignore is slow.
  }
  standardize_deparsed_function(str)
}

standardize_deparsed_function <- function(str) {
  gsub("<pointer: 0x[0-9a-zA-Z]*>", "", str)
}

store_item_impl.storr <- function(target, value, meta, config) {
  store_object(target, value = "storr", meta, config)
}

store_item_impl.object <- function(target, value, meta, config) {
  store_object(target, value, meta, config)
}

store_object <- function(target, value, meta, config) {
  config$cache$set(key = target, value = value, use_cache = FALSE)
}

store_meta <- function(target, value, meta, hash, config) {
  meta <- finalize_meta(
    target = target,
    value = value,
    meta = meta,
    hash = hash,
    config = config
  )
  meta_lite <- meta
  meta_lite$trigger$value <- NULL
  meta_hash <- config$cache$set(
    key = target,
    value = meta_lite,
    namespace = "meta",
    use_cache = FALSE
  )
  is_target <- !meta$imported && !is_encoded_path(target)
  if (is_target && is_history(config$cache$history)) {
    config$cache$history$push(title = target, message = meta_hash)
  }
  if (is_target && config$recoverable) {
    store_recovery(target, meta, meta_hash, config)
  }
}

store_recovery <- function(target, meta, meta_hash, config) {
  key <- recovery_key(target = target, meta = meta, config = config)
  if (!is.na(key)) {
    config$cache$driver$set_hash(
      key = key,
      namespace = "recover",
      hash = meta_hash
    )
  }
}

finalize_meta <- function(target, value, meta, hash, config) {
  meta <- finalize_triggers(target, meta, config)
  meta$time_command <- runtime_entry(
    runtime = meta$time_command,
    target = target
  )
  meta$time_build <- runtime_entry(
    runtime = proc.time() - meta$time_start,
    target = target
  )
  meta$time_start <- NULL
  meta$date <- microtime()
  if (!meta$imported && !is_encoded_path(target)) {
    log_time(target, meta, config)
  }
  meta$hash <- hash
  meta$size <- NROW(value)
  if (is_dynamic(target, config)) {
    meta$subtargets <- config$layout[[target]]$subtargets
  }
  if (is_dynamic_dep(target, config)) {
    meta$dynamic_hashes <- dynamic_hashes(value, meta$size, config)
  }
  meta
}

finalize_triggers <- function(target, meta, config) {
  if (is_subtarget(target, config)) {
    return(meta)
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
  if (length(file_out) || is.null(file_out)) {
    meta$output_file_hash <- output_file_hash(
      target = target,
      config = config
    )
  }
  meta
}

dynamic_hashes <- function(value, size, config) {
  UseMethod("dynamic_hashes")
}

dynamic_hashes.drake_dynamic <- function(value, size, config) {
  vapply(
    seq_len(size),
    function(value, index) {
      dynamic_subvalue(value = value, index = index)
    },
    FUN.VALUE = character(1),
    value = value
  )
}

dynamic_hashes.default <- function(value, size, config) {
  vapply(
    seq_len(size),
    dynamic_hash,
    FUN.VALUE = character(1),
    value = value,
    config = config
  )
}

dynamic_hash <- function(index, value, config) {
  subvalue <- dynamic_subvalue(value, index)
  digest::digest(subvalue, algo = config$cache$hash_algorithm)
}

log_time <- function(target, meta, config) {
  if (is.null(config$logger$file)) {
    return()
  }
  if (requireNamespace("lubridate", quietly = TRUE)) {
    exec <- round(lubridate::dseconds(meta$time_command$elapsed), 3)
    total <- round(lubridate::dseconds(meta$time_build$elapsed), 3)
    tail <- paste("", exec, "|", total, " (exec | total)")
  } else {
    tail <- " (install lubridate to print runtimes in the log)" # nocov
  }
  config$logger$minor("time", tail, target = target)
}

runtime_entry <- function(runtime, target) {
  list(
    target = target,
    elapsed = runtime[["elapsed"]],
    user = runtime[["user.self"]],
    system = runtime[["sys.self"]]
  )
}

microtime <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS9 %z GMT")
}
