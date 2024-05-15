store_outputs <- function(target, value, meta, config) {
  if (inherits(meta$error, "error")) {
    return()
  }
  config$logger$disk("store", target = target)
  invalidate_old_unused_subtargets(target, config)
  store_triggers(target, meta, config)
  meta$name <- target
  value <- decorate_format_value(value, target, config)
  store_item(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
  finalize_progress(target, config)
}

# #1260
invalidate_old_unused_subtargets <- function(target, config) { # nolint
  if (!is_dynamic(target, config)) {
    return()
  }
  if (!config$cache$exists(target, namespace = "meta")) {
    return()
  }
  old_subtargets <- config$cache$get(
    target,
    namespace = "meta",
    use_cache = FALSE
  )$subtargets
  current_subtargets <- config$spec[[target]]$subtargets
  invalid_subtargets <- setdiff(old_subtargets, current_subtargets)
  config$cache$del(invalid_subtargets)
}

decorate_format_value <- function(value, target, config) {
  UseMethod("decorate_format_value")
}

#' @export
decorate_format_value.default <- function(value, target, config) {
  value
}

#' @export
decorate_format_value.drake_format_file <- function(value, target, config) { # nolint
  path <- value$value
  hash <- rep(NA_character_, length(path))
  exists <- file.exists(path)
  if (any(!exists)) {
    msg <- paste0(
      "missing dynamic files for target ",
      target, ":\n", multiline_message(path[!exists])
    )
    warn0(msg)
    config$logger$disk(msg)
  }
  hash[exists] <- rehash_local(path[exists], config)
  value$hash <- hash
  value
}

undecorate_format_value <- function(value) {
  UseMethod("undecorate_format_value")
}

#' @export
undecorate_format_value.default <- function(value) { # nolint
  value
}

#' @export
undecorate_format_value.drake_format <- function(value) { # nolint
  value$value
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
  store_file_out_files(config$spec[[target]]$deps_build$file_out, meta, config)
}

store_file_out_files <- function(files, meta, config) {
  meta$isfile <- TRUE
  for (file in files) {
    meta$name <- file
    path <- config$cache$decode_path(file)
    meta$mtime <- storage_mtime(path)
    meta$size_storage <- storage_size(path)
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
    return("drake_static_storage")
  }
  if (is.function(value)) {
    return("drake_function")
  }
  if (is_storr(value)) {
    return("drake_storr")
  }
  "drake_object"
}

is_storr <- function(value) {
  inherits(value, "refclass_decorated_storr") ||
    inherits(value, "storr")
}

store_item_impl <- function(target, value, meta, config) {
  UseMethod("store_item_impl")
}

#' @export
store_item_impl.drake_static_storage <- function( # nolint
  target,
  value = NULL,
  meta,
  config
) {
  if (meta$imported) {
    value <- static_storage_hash(target = target, config = config)
  } else {
    value <- rehash_static_storage(target = target, config = config)
  }
  store_object(
    target = target,
    value = value,
    meta = meta,
    config = config
  )
}

#' @export
store_item_impl.drake_function <- function(target, value, meta, config) { # nolint
  if (meta$imported) {
    value <- standardize_imported_function(value)
    value <- c(value, meta$dependency_hash)
  }
  store_object(target, value, meta, config)
}

standardize_imported_function <- function(fun) {
  fun <- unwrap_function(fun)
  # Because the function body still has attributes.
  str <- safe_deparse(fun, backtick = TRUE)
  if (any(grepl("ignore", str, fixed = TRUE))) {
    fun <- ignore_ignore(fun)
    # Worth it. ignore_ignore is slow.
    str <- safe_deparse(fun, backtick = TRUE)
  }
  standardize_deparsed_function(str)
}

standardize_deparsed_function <- function(str) {
  gsub("<pointer: 0x[0-9a-zA-Z]*>", "", str)
}

#' @export
store_item_impl.drake_storr <- function(target, value, meta, config) {
  store_object(target, value = "storr", meta, config)
}

#' @export
store_item_impl.drake_object <- function(target, value, meta, config) {
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
  is_target <- !meta$imported && !meta$isfile
  if (is_target && is_history(config$cache$history)) {
    config$cache$history$push(title = target, message = meta_hash)
  }
  if (is_target && config$settings$recoverable) {
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
  meta <- finalize_times(target, meta, config)
  meta$time_start <- NULL
  meta$date <- microtime()
  if (!meta$imported && !is_encoded_path(target)) {
    log_time(target, meta, config)
  }
  meta$hash <- hash
  meta$size_vec <- safe_nrow(undecorate_format_value(value))
  if (is_dynamic(target, config)) {
    meta$subtargets <- config$spec[[target]]$subtargets
  }
  if (is_dynamic_dep(target, config)) {
    meta$dynamic_hashes <- dynamic_hashes(value, meta$size_vec, config)
  }
  meta <- decorate_format_meta(value, target, meta, config)
  meta
}

safe_nrow <- function(x) {
  tryCatch(NROW(x), error = function(e) length(x))
}

decorate_format_meta <- function(value, target, meta, config) {
  UseMethod("decorate_format_meta")
}

#' @export
decorate_format_meta.drake_format_file <- function( # nolint
  value,
  target,
  meta,
  config
) {
  path <- value$value
  meta$format_file_path <- path
  meta$format_file_hash <- value$hash
  meta$format_file_time <- storage_mtime(path)
  meta$format_file_size <- storage_size(path)
  meta
}

#' @export
decorate_format_meta.default <- function(value, target, meta, config) {
  meta
}

finalize_times <- function(target, meta, config) {
  if (config$settings$log_build_times) {
    meta$time_command <- runtime_entry(meta$time_command, target)
    meta$time_build <- runtime_entry(proc_time() - meta$time_start, target)
  } else {
    meta$time_command <- meta$time_build <- empty_times()
  }
  meta
}

finalize_triggers <- function(target, meta, config) {
  if (is_subtarget(target, config)) {
    return(meta)
  }
  spec <- config$spec[[target]]
  if (is.null(meta$command)) {
    meta$command <- spec$command_standardized
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

#' @export
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

#' @export
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
  config$cache$digest(subvalue)
}

log_time <- function(target, meta, config) {
  if (is.null(config$logger$file)) {
    return()
  }
  if (requireNamespace("lubridate", quietly = TRUE)) {
    exec <- round(lubridate::dseconds(meta$time_command$elapsed), 3)
    total <- round(lubridate::dseconds(meta$time_build$elapsed), 3)
    tail <- paste("", exec, ":", total, " (exec : total)")
  } else {
    tail <- " (install lubridate to print runtimes in the log)" # nocov
  }
  config$logger$disk("time", tail, target = target)
}

runtime_entry <- function(runtime, target) {
  list(
    target = as.character(target),
    elapsed = as.numeric(runtime["elapsed"]),
    user = as.numeric(runtime["user.self"]),
    system = as.numeric(runtime["sys.self"])
  )
}

microtime <- function() {
  format(Sys.time(), "%Y-%m-%d %H:%M:%OS9 %z GMT")
}

proc_time <- function() {
  unclass(proc.time())
}

# GitHub issue 1209
finalize_progress <- function(target, config) {
  set_progress(target = target, value = "done", config = config)
  if (is_dynamic(target, config)) {
    finalize_progress_dynamic(target, config)
  }
  if (is_subtarget(target, config)) {
    finalize_progress_subtarget(target, config)
  }
}

finalize_progress_dynamic <- function(target, config) {
  config$cache$clear_dynamic_progress(target)
}

finalize_progress_subtarget <- function(target, config) {
  parent <- config$spec[[target]]$subtarget_parent
  namespace <- config$meta[[parent]]$dynamic_progress_namespace
  config$cache$inc_dynamic_progress(target, namespace)
}
