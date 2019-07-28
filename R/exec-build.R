announce_build <- function(target, meta, config) {
  set_progress(
    target = target,
    meta = meta,
    value = "running",
    config = config
  )
  log_msg(
    "target",
    target,
    target = target,
    config = config,
    color = "target",
    tier = 1L
  )
}

set_progress <- function(target, meta, value, config) {
  skip_progress <- !identical(config$running_make, TRUE) ||
    !config$log_progress ||
    (meta$imported %||% FALSE)
  if (skip_progress) {
    return()
  }
  config$cache$driver$set_hash(
    key = target,
    namespace = "progress",
    hash = config$progress_hashmap[[value]]
  )
}

build_target <- function(target, meta, config) {
  if (identical(config$garbage_collection, TRUE)) {
    on.exit(gc())
  }
  retries <- 0L
  layout <- config$layout[[target]] %||% list()
  max_retries <- as.numeric(layout$retries %||NA% config$retries)
  while (retries <= max_retries) {
    if (retries > 0L) {
      log_msg(
        "retry",
        target,
        retries,
        "of",
        max_retries,
        target = target,
        config = config,
        color = "retry",
        tier = 1L
      )
    }
    build <- with_seed_timeout(
      target = target,
      meta = meta,
      config = config
    )
    if (!inherits(build$meta$error, "error")) {
      return(build)
    }
    retries <- retries + 1L
  }
  build
}

conclude_build <- function(build, config) {
  target <- build$target
  value <- build$value
  meta <- build$meta
  assert_output_files(target = target, meta = meta, config = config)
  handle_build_exceptions(target = target, meta = meta, config = config)
  store_outputs(target = target, value = value, meta = meta, config = config)
  assign_to_envir(target = target, value = value, config = config)
  invisible(value)
}

assert_output_files <- function(target, meta, config) {
  deps <- config$layout[[target]]$deps_build
  if (!length(deps$file_out)) {
    return()
  }
  files <- unique(as.character(deps$file_out))
  files <- decode_path(files, config)
  missing_files <- files[!file.exists(files)]
  if (length(missing_files)) {
    msg <- paste0(
      "Missing files for target ",
      target, ":\n",
      multiline_message(missing_files)
    )
    drake_log(paste("Warning:", msg), config = config)
    warning(msg, call. = FALSE)
  }
}

handle_build_exceptions <- function(target, meta, config) {
  if (length(meta$warnings) && config$verbose) {
    warn_opt <- max(1, getOption("warn"))
    with_options(
      new = list(warn = warn_opt),
      warning(
        "target ", target, " warnings:\n",
        multiline_message(meta$warnings),
        call. = FALSE
      )
    )
  }
  if (length(meta$messages) && config$verbose) {
    message(
      "Target ", target, " messages:\n",
      multiline_message(meta$messages)
    )
  }
  if (inherits(meta$error, "error")) {
    log_msg(
      "fail",
      target,
      target = target,
      config = config,
      color = "fail",
      tier = 1L
    )
    store_failure(target = target, meta = meta, config = config)
    if (!config$keep_going) {
      msg <- paste0(
        "Target `", target, "` failed. Call `diagnose(", target,
        ")` for details. Error message:\n  ",
        meta$error$message
      )
      drake_log(paste("Error:", msg), config = config)
      stop(msg, call. = FALSE)
    }
  }
}

assign_to_envir <- function(target, value, config) {
  memory_strategy <- config$layout[[target]]$memory_strategy %||NA%
    config$memory_strategy
  if (memory_strategy %in% c("autoclean", "unload", "none")) {
    return()
  }
  if (
    identical(config$lazy_load, "eager") &&
    !is_encoded_path(target) &&
    !is_imported(target, config)
  ) {
    assign(x = target, value = value, envir = config$eval)
  }
  invisible()
}
