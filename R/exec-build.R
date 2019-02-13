announce_build <- function(target, meta, config) {
  set_progress(
    target = target,
    meta = meta,
    value = "running",
    config = config
  )
  console(imported = meta$imported, target = target, config = config)
}

build_target <- function(target, meta, config) {
  if (identical(config$garbage_collection, TRUE)) {
    on.exit(gc())
  }
  retries <- 0
  layout <- config$layout[[target]] %||% list()
  max_retries <- as.numeric(layout$retries %||NA% config$retries)
  while (retries <= max_retries) {
    build <- with_seed_timeout(
      target = target,
      meta = meta,
      config = config
    )
    if (!inherits(build$meta$error, "error")) {
      return(build)
    }
    retries <- retries + 1
    console_retry(target = target, retries = retries, config = config)
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
    drake_warning(
      "Missing files for target ", target, ":\n",
      multiline_message(missing_files),
      config = config
    )
  }
}
