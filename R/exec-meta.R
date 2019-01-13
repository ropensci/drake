# Compute the initial pre-build metadata of a target or import.
drake_meta_ <- function(target, config) {
  layout <- config$layout[[target]]
  meta <- list(
    name = target,
    target = target,
    imported = layout$imported %||% TRUE,
    missing = !target_exists(target = target, config = config),
    seed = seed_from_basic_types(config$seed, target),
    time_start = proc.time(),
    file_out = layout$deps_build$file_out
  )
  if (meta$imported) {
    meta$isfile <- is_encoded_path(target)
    meta$trigger <- trigger(condition = TRUE)
  } else {
    meta$isfile <- FALSE
    meta$trigger <- as.list(layout$trigger)
  }
  # For imported files.
  if (meta$isfile) {
    meta$mtime <- file.mtime(decode_path(target, config))
  }
  if (meta$trigger$command) {
    meta$command <- layout$command_standardized
  }
  if (meta$trigger$depend) {
    meta$dependency_hash <- dependency_hash(target = target, config = config)
  }
  if (meta$trigger$file) {
    meta$input_file_hash <- input_file_hash(target = target, config = config)
    meta$output_file_hash <- output_file_hash(target = target, config = config)
  }
  if (!is.null(meta$trigger$change)) {
    try_load(layout$deps_change$memory, config = config)
    meta$trigger$value <- eval(meta$trigger$change, config$eval)
  }
  meta
}

dependency_hash <- function(target, config) {
  x <- config$layout[[target]]$deps_build
  deps <- c(x$globals, x$namespaced, x$loadd, x$readd)
  if (is_imported(target, config)) {
    deps <- c(deps, x$file_in, x$knitr_in)
  }
  if (!length(deps)) {
    return("")
  }
  deps <- unlist(deps)
  deps <- as.character(deps)
  deps <- unique(deps)
  deps <- sort(deps)
  out <- ht_memo(
    ht = config$ht_get_hash,
    x = deps,
    fun = self_hash,
    config = config
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$cache$driver$hash_algorithm,
    serialize = FALSE
  )
}

self_hash <- function(target, config) {
  # tryCatch is faster than checking if the key exists beforehand.
  tryCatch(
    config$cache$get_hash(target),
    error = error_na
  )
}

input_file_hash <- function(
  target,
  config,
  size_cutoff = rehash_file_size_cutoff
) {
  deps <- config$layout[[target]]$deps_build
  files <- sort(unique(as.character(c(deps$file_in, deps$knitr_in))))
  if (!length(files)) {
    return("")
  }
  out <- ht_memo(
    ht = config$ht_get_hash,
    x = files,
    fun = file_hash,
    config = config,
    size_cutoff = size_cutoff
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$cache$driver$hash_algorithm,
    serialize = FALSE
  )
}

output_file_hash <- function(
  target,
  config,
  size_cutoff = rehash_file_size_cutoff
) {
  deps <- config$layout[[target]]$deps_build
  files <- sort(unique(as.character(deps$file_out)))
  if (!length(files)) {
    return("")
  }
  out <- vapply(
    X = files,
    FUN = file_hash,
    FUN.VALUE = character(1),
    config = config,
    size_cutoff = size_cutoff
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$cache$driver$hash_algorithm,
    serialize = FALSE
  )
}

rehash_file <- function(target, config) {
  if (!is_encoded_path(target)) {
    return(NA_character_)
  }
  file <- decode_path(target, config)
  if (!file.exists(file) || file.info(file)$isdir) {
    return(NA_character_)
  }
  digest::digest(
    object = file,
    algo = config$cache$driver$hash_algorithm,
    file = TRUE,
    serialize = FALSE
  )
}

safe_rehash_file <- function(target, config) {
  if (file.exists(decode_path(target, config))) {
    rehash_file(target = target, config = config)
  } else {
    NA_character_
  }
}

should_rehash_file <- function(filename, new_mtime, old_mtime,
  size_cutoff) {
  do_rehash <- file.size(filename) < size_cutoff | new_mtime > old_mtime
  if (safe_is_na(do_rehash)) {
    do_rehash <- TRUE
  }
  do_rehash
}

file_hash <- function(
  target,
  config,
  size_cutoff = rehash_file_size_cutoff
) {
  if (!is_encoded_path(target)) {
    return(NA_character_)
  }
  filename <- decode_path(target, config)
  if (!file.exists(filename)) {
    return(NA_character_)
  }
  old_mtime <- ifelse(
    exists_in_subspace(
      key = target,
      subspace = "mtime",
      namespace = "meta",
      cache = config$cache
    ),
    get_from_subspace(
      key = target,
      subspace = "mtime",
      namespace = "meta",
      cache = config$cache
    ),
    -Inf
  )
  new_mtime <- file.mtime(filename)
  do_rehash <- should_rehash_file(
    filename = filename,
    new_mtime = new_mtime,
    old_mtime = old_mtime,
    size_cutoff = size_cutoff)
  old_hash_exists <- config$cache$exists(key = target)
  if (do_rehash || !old_hash_exists) {
    rehash_file(target = target, config = config)
  } else {
    config$cache$get(key = target)
  }
}
