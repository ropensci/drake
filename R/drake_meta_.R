drake_meta_ <- function(target, config) {
  layout <- config$layout[[target]]
  meta <- list(
    name = target,
    target = target,
    imported = layout$imported %||% TRUE,
    missing = !config$cache$exists(key = target),
    seed = as.integer(
      layout$seed %||NA% seed_from_basic_types(config$seed, target)
    ),
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
    path <- config$cache$decode_path(target)
    meta$mtime <- storage_mtime(path)
    meta$size <- storage_size(path)
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

# A numeric hash that could be used as a
# random number generator seed. Generated
# from arguments of basic types such as
# numerics and characters.
seed_from_basic_types <- function(...) {
  x <- paste0(..., collapse = "")
  hash <- digest::digest(x, algo = "murmur32", serialize = FALSE)
  hexval <- paste0("0x", hash)
  utils::type.convert(hexval) %% .Machine$integer.max
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
  out <- config$cache$memo_hash(
    x = deps,
    fun = self_hash,
    config = config
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$cache$hash_algorithm,
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

is_imported <- function(target, config) {
  config$layout[[target]]$imported %||% TRUE
}

input_file_hash <- function(
  target,
  config,
  size_threshold = rehash_storage_size_threshold
) {
  deps <- config$layout[[target]]$deps_build
  files <- sort(unique(as.character(c(deps$file_in, deps$knitr_in))))
  if (!length(files)) {
    return("")
  }
  out <- config$cache$memo_hash(
    x = files,
    fun = storage_hash,
    config = config,
    size_threshold = size_threshold
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$cache$hash_algorithm,
    serialize = FALSE
  )
}

output_file_hash <- function(
  target,
  config,
  size_threshold = rehash_storage_size_threshold
) {
  deps <- config$layout[[target]]$deps_build
  files <- sort(unique(as.character(deps$file_out)))
  if (!length(files)) {
    return("")
  }
  out <- vapply(
    X = files,
    FUN = storage_hash,
    FUN.VALUE = character(1),
    config = config,
    size_threshold = size_threshold
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = config$cache$hash_algorithm,
    serialize = FALSE
  )
}

storage_hash <- function(
  target,
  config,
  size_threshold = rehash_storage_size_threshold
) {
  if (!is_encoded_path(target)) {
    return(NA_character_)
  }
  file <- config$cache$decode_path(target)
  if (is_url(file)) {
    return(rehash_storage(target = target, file = file, config = config))
  }
  if (!file.exists(file)) {
    return(NA_character_)
  }
  not_cached <- !config$cache$exists(key = target) ||
    !config$cache$exists(key = target, namespace = "meta")
  if (not_cached) {
    return(rehash_storage(target = target, file = file, config = config))
  }
  meta <- config$cache$get(key = target, namespace = "meta")
  should_rehash <- should_rehash_storage(
    size_threshold = size_threshold,
    new_mtime = storage_mtime(file),
    old_mtime = meta$mtime %||% -Inf,
    new_size = storage_size(file),
    old_size = meta$size
  )
  ifelse(
    should_rehash,
    rehash_storage(target = target, config = config),
    config$cache$get(key = target)
  )
}

should_rehash_storage <- function(
  size_threshold,
  new_mtime,
  old_mtime,
  new_size,
  old_size
) {
  small <- (new_size < size_threshold) %||NA% TRUE
  touched <- (new_mtime > old_mtime) %||NA% TRUE
  resized <- (abs(new_size - old_size) > rehash_storage_size_tol) %||NA% TRUE
  small || touched || resized
}

rehash_storage_size_threshold <- 1e5
rehash_storage_size_tol <- .Machine$double.eps ^ 0.5

storage_mtime <- function(x) {
  if (dir.exists(x)) {
    dir_mtime(x)
  } else {
    file.mtime(x)
  }
}

dir_mtime <- function(x) {
  files <- list.files(
    path = x,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  times <- vapply(files, file.mtime, FUN.VALUE = numeric(1))
  max(times %||% Inf)
}

storage_size <- function(x) {
  if (dir.exists(x)) {
    dir_size(x)
  } else {
    file_size(x)
  }
}

dir_size <- function(x) {
  files <- list.files(
    path = x,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  sizes <- vapply(files, file_size, FUN.VALUE = numeric(1))
  max(sizes %||% 0)
}

file_size <- function(x) {
  if (file.exists(x)) {
    file.size(x)
  } else {
    NA_real_
  }
}

rehash_storage <- function(target, file = NULL, config) {
  if (!is_encoded_path(target)) {
    return(NA_character_)
  }
  if (is.null(file)) {
    file <- config$cache$decode_path(target)
  }
  if (is_url(file)) {
    return(rehash_url(url = file, config = config))
  }
  if (!file.exists(file)) {
    return(NA_character_)
  }
  rehash_local(file, hash_algorithm = config$cache$hash_algorithm)
}

rehash_local <- function(file, hash_algorithm) {
  if (dir.exists(file)) {
    rehash_dir(file, hash_algorithm)
  } else {
    rehash_file(file, hash_algorithm)
  }
}

rehash_dir <- function(dir, hash_algorithm) {
  files <- list.files(
    path = dir,
    all.files = TRUE,
    full.names = TRUE,
    recursive = TRUE,
    include.dirs = FALSE
  )
  out <- vapply(
    files,
    rehash_file,
    FUN.VALUE = character(1),
    hash_algorithm = hash_algorithm
  )
  out <- paste(out, collapse = "")
  digest::digest(
    out,
    algo = hash_algorithm,
    serialize = FALSE
  )
}

rehash_file <- function(file, hash_algorithm) {
  digest::digest(
    object = file,
    algo = hash_algorithm,
    file = TRUE,
    serialize = FALSE
  )
}

rehash_url <- function(url, config) {
  assert_pkg("curl")
  headers <- NULL
  if (!curl::has_internet()) {
    # Tested in tests/testthat/test-always-skipped.R.
    stop("no internet. Cannot check url: ", url, call. = FALSE) # nocov
  }
  # Find the longest name of the handle that matches the url.
  choices <- names(config$curl_handles)
  name <- longest_match(choices = choices, against = url) %||% NA_character_
  handle <- config$curl_handles[[name]] %||% curl::new_handle()
  # Do not download the whole URL.
  handle <- curl::handle_setopt(handle, nobody = TRUE)
  req <- curl::curl_fetch_memory(url, handle = handle)
  stopifnot(length(req$content) < 1L)
  headers <- curl::parse_headers_list(req$headers)
  assert_status_code(req, url)
  assert_useful_headers(headers, url)
  etag <- paste(headers[["etag"]], collapse = "")
  mtime <- paste(headers[["last-modified"]], collapse = "")
  return(paste(etag, mtime))
}

is_url <- function(x) {
  grepl("^http://|^https://|^ftp://", x)
}

assert_status_code <- function(req, url) {
  if (req$status_code != 200L) {
    stop("could not access url: ", url, call. = FALSE)
  }
}

assert_useful_headers <- function(headers, url) {
  if (!any(c("etag", "last-modified") %in% names(headers))) {
    stop("no ETag or Last-Modified for url: ", url, call. = FALSE)
  }
}
