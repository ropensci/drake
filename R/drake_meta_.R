drake_meta_ <- function(target, config) {
  is_subtarget <- is_subtarget(target, config) %|||% FALSE
  class <- ifelse(is_subtarget, "subtarget", "target")
  class(target) <- class
  drake_meta_impl(target, config)
}

drake_meta_impl <- function(target, config) {
  UseMethod("drake_meta_impl")
}

drake_meta_impl.subtarget <- function(target, config) {
  out <- list(
    name = target,
    target = target,
    imported = FALSE,
    isfile = FALSE,
    seed = resolve_target_seed(target, config)
  )
  if (config$log_build_times) {
    out$time_start <- proc_time()
  }
  out
}

drake_meta_impl.default <- function(target, config) {
  spec <- config$spec[[target]]
  meta <- list(
    name = target,
    target = target,
    imported = spec$imported %|||% TRUE,
    missing = target_missing(target, config),
    file_out = spec$deps_build$file_out,
    format = spec$format %||NA% "none",
    dynamic = is.call(spec$dynamic)
  )
  if (config$log_build_times) {
    meta$time_start <- proc_time()
  }
  if (meta$imported) {
    meta$isfile <- is_encoded_path(target)
    meta$trigger <- trigger(condition = TRUE)
  } else {
    meta$isfile <- FALSE
    meta$trigger <- as.list(spec$trigger)
    meta$seed <- resolve_target_seed(target, config)
  }
  # For imported files.
  if (meta$isfile) {
    path <- config$cache$decode_path(target)
    meta$mtime <- storage_mtime(path)
    meta$size_storage <- storage_size(path)
  }
  if (meta$trigger$command) {
    meta$command <- spec$command_standardized
  }
  if (meta$trigger$depend) {
    meta$dependency_hash <- dependency_hash(target = target, config = config)
  }
  if (meta$trigger$file) {
    meta$input_file_hash <- input_file_hash(target = target, config = config)
    meta$output_file_hash <- output_file_hash(target = target, config = config)
  }
  if (!is.null(meta$trigger$change)) {
    try_load_deps(spec$deps_change$memory, config = config)
    meta$trigger$value <- eval(meta$trigger$change, config$envir_targets)
  }
  if (is_dynamic(target, config)) {
    meta$dynamic_dependency_hash <- dynamic_dependency_hash(target, config)
    meta$max_expand <- config$max_expand
  }
  meta
}

target_missing <- function(target, config) {
  !target_exists(target, config)
}

target_exists <- function(target, config) {
  if (is.null(config$ht_target_exists)) {
    target_exists_slow(target, config)
  } else {
    target_exists_fast(target, config)
  }
}

target_exists_slow <- function(target, config) {
  config$cache$exists(key = target) &
    config$cache$exists(key = target, namespace = "meta")
}

target_exists_single <- function(target, config) {
  ht_exists(ht = config$ht_target_exists, x = target)
}

target_exists_fast <- Vectorize(
  target_exists_single,
  vectorize.args = "target",
  USE.NAMES = FALSE
)

resolve_target_seed <- function(target, config) {
  seed <- config$spec[[target]]$seed
  if (is.null(seed) || is.na(seed)) {
    seed <- seed_from_basic_types(config$seed, target)
  }
  as.integer(seed)
}

# A numeric hash that could be used as a
# random number generator seed. Generated
# from arguments of basic types such as
# numerics and characters.
seed_from_basic_types <- function(...) {
  x <- paste0(..., collapse = "")
  integer_hash(x = x, mod = .Machine$integer.max)
}

integer_hash <- function(x, mod = .Machine$integer.max) {
  hash <- digest_murmur32(x, serialize = FALSE)
  hexval <- paste0("0x", hash)
  as.integer(type.convert(hexval) %% mod)
}

dependency_hash <- function(target, config) {
  spec <- config$spec[[target]]
  x <- spec$deps_build
  deps <- c(x$globals, x$namespaced, x$loadd, x$readd)
  if (is_imported(target, config)) {
    deps <- c(deps, x$file_in, x$knitr_in)
  }
  deps <- setdiff(deps, spec$deps_dynamic)
  if (!length(deps)) {
    return("")
  }
  deps <- unlist(deps)
  deps <- as.character(deps)
  deps <- unique(deps)
  deps <- sort(deps)
  dependency_hash_impl(deps, config)
}

dynamic_dependency_hash <- function(target, config) {
  spec <- config$spec[[target]]
  deps_dynamic <- spec$deps_dynamic
  deps_trace <- sort(unique(spec$deps_dynamic_trace))
  deps <- c(deps_dynamic, deps_trace)
  dependency_hash_impl(deps, config)
}

dependency_hash_impl <- function(deps, config) {
  out <- config$cache$memo_hash(
    x = deps,
    fun = self_hash,
    config = config
  )
  out <- paste(out, collapse = "")
  config$cache$digest(out, serialize = FALSE)
}

self_hash <- function(target, config) {
  # tryCatch is faster than checking if the key exists beforehand.
  tryCatch(
    config$cache$get_hash(target),
    error = error_na
  )
}

is_imported <- function(target, config) {
  config$spec[[target]]$imported %|||% TRUE
}

input_file_hash <- function(
  target,
  config,
  size_threshold = rehash_storage_size_threshold
) {
  deps <- config$spec[[target]]$deps_build
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
  config$cache$digest(out, serialize = FALSE)
}

output_file_hash <- function(
  target,
  config,
  size_threshold = rehash_storage_size_threshold
) {
  deps <- config$spec[[target]]$deps_build
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
  config$cache$digest(out, serialize = FALSE)
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
  if (target_missing(target, config)) {
    return(rehash_storage(target = target, file = file, config = config))
  }
  meta <- config$cache$get(key = target, namespace = "meta")
  should_rehash <- should_rehash_storage(
    size_threshold = size_threshold,
    new_mtime = storage_mtime(file),
    old_mtime = as.numeric(meta$mtime %|||% -Inf),
    new_size = storage_size(file),
    old_size = meta$size_storage %|||% -1L
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
  small <- (new_size < size_threshold) %|||NA% TRUE
  touched <- (new_mtime > old_mtime) %|||NA% TRUE
  resized <- (abs(new_size - old_size) > rehash_storage_size_tol) %|||NA% TRUE
  small || touched || resized
}

rehash_storage_size_threshold <- 1e5
rehash_storage_size_tol <- .Machine$double.eps ^ 0.5

storage_mtime <- function(x) {
  if (dir.exists(x)) {
    dir_mtime(x)
  } else {
    file_mtime(x)
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
  times <- vapply(files, file_mtime, FUN.VALUE = numeric(1))
  max(times %||% Inf)
}

file_mtime <- function(x) {
  as.numeric(file.mtime(x))
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
  sum(sizes %||% 0)
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
  rehash_local(file, config)
}

rehash_local <- function(file, config) {
  if (dir.exists(file)) {
    rehash_dir(file, config)
  } else {
    rehash_file(file, config)
  }
}

rehash_dir <- function(dir, config) {
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
    config = config
  )
  out <- paste(out, collapse = "")
  config$cache$digest(out, serialize = FALSE)
}

rehash_file <- function(file, config) {
  config$cache$digest(object = file, file = TRUE, serialize = FALSE)
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
  handle <- config$curl_handles[[name]] %|||% curl::new_handle()
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
