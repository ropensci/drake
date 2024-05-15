drake_meta_ <- function(target, config) {
  if (exists(target, envir = config$meta, inherits = FALSE)) {
    return(config$meta[[target]])
  }
  set_drake_meta(target, config)
  config$meta[[target]]
}

drake_meta_old <- function(target, config) {
  if (exists(target, envir = config$meta_old, inherits = FALSE)) {
    return(config$meta_old[[target]])
  }
  set_drake_meta_old(target, config)
  config$meta_old[[target]]
}

set_drake_meta <- function(target, config) {
  class(target) <- drake_meta_class(target, config)
  meta <- drake_meta_impl(target, config)
  set_drake_meta_old(target, config)
  meta <- subsume_old_meta(target, meta, config)
  class(meta) <- c("drake_meta", "drake")
  config$meta[[target]] <- meta
  NULL
}

set_drake_meta_old <- function(target, config) {
  if (target_exists(target, config)) {
    meta_old <- config$cache$get(
      key = target,
      namespace = "meta",
      use_cache = FALSE
    )
    config$meta_old[[target]] <- meta_old
  }
}

#' @export
print.drake_meta <- function(x, ...) {
  cat("drake metadata for ", display_key(x$name), ":\n", sep = "")
  elts <- names(x)
  long <- c("command", "date")
  lsts <- c("trigger", "time_start", "time_build", "time_command")
  list1 <- x[setdiff(elts, c(long, lsts))]
  list2 <- x[intersect(elts, long)]
  list2 <- lapply(list2, crop_text, width = getOption("width") - 18L)
  list3 <- x[intersect(elts, lsts)]
  str(list1, no.list = TRUE)
  str(list2, no.list = TRUE)
  min_str(list3)
}

drake_meta_class <- function(target, config) {
  spec <- config$spec[[target]]
  if (is_subtarget(target, config)) {
    return("subtarget")
  }
  if (is_dynamic(target, config)) {
    return("dynamic")
  }
  if (is_encoded_path(target)) {
    return("imported_file")
  }
  is_imported <- is_encoded_namespaced(target) || (spec$imported %|||% TRUE)
  if (is_imported) {
    return("imported_object")
  }
  "static"
}

drake_meta_impl <- function(target, config) {
  UseMethod("drake_meta_impl")
}

#' @export
drake_meta_impl.imported_file <- function(target, config) { # nolint
  spec <- config$spec[[target]]
  meta <- list(
    name = target,
    target = target,
    imported = TRUE,
    isfile = TRUE,
    format = "none",
    dynamic = FALSE,
    missing = target_missing(target, config)
  )
  path <- config$cache$decode_path(target)
  meta$mtime <- storage_mtime(path)
  meta$size_storage <- storage_size(path)
  spec$trigger <- trigger(condition = TRUE)
  meta <- decorate_trigger_meta(target, meta, spec, config)
  meta
}

#' @export
drake_meta_impl.imported_object <- function(target, config) { # nolint
  spec <- config$spec[[target]]
  meta <- list(
    name = target,
    target = target,
    imported = TRUE,
    isfile = FALSE,
    dynamic = FALSE,
    format = "none",
    missing = target_missing(target, config),
    file_out = spec$deps_build$file_out
  )
  spec$trigger <- trigger(condition = TRUE)
  meta <- decorate_trigger_meta(target, meta, spec, config)
  meta
}

#' @export
drake_meta_impl.subtarget <- function(target, config) {
  parent_spec <- config$spec[[subtarget_parent(target, config)]]
  list(
    name = target,
    target = target,
    imported = FALSE,
    isfile = FALSE,
    dynamic = FALSE,
    format = parent_spec$format %||NA% "none",
    seed = resolve_target_seed(target, config),
    time_start = drake_meta_start(config),
    trigger = as.list(parent_spec$trigger)
  )
}

#' @export
drake_meta_impl.dynamic <- function(target, config) {
  spec <- config$spec[[target]]
  meta <- list(
    name = target,
    target = target,
    imported = FALSE,
    isfile = FALSE,
    dynamic = TRUE,
    format = spec$format %||NA% "none",
    missing = target_missing(target, config),
    seed = resolve_target_seed(target, config),
    time_start = drake_meta_start(config),
    dynamic_dependency_hash = dynamic_dependency_hash(target, config),
    max_expand = spec$max_expand %||NA% config$max_expand
  )
  meta <- decorate_trigger_meta(target, meta, spec, config)
  meta$dynamic_progress_namespace <- dynamic_progress_namespace(
    target,
    meta,
    config
  )
  meta
}

# GitHub issue 1209
dynamic_progress_namespace <- function(target, meta, config) {
  prefix <- dynamic_progress_ns_pfx(target)
  key <- dynamic_progress_key(target, meta, config)
  paste0(prefix, key)
}

dynamic_progress_key <- function(target, meta, config) {
  x <- dynamic_progress_prekey(target, meta, config)
  x <- paste(as.character(x), collapse = "|")
  digest_murmur32(x, serialize = FALSE)
}

dynamic_progress_prekey <- function(target, meta, config) {
 command <- ifelse(
    meta$trigger$command,
    meta$command,
    NA_character_
  )
  depend <- ifelse(
    meta$trigger$depend,
    meta$dependency_hash,
    NA_character_
  )
  input_file_hash <- ifelse(
    meta$trigger$file,
    meta$input_file_hash,
    NA_character_
  )
  output_file_hash <- ifelse(
    meta$trigger$file,
    meta$output_file_hash,
    NA_character_
  )
  seed <- ifelse(
    meta$trigger$seed,
    as.character(meta$seed),
    NA_character_
  )
  format <- ifelse(
    meta$trigger$format,
    meta$format,
    NA_character_
  )
  condition <- safe_deparse(meta$trigger$condition, backtick = TRUE)
  mode <- meta$trigger$mode
  change_hash <- ifelse(
    is.null(meta$trigger$value),
    NA_character_,
    config$cache$digest(meta$trigger$value)
  )
  list(
    command = command,
    depend = depend,
    input_file_hash = input_file_hash,
    output_file_hash = output_file_hash,
    seed = seed,
    format = format,
    condition = condition,
    mode = mode,
    change_hash = change_hash
  )
}

dynamic_progress_ns_pfx <- function(target) {
  paste0("dyn-", target, "-")
}

#' @export
drake_meta_impl.static <- function(target, config) {
  spec <- config$spec[[target]]
  meta <- list(
    name = target,
    target = target,
    imported = FALSE,
    isfile = FALSE,
    dynamic = FALSE,
    format = spec$format %||NA% "none",
    missing = target_missing(target, config),
    file_out = spec$deps_build$file_out,
    seed = resolve_target_seed(target, config),
    time_start = drake_meta_start(config)
  )
  meta <- decorate_trigger_meta(target, meta, spec, config)
  meta
}

decorate_trigger_meta <- function(target, meta, spec, config) {
  meta$trigger <- as.list(spec$trigger)
  meta$command <- spec$command_standardized
  meta$dependency_hash <- static_dependency_hash(target, config)
  meta$input_file_hash <- input_file_hash(target = target, config = config)
  meta$output_file_hash <- output_file_hash(target = target, config = config)
  if (!is.null(meta$trigger$change)) {
    try_load_deps(spec$deps_change$memory, config = config)
    meta$trigger$value <- eval(meta$trigger$change, config$envir_targets)
  }
  meta
}

subsume_old_meta <- function(target, meta, config) {
  if (!is_dynamic(target, config)) {
    class(target) <- meta$format
    meta <- decorate_trigger_format_meta(target, meta, config)
  }
  meta
}

decorate_trigger_format_meta <- function(target, meta, config) {
  UseMethod("decorate_trigger_format_meta")
}

#' @export
decorate_trigger_format_meta.default <- function(target, meta, config) { # nolint
  meta
}

#' @export
decorate_trigger_format_meta.file <- function(target, meta, config) { # nolint
  meta_old <- config$meta_old[[target]]
  if (is.null(meta_old) || !meta$trigger$file) {
    return(meta)
  }
  path <- as.character(meta_old$format_file_path)
  new_mtime <- storage_mtime(path)
  new_size <- storage_size(path)
  hash <- as.character(meta_old$format_file_hash)
  exists <- file.exists(path)
  hash[!exists] <- ""
  should_rehash <- exists & should_rehash_local(
    size_threshold = rehash_storage_size_threshold,
    new_mtime = new_mtime,
    old_mtime = as.numeric(meta_old$format_file_time),
    new_size = new_size,
    old_size = as.numeric(meta_old$format_file_size)
  )
  hash[should_rehash] <- rehash_local(path[should_rehash], config)
  meta$format_file_path <- path
  meta$format_file_hash <- hash
  meta$format_file_time <- new_mtime
  meta$format_file_size <- new_size
  meta
}

drake_meta_start <- function(config) {
  if (config$settings$log_build_times) {
    proc_time()
  }
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

target_exists_fast_list <- Vectorize(
  target_exists_single,
  vectorize.args = "target",
  USE.NAMES = FALSE
)

target_exists_fast <- function(target, config) {
  out <- target_exists_fast_list(target, config)
  as.logical(out)
}

resolve_target_seed <- function(target, config) {
  seed <- config$spec[[target]]$seed
  if (is.null(seed) || is.na(seed)) {
    seed <- seed_from_basic_types(config$settings$seed, target)
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
  as.integer(type.convert(hexval, as.is = TRUE) %% mod)
}

static_dependency_hash <- function(target, config) {
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
    fun = static_storage_hash,
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
    FUN = static_storage_hash,
    FUN.VALUE = character(1),
    config = config,
    size_threshold = size_threshold
  )
  out <- paste(out, collapse = "")
  config$cache$digest(out, serialize = FALSE)
}

static_storage_hash <- function(
  target,
  config,
  size_threshold = rehash_storage_size_threshold
) {
  if (!is_encoded_path(target)) {
    return(NA_character_)
  }
  file <- config$cache$decode_path(target)
  if (is_url(file)) {
    return(rehash_static_storage(target, file, config))
  }
  if (!file.exists(file)) {
    return(NA_character_)
  }
  if (target_missing(target, config)) {
    return(rehash_static_storage(target, file, config))
  }
  meta <- config$cache$get(key = target, namespace = "meta")
  should_rehash <- should_rehash_local(
    size_threshold = size_threshold,
    new_mtime = storage_mtime(file),
    old_mtime = as.numeric(meta$mtime %|||% -Inf),
    new_size = storage_size(file),
    old_size = meta$size_storage %|||% -1L
  )
  ifelse(
    should_rehash,
    rehash_static_storage(target = target, config = config),
    config$cache$get(key = target)
  )
}

should_rehash_local_impl <- function(
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

should_rehash_local_list <- Vectorize(
  should_rehash_local_impl,
  vectorize.args = c("new_mtime", "old_mtime", "new_size", "old_size"),
  USE.NAMES = FALSE
)

should_rehash_local <- function(
  size_threshold,
  new_mtime,
  old_mtime,
  new_size,
  old_size
) {
  out <- should_rehash_local_list(
    size_threshold = size_threshold,
    new_mtime = new_mtime,
    old_mtime = old_mtime,
    new_size = new_size,
    old_size = old_size
  )
  as.logical(out)
}

rehash_storage_size_threshold <- 1e5
rehash_storage_size_tol <- .Machine$double.eps ^ 0.5

storage_mtime_impl <- function(x) {
  ifelse(dir.exists(x), dir_mtime(x), file_mtime(x))
}

storage_mtime_list <- Vectorize(
  storage_mtime_impl,
  vectorize.args = "x",
  USE.NAMES = FALSE
)

storage_mtime <- function(x) {
  as.numeric(storage_mtime_list(x))
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

storage_size_impl <- function(x) {
  ifelse(dir.exists(x), dir_size(x), file_size(x))
}

storage_size_list <- Vectorize(
  storage_size_impl,
  vectorize.args = "x",
  USE.NAMES = FALSE
)

storage_size <- function(x) {
  as.numeric(storage_size_list(x))
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

rehash_static_storage <- function(target, file = NULL, config) {
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

rehash_local_impl <- function(file, config) {
  ifelse(dir.exists(file), rehash_dir(file, config), rehash_file(file, config))
}

rehash_local_list <- Vectorize(
  rehash_local_impl,
  vectorize.args = "file",
  USE.NAMES = FALSE
)

rehash_local <- function(file, config) {
  as.character(rehash_local_list(file, config))
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
    stop0("no internet. Cannot check url: ", url) # nocov
  }
  # Find the longest name of the handle that matches the url.
  choices <- names(config$settings$curl_handles)
  name <- longest_match(choices = choices, against = url) %||% NA_character_
  handle <- config$settings$curl_handles[[name]] %|||% curl::new_handle()
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

longest_match <- function(choices, against) {
  index <- vapply(
    choices,
    pmatch,
    table = against,
    FUN.VALUE = integer(1)
  )
  matches <- names(index[!is.na(index)])
  matches[which.max(nchar(matches))]
}

is_url <- function(x) {
  grepl("^http://|^https://|^ftp://", x)
}

assert_status_code <- function(req, url) {
  if (req$status_code != 200L) {
    stop0("could not access url: ", url)
  }
}

assert_useful_headers <- function(headers, url) {
  if (!any(c("etag", "last-modified") %in% names(headers))) {
    stop0("no ETag or Last-Modified for url: ", url)
  }
}
