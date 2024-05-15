decorate_storr <- function(storr) {
  if (inherits(storr, "refclass_decorated_storr")) {
    return(storr)
  }
  if (!inherits(storr, "storr")) {
    stop0("not a storr")
  }
  hash_algorithm <- storr$driver$hash_algorithm %||% "xxhash64"
  digest <- new_digest_function(hash_algorithm)
  path <- storr$driver$path %||% default_cache_path()
  refclass_decorated_storr$new(
    storr = storr,
    driver = storr$driver,
    default_namespace = storr$default_namespace,
    envir = storr$envir,
    hash_algorithm = hash_algorithm,
    digest = digest,
    history = recover_default_history(path),
    ht_encode_path = ht_new(),
    ht_decode_path = ht_new(),
    ht_encode_namespaced = ht_new(),
    ht_decode_namespaced = ht_new(),
    ht_hash = ht_new(),
    ht_keys = ht_keys(digest),
    path = path,
    path_return = file.path(path, "drake", "return"),
    path_tmp = file.path(path, "drake", "tmp")
  )
}

new_digest_function <- function(hash_algorithm) {
  inner_digest <- digest::getVDigest(algo = hash_algorithm)
  digest <- function(object, serialize = TRUE, ...) {
    if (serialize) {
      suppressWarnings(inner_digest(list(object), serialize = TRUE, ...))
    } else {
      suppressWarnings(inner_digest(object, serialize = FALSE, ...))
    }
  }
}

digest_murmur32 <- new_digest_function("murmur32")

refclass_decorated_storr <- methods::setRefClass(
  Class = "refclass_decorated_storr",
  fields = c(
    "storr",
    "driver",
    "default_namespace",
    "envir",
    "hash_algorithm",
    "digest",
    "history",
    "ht_encode_path",
    "ht_decode_path",
    "ht_encode_namespaced",
    "ht_decode_namespaced",
    "ht_hash",
    "ht_keys",
    "path",
    "path_return",
    "path_tmp"
  ),
  # Tedious, but better than inheritance, which would
  # prevent users from supplying their own true `storr`s.
  methods = list(
    # Custom:
    assert_dirs = function() {
      dir_create(.self$path_return)
      dir_create(.self$path_tmp)
    },
    file_return_hash = function(hash) {
      file.path(.self$path_return, hash)
    },
    file_return_key = function(key) {
      hash <- .self$get_hash(key)
      .self$file_return_hash(hash)
    },
    file_tmp = function() {
      file.path(.self$path_tmp, basename(tempfile()))
    },
    gc = function(...) dcst_gc(..., .self = .self),
    get = function(key, ...) dcst_get(key = key, ..., .self = .self),
    get_value = function(hash, ...) {
      dcst_get_value(hash = hash, ..., .self = .self)
    },
    safe_get = function(key, ...) {
      out <- just_try(.self$get(key = key, ...))
      if (inherits(out, "try-error")) {
        out <- NA_character_
      }
      out
    },
    safe_get_hash = function(key, ...) {
      out <- just_try(.self$get_hash(key = key, ...))
      if (inherits(out, "try-error")) {
        out <- NA_character_
      }
      out
    },
    set = function(key, value, ...) {
      dcst_set(value = value, key = key, ..., .self = .self)
    },
    memo_hash = function(x, fun, ...) {
      suppressWarnings(ht_memo(ht = .self$ht_hash, x = x, fun = fun, ...))
    },
    reset_memo_hash = function() {
      ht_clear(.self$ht_hash)
    },
    reset_ht_hash = function() {
      # deprecated on 2019-09-13
      ht_clear(.self$ht_hash)
    },
    encode_path = function(x) {
      ht_memo(ht = .self$ht_encode_path, x = x, fun = reencode_path)
    },
    decode_path = function(x) {
      ht_memo(ht = .self$ht_decode_path, x = x, fun = redecode_path)
    },
    encode_namespaced = function(x) {
      ht_memo(ht = .self$ht_encode_namespaced, x = x, fun = reencode_namespaced)
    },
    decode_namespaced = function(x) {
      ht_memo(ht = .self$ht_decode_namespaced, x = x, fun = redecode_namespaced)
    },
    display_keys = function(x) {
      vapply(
        X = x,
        FUN = display_key,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE,
        .self = .self
      )
    },
    set_progress = function(target, value) {
      .self$driver$set_hash(
        key = target,
        namespace = "progress",
        hash = .self$ht_keys[[value]]
      )
    },
    inc_dynamic_progress = function(subtarget, namespace) {
      .self$driver$set_hash(
        key = subtarget,
        namespace = namespace,
        hash = .self$ht_keys[["done"]]
      )
    },
    clear_dynamic_progress = function(target) {
      prefix <- dynamic_progress_ns_pfx(target)
      namespaces <- .self$list_namespaces()
      namespaces <- grep(pattern = prefix, x = namespaces, value = TRUE)
      for (namespace in namespaces) {
        clear_namespace_folder(.self, namespace)
      }
    },
    get_progress = function(targets) {
      retrieve_progress(targets = targets, cache = .self)
    },
    set_history = function(history = NULL) {
      .self$history <- manage_history(history, cache_path = .self$path)
    },
    import = function(
      from,
      ...,
      list = NULL,
      jobs = 1L,
      gc = TRUE
    ) {
      stopifnot(inherits(from, "refclass_decorated_storr"))
      targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
      if (requireNamespace("tidyselect", quietly = TRUE)) {
        targets <- drake_tidyselect_cache(
          ...,
          list = list,
          cache = from,
          namespaces = "meta"
        )
      }
      import_targets(
        targets = targets,
        from = from,
        to = .self,
        jobs = jobs,
        gc = gc
      )
      invisible()
    },
    export = function(
      to,
      ...,
      list = NULL,
      targets = NULL,
      jobs = 1L,
      gc = TRUE
    ) {
      stopifnot(inherits(to, "refclass_decorated_storr"))
      targets <- c(as.character(match.call(expand.dots = FALSE)$...), list)
      if (requireNamespace("tidyselect", quietly = TRUE)) {
        targets <- drake_tidyselect_cache(
          ...,
          list = list,
          cache = .self,
          namespaces = "meta"
        )
      }
      import_targets(
        targets = targets,
        from = .self,
        to = to,
        jobs = jobs,
        gc = gc
      )
      invisible()
    },
    lock = function() {
      .self$assert_unlocked()
      .self$driver$set_hash(
        key = "lock",
        namespace = "session",
        hash = .self$ht_keys[["lock"]]
      )
    },
    unlock = function() {
      .self$del(key = "lock", namespace = "session")
    },
    assert_unlocked = function() {
      if (!.self$exists(key = "lock", namespace = "session")) {
        return()
      }
      stop0(
        "drake's cache is locked.\nRead ",
        "https://docs.ropensci.org/drake/reference/make.html#cache-locking\n",
        "or force unlock the cache with drake::drake_cache(\"",
        normalizePath(.self$path, winslash = "/"),
        "\")$unlock()"
      )
    },
    # Delegate to storr:
    archive_export = function(...) .self$storr$archive_export(...),
    archive_import = function(...) .self$storr$archive_import(...),
    check = function(...) .self$storr$check(...),
    clear = function(...) .self$storr$clear(...),
    clone = function(...) .self$storr$clone(...),
    del = function(...) .self$storr$del(...),
    destroy = function(...) .self$storr$destroy(...),
    duplicate = function(...) .self$storr$duplicate(...),
    exists = function(...) .self$storr$exists(...),
    exists_object = function(...) .self$storr$exists_object(...),
    fill = function(...) .self$storr$fill(...),
    flush_cache = function(...) .self$storr$flush_cache(...),
    get_hash = function(...) .self$storr$get_hash(...),
    hash_object = function(...) .self$storr$hash_object(...),
    hash_raw = function(...) .self$storr$hash_raw(...),
    index_export = function(...) .self$storr$index_export(...),
    index_import = function(...) .self$storr$index_import(...),
    list = function(...) .self$storr$list(...),
    list_hashes = function(...) .self$storr$list_hashes(...),
    list_namespaces = function(...) .self$storr$list_namespaces(...),
    # Does not respect drake's decorated wrapper.
    mget = function(...) .self$storr$mget(...),
    mget_hash = function(...) .self$storr$mget_hash(...),
    # Does not respect drake's decorated wrapper.
    mget_value = function(...) .self$storr$mget_value(...),
    mset = function(...) .self$storr$mset(...),
    mset_by_value = function(...) .self$storr$mset_by_value(...),
    mset_value = function(...) .self$storr$mset_value(...),
    repair = function(...) .self$storr$repair(...),
    serialize_object = function(...) .self$storr$serialize_object(...),
    set_by_value = function(...) .self$storr$set_by_value(...),
    set_value = function(...) .self$storr$set_value(...)
  )
)

dcst_gc <- function(..., .self) {
  before <- .self$storr$list_hashes()
  .self$storr$gc(...)
  after <- .self$storr$list_hashes()
  removed <- setdiff(before, after)
  unlink(.self$file_return_hash(removed))
}

dcst_get <- function(key, ..., .self) {
  value <- .self$storr$get(key = key, ...)
  dcst_get_(value = value, key = key, .self = .self)
}

dcst_get_ <- function(value, key, .self) {
  UseMethod("dcst_get_")
}

#' @export
dcst_get_.default <- function(value, key, .self) {
  value
}

#' @export
dcst_get_.drake_format_fst <- function(value, key, .self) {
  assert_pkg("fst")
  fst::read_fst(.self$file_return_key(key))
}

#' @export
dcst_get_.drake_format_fst_tbl <- function(value, key, .self) {
  assert_pkg("fst")
  assert_pkg("tibble")
  out <- fst::read_fst(.self$file_return_key(key))
  tibble::as_tibble(out)
}

#' @export
dcst_get_.drake_format_fst_dt <- function(value, key, .self) { # nolint
  assert_pkg("data.table")
  assert_pkg("fst")
  fst::read_fst(.self$file_return_key(key), as.data.table = TRUE)
}

#' @export
dcst_get_.drake_format_diskframe <- function(value, key, .self) { # nolint
  assert_pkg("disk.frame")
  assert_pkg("fst")
  disk.frame::disk.frame(.self$file_return_key(key), backend = "fst")
}

#' @export
dcst_get_.drake_format_qs <- function(value, key, .self) { # nolint
  assert_pkg("qs")
  qs::qread(
    file = .self$file_return_key(key),
    use_alt_rep = FALSE,
    strict = FALSE,
    nthreads = 1L
  )
}

# Requires Python Keras and TensorFlow to test. Tested in test-keras.R.
# nocov start
#' @export
dcst_get_.drake_format_keras <- function(value, key, .self) {
  assert_pkg("keras")
  keras::load_model_hdf5(.self$file_return_key(key))
}
# nocov end

#' @export
dcst_get_.drake_format_rds <- function(value, key, .self) {
  readRDS(.self$file_return_key(key))
}

#' @export
dcst_get_.drake_format_file <- function(value, key, .self) {
  value$value
}

dcst_get_value <- function(hash, ..., .self) {
  value <- .self$storr$get_value(hash = hash, ...)
  dcst_get_value_(value = value, hash = hash, .self = .self)
}

dcst_get_value_ <- function(value, hash, .self) {
  UseMethod("dcst_get_value_")
}

#' @export
dcst_get_value_.default <- function(value, hash, .self) {
  value
}

#' @export
dcst_get_value_.drake_format_fst <- function(value, hash, .self) { # nolint
  assert_pkg("fst")
  fst::read_fst(.self$file_return_hash(hash))
}

#' @export
dcst_get_value_.drake_format_fst_tbl <- function(value, hash, .self) { # nolint
  assert_pkg("fst")
  assert_pkg("tibble")
  out <- fst::read_fst(.self$file_return_hash(hash))
  tibble::as_tibble(out)
}

#' @export
dcst_get_value_.drake_format_fst_dt <- function(value, hash, .self) { # nolint
  assert_pkg("data.table")
  assert_pkg("fst")
  fst::read_fst(.self$file_return_hash(hash), as.data.table = TRUE)
}

#' @export
dcst_get_value_.drake_format_diskframe <- function(value, hash, .self) { # nolint
  assert_pkg("disk.frame")
  assert_pkg("fst")
  disk.frame::disk.frame(.self$file_return_hash(hash), backend = "fst")
}

#' @export
dcst_get_value_.drake_format_qs <- function(value, hash, .self) { # nolint
  assert_pkg("qs")
  qs::qread(
    file = .self$file_return_hash(hash),
    use_alt_rep = FALSE,
    strict = FALSE,
    nthreads = 1L
  )
}

# Requires Python Keras and TensorFlow to test. Tested in test-keras.R.
# nocov start
#' @export
dcst_get_value_.drake_format_keras <- function(value, hash, .self) { # nolint
  assert_pkg("keras")
  keras::load_model_hdf5(.self$file_return_hash(hash))
}
# nocov end

#' @export
dcst_get_value_.drake_format_rds <- function(value, hash, .self) { # nolint
  readRDS(.self$file_return_hash(hash))
}

#' @export
dcst_get_value_.drake_format_file <- function(value, hash, .self) { # nolint
  value$value
}

dcst_set <- function(value, key, ..., .self) {
  UseMethod("dcst_set")
}

#' @export
dcst_set.default <- function(value, key, ..., .self) {
  suppressWarnings(.self$storr$set(key = key, value = value, ...))
}

#' @export
dcst_set.drake_format_fst <- function(value, key, ..., .self) {
  assert_pkg("fst")
  .self$assert_dirs()
  tmp <- .self$file_tmp()
  on.exit(file_remove(tmp), add = TRUE)
  fst::write_fst(x = value$value, path = tmp)
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}

#' @export
dcst_set.drake_format_fst_tbl <- dcst_set.drake_format_fst

#' @export
dcst_set.drake_format_fst_dt <- function(value, key, ..., .self) {
  assert_pkg("data.table")
  assert_pkg("fst")
  .self$assert_dirs()
  tmp <- .self$file_tmp()
  on.exit(file_remove(tmp), add = TRUE)
  fst::write_fst(x = value$value, path = tmp)
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}

#' @export
dcst_set.drake_format_diskframe <- function(value, key, ..., .self) { # nolint
  assert_pkg("disk.frame")
  assert_pkg("fst")
  .self$assert_dirs()
  tmp <- attr(value$value, "path")
  on.exit(file_remove(tmp), add = TRUE)
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}

#' @export
dcst_set.drake_format_qs <- function(value, key, ..., .self) { # nolint
  assert_pkg("qs")
  .self$assert_dirs()
  tmp <- .self$file_tmp()
  on.exit(file_remove(tmp), add = TRUE)
  qs::qsave(
    x = value$value,
    file = tmp,
    preset = "high",
    algorithm = "zstd",
    compress_level = 4L,
    shuffle_control = 15L,
    check_hash = TRUE
  )
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}

# Requires Python Keras and TensorFlow to test. Tested in test-test-keras.R
# nocov start
#' @export
dcst_set.drake_format_keras <- function(value, key, ..., .self) {
  assert_pkg("keras")
  .self$assert_dirs()
  tmp <- .self$file_tmp()
  on.exit(file_remove(tmp), add = TRUE)
  keras::save_model_hdf5(object = value$value, filepath = tmp)
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}
# nocov end

#' @export
dcst_set.drake_format_rds <- function(value, key, ..., .self) {
  .self$assert_dirs()
  stopifnot(getRversion() >= "3.5.0") # for ALTREP
  tmp <- .self$file_tmp()
  saveRDS(
    object = value$value,
    file = tmp,
    ascii = FALSE,
    version = 3L,
    compress = TRUE,
    refhook = NULL
  )
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}

dcst_set_move_tmp <- function(key, value, tmp, .self) {
  hash_tmp <- rehash_local(tmp, config = list(cache = .self))
  class(hash_tmp) <- class(value)
  hash <- .self$storr$set(key = key, value = hash_tmp)
  file <- .self$file_return_hash(hash)
  storage_move(
    tmp,
    file,
    overwrite = FALSE,
    merge = FALSE,
    warn = FALSE
  )
  invisible(hash)
}

#' @title drake tempfile
#' `r lifecycle::badge("stable")`
#' @description Create the path to a temporary file inside drake's cache.
#' @details This function is just like the `tempfile()` function in base R
#'   except that the path points to a special location inside `drake`'s cache.
#'   This ensures that if the file needs to be copied to
#'   persistent storage in the cache, `drake` does not need to copy across
#'   physical storage media. Example: the `"diskframe"` format. See the
#'   "Formats" and "Columns" sections of the [drake_plan()] help file.
#'   Unless you supply the cache or the path to the cache
#'   (see [drake_cache()]) `drake` will assume the cache folder is named
#'   `.drake/` and it is located either in your working directory or an
#'   ancestor of your working directory.
#' @export
#' @seealso [drake_cache()], [new_cache()]
#' @inheritParams cached
#' @examples
#' cache <- new_cache(tempfile())
#' # No need to supply a cache if a .drake/ folder exists.
#' drake_tempfile(cache = cache)
#' drake_plan(
#'   x = target(
#'     as.disk.frame(large_data, outdir = drake_tempfile()),
#'     format = "diskframe"
#'   )
#' )
drake_tempfile <- function(
  path = NULL,
  cache = drake::drake_cache(path = path)
) {
  if (is.null(cache)) {
    stop0("drake cache not found")
  }
  cache <- decorate_storr(cache)
  cache$file_tmp()
}

#' @title Show a file's encoded representation in the cache
#' `r lifecycle::badge("stable")`
#' @description This function simply wraps literal double quotes around
#' the argument `x` so `drake` knows it is the name of a file.
#' Use when you are calling functions like `deps_code()`: for example,
#' `deps_code(file_store("report.md"))`. See the examples for details.
#' Internally, `drake` wraps the names of file targets/imports
#' inside literal double quotes to avoid confusion between
#' files and generic R objects.
#' @export
#' @return A single-quoted character string: i.e., a filename
#' understandable by drake.
#' @param x Character string to be turned into a filename
#' understandable by drake (i.e., a string with literal
#' single quotes on both ends).
#' @examples
#' # Wraps the string in single quotes.
#' file_store("my_file.rds") # "'my_file.rds'"
#' \dontrun{
#' isolate_example("contain side effects", {
#' if (suppressWarnings(require("knitr"))) {
#' load_mtcars_example() # Get the code with drake_example("mtcars").
#' make(my_plan) # Run the workflow to build the targets
#' list.files() # Should include input "report.Rmd" and output "report.md".
#' head(readd(small)) # You can use symbols for ordinary objects.
#' # But if you want to read cached info on files, use `file_store()`.
#' readd(file_store("report.md"), character_only = TRUE) # File fingerprint.
#' deps_code(file_store("report.Rmd"))
#' config <- drake_config(my_plan)
#' deps_profile(
#'   file_store("report.Rmd"),
#'   plan = my_plan,
#'   character_only = TRUE
#' )
#' }
#' })
#' }
file_store <- function(x) {
  reencode_path(x)
}

display_key <- function(x, .self) {
  if (is_encoded_path(x)) {
    display_path(x = x, .self = .self)
  } else if (is_encoded_namespaced(x)) {
    sprintf("%s", .self$decode_namespaced(x = x))
  } else {
    x
  }
}

display_path <- function(x, .self) {
  path_ <- .self$decode_path(x = x)
  if (is_url(path_)) {
    sprintf("url %s", path_)
  } else {
    sprintf("file %s", path_)
  }
}

redisplay_keys <- function(x) {
  vapply(
    X = x,
    FUN = redisplay_key,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE
  )
}

redisplay_key <- function(x) {
  if (is_encoded_path(x)) {
    redisplay_path(x = x)
  } else if (is_encoded_namespaced(x)) {
    sprintf("%s", redecode_namespaced(x = x))
  } else {
    x
  }
}

redisplay_path <- function(x) {
  path <- redecode_path(x = x)
  if (is_url(path)) {
    sprintf("url %s", path)
  } else {
    sprintf("file %s", path)
  }
}

is_encoded_path <- function(x) {
  substr(x = x, start = 1, stop = 2) == "p-"
}

is_encoded_namespaced <- function(x) {
  substr(x = x, start = 1, stop = 2) == "n-"
}

reencode_path <- function(x) {
  y <- base64url::base32_encode(x = x, use.padding = FALSE)
  sprintf("p-%s", y)
}

redecode_path <- function(x) {
  y <- substr(x = x, start = 3, stop = 1e6)
  base64url::base32_decode(x = y, use.padding = FALSE)
}

reencode_namespaced <- function(x) {
  y <- base64url::base32_encode(x, use.padding = FALSE)
  sprintf("n-%s", y)
}

redecode_namespaced <- redecode_path

standardize_key <- function(text) {
  if (any(grepl("::", text))) {
    text <- reencode_namespaced(text)
  }
  text
}

ht_keys <- function(digest_fn) {
  keys <- c("running", "done", "cancelled", "failed", "lock")
  out <- lapply(keys, precomputed_key_hash, digest_fn = digest_fn)
  names(out) <- keys
  out
}

precomputed_key_hash <- function(key, digest_fn) {
  out <- digest_fn(key, serialize = FALSE)
  gsub("^.", substr(key, 1L, 1L), out)
}

retrieve_progress <- function(targets, cache) {
  hash <- cache$mget_hash(key = targets, namespace = "progress")
  substr <- substr(hash, 1, 1)
  deduce_progress(substr)
}

deduce_progress <- Vectorize(function(substr) {
  switch(
    substr,
    r = "running",
    d = "done",
    c = "cancelled",
    f = "failed",
    "none"
  )
}, vectorize.args = "substr", USE.NAMES = FALSE)

manage_history <- function(history, cache_path) {
  if (!is_history(history)) {
    migrate_history(history, cache_path)
  }
  if (is.null(history)) {
    history <- recover_default_history(cache_path)
  } else if (identical(history, TRUE)) {
    history <- initialize_history(cache_path)
  } else if (identical(history, FALSE)) {
    history <- NULL
  }
  stopifnot(is.null(history) || is_history(history))
  history
}

migrate_history <- function(history, cache_path) {
  old_path <- file.path(dirname(cache_path), ".drake_history")
  if (file.exists(old_path)) {
    dir_move(old_path, default_history_path(cache_path), merge = FALSE)
  }
}

recover_default_history <- function(cache_path) {
  history_path <- default_history_path(cache_path)
  if (file.exists(history_path)) {
    history_queue(history_path)
  }
}

initialize_history <- function(cache_path) {
  history_queue(default_history_path(cache_path))
}

default_history_path <- function(cache_path) {
  file.path(cache_path, "drake", "history")
}

history_queue <- function(history_path) {
  dir_create(history_path)
  txtq::txtq(history_path, use_lock_file = FALSE)
}

is_history <- function(history) {
  inherits(history, "R6_txtq")
}

import_targets <- function(targets, from, to, jobs, gc) {
  if (!length(targets)) {
    targets <- from$list()
  }
  lightly_parallelize(
    X = targets %||% from$list(),
    FUN = import_target,
    jobs = jobs,
    from = from,
    to = to,
    gc = gc
  )
}

import_target <- function(target, from, to, gc) {
  import_target_storr(target = target, from = from, to = to, gc = gc)
  import_target_formatted(target = target, from = from, to = to)
}

import_target_storr <- function(target, from, to, gc) {
  for (ns in setdiff(from$storr$list_namespaces(), "progress")) {
    if (from$storr$exists(target, namespace = ns)) {
      value <- from$get(key = target, namespace = ns)
      to$set(key = target, value = value, namespace = ns)
    }
    ifelse(gc, gc(), TRUE)
  }
  invisible()
}

import_target_formatted <- function(target, from, to) {
  if (from$exists(target) && file.exists(from$file_return_key(target))) {
    storage_copy(
      from = from$file_return_key(target),
      to = to$file_return_key(target),
      warn = FALSE
    )
  }
}

# TODO: simplify to just clear when
# https://github.com/richfitz/storr/pull/122 is merged.
clear_namespace_folder <- function(cache, namespace) {
  if (inherits(cache$driver, "driver_rds")) {
    path <- file.path(cache$path, "keys", namespace)
    if (file.exists(path)) {
      unlink(path, recursive = TRUE)
    }
  }
  cache$clear(namespace = namespace)
}

# Should be used as sparingly as possible.
just_try <- function(code) {
  try(suppressWarnings(code), silent = TRUE)
}
