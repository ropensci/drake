decorate_storr <- function(storr) {
  if (inherits(storr, "refclass_decorated_storr")) {
    return(storr)
  }
  if (!inherits(storr, "storr")) {
    stop("not a storr", call. = FALSE)
  }
  hash_algorithm <- storr$driver$hash_algorithm %||% "xxhash64"
  path <- storr$driver$path %||% default_cache_path()
  refclass_decorated_storr$new(
    storr = storr,
    driver = storr$driver,
    default_namespace = storr$default_namespace,
    envir = storr$envir,
    hash_algorithm = hash_algorithm,
    ht_encode_path = ht_new(),
    ht_decode_path = ht_new(),
    ht_encode_namespaced = ht_new(),
    ht_decode_namespaced = ht_new(),
    ht_hash = ht_new(),
    ht_progress = ht_progress(hash_algorithm),
    path = path,
    path_return = file.path(path, "drake", "return"),
    path_tmp = file.path(path, "drake", "tmp")
  )
}

refclass_decorated_storr <- methods::setRefClass(
  Class = "refclass_decorated_storr",
  fields = c(
    "storr",
    "driver",
    "default_namespace",
    "envir",
    "hash_algorithm",
    "ht_encode_path",
    "ht_decode_path",
    "ht_encode_namespaced",
    "ht_decode_namespaced",
    "ht_hash",
    "ht_progress",
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
    set = function(key, value, ...) {
      dcst_set(value = value, key = key, ..., .self = .self)
    },
    memo_hash = function(x, fun, ...) {
      ht_memo(ht = .self$ht_hash, x = x, fun = fun, ...)
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
        hash = .self$ht_progress[[value]]
      )
    },
    get_progress = function(target) {
      retrieve_progress(target = target, cache = .self)
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
    export = function(...) .self$storr$export(...),
    fill = function(...) .self$storr$fill(...),
    flush_cache = function(...) .self$storr$flush_cache(...),
    get_hash = function(...) .self$storr$get_hash(...),
    hash_object = function(...) .self$storr$hash_object(...),
    hash_raw = function(...) .self$storr$hash_raw(...),
    import = function(...) .self$storr$import(...),
    index_export = function(...) .self$storr$index_export(...),
    index_import = function(...) .self$storr$index_import(...),
    list = function(...) .self$storr$list(...),
    list_hashes = function(...) .self$storr$list_hashes(...),
    list_namespaces = function(...) .self$storr$list_namespaces(...),
    mget = function(...) .self$storr$mget(...),
    mget_hash = function(...) .self$storr$mget_hash(...),
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

dcst_get_.default <- function(value, key, .self) {
  value
}

dcst_get_.drake_format_fst <- function(value, key, .self) {
  assert_pkg("fst")
  fst::read_fst(.self$file_return_key(key))
}

dcst_get_.drake_format_fst_dt <- function(value, key, .self) { # nolint
  assert_pkg("data.table")
  assert_pkg("fst")
  fst::read_fst(.self$file_return_key(key), as.data.table = TRUE)
}

# Requires Python Keras and TensorFlow to test. Tested in test-keras.R.
# nocov start
dcst_get_.drake_format_keras <- function(value, key, .self) {
  assert_pkg("keras")
  keras::load_model_hdf5(.self$file_return_key(key))
}
# nocov end

dcst_get_.drake_format_rds <- function(value, key, .self) {
  readRDS(.self$file_return_key(key))
}

dcst_get_value <- function(hash, ..., .self) {
  value <- .self$storr$get_value(hash = hash, ...)
  dcst_get_value_(value = value, hash = hash, .self = .self)
}

dcst_get_value_ <- function(value, hash, .self) {
  UseMethod("dcst_get_value_")
}

dcst_get_value_.default <- function(value, hash, .self) {
  value
}

dcst_get_value_.drake_format_fst <- function(value, hash, .self) { # nolint
  assert_pkg("fst")
  fst::read_fst(.self$file_return_hash(hash))
}

dcst_get_value_.drake_format_fst_dt <- function(value, hash, .self) { # nolint
  assert_pkg("data.table")
  assert_pkg("fst")
  fst::read_fst(.self$file_return_hash(hash), as.data.table = TRUE)
}

# Requires Python Keras and TensorFlow to test. Tested in test-keras.R.
# nocov start
dcst_get_value_.drake_format_keras <- function(value, hash, .self) { # nolint
  assert_pkg("keras")
  keras::load_model_hdf5(.self$file_return_hash(hash))
}
# nocov end

dcst_get_value_.drake_format_rds <- function(value, hash, .self) { # nolint
  readRDS(.self$file_return_hash(hash))
}

dcst_set <- function(value, key, ..., .self) {
  UseMethod("dcst_set")
}

dcst_set.default <- function(value, key, ..., .self) {
  .self$storr$set(key = key, value = value, ...)
}

dcst_set.drake_format_fst <- function(value, key, ..., .self) {
  assert_pkg("fst")
  .self$assert_dirs()
  tmp <- .self$file_tmp()
  fst::write_fst(x = value$value, path = tmp)
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}

dcst_set.drake_format_fst_dt <- function(value, key, ..., .self) {
  assert_pkg("data.table")
  assert_pkg("fst")
  .self$assert_dirs()
  tmp <- .self$file_tmp()
  fst::write_fst(x = value$value, path = tmp)
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}

# Requires Python Keras and TensorFlow to test. Tested in test-test-keras.R
# nocov start
dcst_set.drake_format_keras <- function(value, key, ..., .self) {
  assert_pkg("keras")
  .self$assert_dirs()
  tmp <- .self$file_tmp()
  keras::save_model_hdf5(object = value$value, filepath = tmp)
  dcst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}
# nocov end

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
  hash_tmp <- rehash_local(tmp, .self$hash_algorithm)
  class(hash_tmp) <- class(value)
  hash <- .self$storr$set(key = key, value = hash_tmp)
  file <- .self$file_return_hash(hash)
  file.copy(tmp, file)
  invisible(hash)
}

dir_create <- function(x) {
  if (!file.exists(x)) {
    dir.create(x, showWarnings = FALSE, recursive = TRUE)
  }
  if (!dir.exists(x)) {
    stop("cannot create directory at ", shQuote(x), call. = FALSE)
  }
  invisible()
}

#' @title Show a file's encoded representation in the cache
#' \lifecycle{stable}
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
#'   config = config,
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

ht_progress <- function(hash_algorithm) {
  keys <- c("running", "done", "failed")
  out <- lapply(keys, progress_hash, hash_algorithm = hash_algorithm)
  names(out) <- keys
  out
}

progress_hash <- function(key, hash_algorithm) {
  out <- digest::digest(
    key,
    algo = hash_algorithm,
    serialize = FALSE
  )
  gsub("^.", substr(key, 1, 1), out)
}

retrieve_progress <- function(target, cache) {
  if (cache$exists(key = target, namespace = "progress")) {
    hash <- cache$get_hash(key = target, namespace = "progress")
    switch(
      substr(hash, 1, 1),
      r = "running",
      d = "done",
      f = "failed",
      NA_character_
    )
  } else{
    "none"
  }
}
