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
    reset_ht_hash = function() {
      ht_clear(.self$ht_hash)
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

# Requires Python Keras and TensorFlow to test. Tested in test-interactive.R.
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

# Requires Python Keras and TensorFlow to test. Tested in test-interactive.R.
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

# Requires Python Keras and TensorFlow to test. Tested in test-interactive.R.
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
  hash_tmp <- digest::digest(
    object = tmp,
    algo = .self$hash_algorithm,
    serialize = FALSE,
    file = TRUE
  )
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
