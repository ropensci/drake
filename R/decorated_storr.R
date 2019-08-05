decorate_storr <- function(storr) {
  if (inherits(storr, "refclass_decorated_storr")) {
    return(storr)
  }
  if (!inherits(storr, "storr")) {
    stop("not a storr", call. = FALSE)
  }
  hash_algorithm <- cache_hash_algorithm(storr)
  path <- force_cache_path(storr)
  path_return <- file.path(path, "drake", "return")
  path_tmp <- file.path(path, "drake", "tmp")
  refclass_decorated_storr$new(
    storr = storr,
    driver = storr$driver,
    default_namespace = storr$default_namespace,
    envir = storr$envir,
    hash_algorithm = hash_algorithm,
    path = path,
    path_return = path_return,
    path_tmp = path_tmp
  )
}

refclass_decorated_storr <- methods::setRefClass(
  Class = "refclass_decorated_storr",
  fields = c(
    "storr",
    "driver",
    "default_namespace",
    "envir",
    "path",
    "path_return",
    "path_tmp",
    "hash_algorithm"
  ),
  # Tedious, but better than inheritance, which would
  # prevent users from supplying their own true `storr`s.
  methods = list(
    # Custom:
    file_return_hash = function(hash) {
      dir_create(.self$path_return)
      file.path(.self$path_return, hash)
    },
    file_return_key = function(key) {
      hash <- .self$get_hash(key)
      .self$file_return_hash(hash)
    },
    file_tmp = function() {
      dir_create(.self$path_tmp)
      file.path(.self$path_tmp, basename(tempfile()))
    },
    gc = function(...) decst_gc(..., .self = .self),
    get = function(key, ...) decst_get(key = key, ..., .self = .self),
    set = function(key, value, ...) {
      decst_set(value = value, key = key, ..., .self = .self)
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
    get_value = function(...) .self$storr$get_value(...),
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

decst_gc <- function(..., .self) {
  .self$storr$gc(...)
}

decst_get <- function(key, ..., .self) {
  value <- .self$storr$get(key = key, ...)
  decst_inner_get(value = value, key = key, .self = .self)
}

decst_inner_get <- function(value, key, .self) {
  UseMethod("decst_inner_get")
}

decst_inner_get.default <- function(value, key, .self) {
  value
}

decst_inner_get.return_fst <- function(value, key, .self) {
  value
}

decst_inner_get.return_keras <- function(value, key, .self) {
  value
}

decst_inner_get.return_rds <- function(value, key, .self) {
  readRDS(.self$file_return_key(key))
}

decst_set <- function(value, key, ..., .self) {
  UseMethod("decst_set")
}

decst_set.default <- function(value, key, ..., .self) {
  .self$storr$set(key = key, value = value, ...)
}

decst_set.return_fst <- function(value, key, ..., .self) {
  assert_pkg("fst")
  .self$storr$set(key = key, value = value, ...)
}

decst_set.return_keras <- function(value, key, ..., .self) {
  assert_pkg("keras")
  .self$storr$set(key = key, value = value, ...)
}

decst_set.return_rds <- function(value, key, ..., .self) {
  r_version <- paste0(R.version$major, ".", R.version$minor)
  sufficient_r_version <- utils::compareVersion(r_version, "3.5.0") >= 0L
  stopifnot(sufficient_r_version)
  tmp <- .self$file_tmp()
  saveRDS(
    object = value$value,
    file = tmp,
    ascii = FALSE,
    version = 3L,
    compress = TRUE,
    refhook = NULL
  )
  decst_set_move_tmp(key = key, value = value, tmp = tmp, .self = .self)
}

decst_set_move_tmp <- function(key, value, tmp, .self) {
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
