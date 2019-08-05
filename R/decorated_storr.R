decorate_storr <- function(storr) {
  if (inherits(storr, "refclass_decorated_storr")) {
    return(storr)
  }
  if (!inherits(storr, "storr")) {
    stop("not a storr", call. = FALSE)
  }
  refclass_decorated_storr$new(
    storr = storr,
    driver = storr$driver,
    default_namespace = storr$default_namespace,
    envir = storr$envir
  )
}

delegate_storr <- function(storr, name, ...) {
  function(storr, name, ...) {
    storr[[name]](...)
  }
}

refclass_decorated_storr <- methods::setRefClass(
  Class = "refclass_decorated_storr",
  fields = c("storr", "driver", "default_namespace", "envir"),
  # Tedious, but better than inheritance, which would
  # prevent users from supplying their own true `storr`s.
  methods = list(
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
    gc = function(...) decorated_storr_gc(.self$storr, ...),
    get = function(...) decorated_storr_get(.self$storr, ...),
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
    set = function(key, value, ...) {
      decorated_storr_set(.self$storr, key = key, value = value, ...)
    },
    set_by_value = function(...) .self$storr$set_by_value(...),
    set_value = function(...) .self$storr$set_value(...)
  )
)

decorated_storr_gc <- function(storr, ...) {
  storr$gc(...)
}

decorated_storr_get <- function(storr, ...) {
  UseMethod("decorated_storr_get")
}

decorated_storr_get.default <- function(storr, ...) {
  storr$get(...)
}

decorated_storr_get.return_fst <- function(storr, ...) {
  storr$get(...)
}

decorated_storr_get.return_keras <- function(storr, ...) {
  storr$get(...)
}

decorated_storr_get.return_rds <- function(storr, ...) {
  storr$get(...)
}

decorated_storr_set <- function(storr, key, value, ...) {
  UseMethod("decorated_storr_set")
}

decorated_storr_set.default <- function(storr, key, value, ...) {
  storr$set(key = key, value = value, ...)
}

decorated_storr_set.return_fst <- function(storr, key, value, ...) {
  storr$set(key = key, value = value, ...)
}

decorated_storr_set.return_keras <- function(storr, key, value, ...) {
  storr$set(key = key, value = value, ...)
}

decorated_storr_set.return_rds <- function(storr, key, value, ...) {
  storr$set(key = key, value = value, ...)
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
