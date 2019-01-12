ht_new <- function(x = NULL, hash = TRUE) {
  out <- new.env(hash = hash, parent = emptyenv())
  if (!is.null(x)) {
    ht_set(out, x)
  }
  out
}

ht_new_from_list <- function(x, hash = (length(x) > 100)) {
  list2env(x, hash = hash, parent = emptyenv())
}

ht_set <- function(ht, x) {
  lapply(
    X = x,
    FUN = assign,
    value = TRUE,
    envir = ht,
    inherits = FALSE
  )
}

ht_get <- function(ht, x) {
  get(x = x, envir = ht, inherits = FALSE)
}

ht_del <- function(ht, x) {
  remove(list = x, envir = ht, inherits = FALSE)
}

ht_exists <- function(ht, x) {
  exists(x, envir = ht, inherits = FALSE)
}

ht_list <- function(ht) {
  names(ht)
}

ht_clear <- function(ht) {
  rm(list = names(ht), envir = ht)
}

ht_clone <- function(ht) {
  list2env(as.list(ht), hash = TRUE, parent = emptyenv())
}

ht_filter <- function(ht, x) {
  index <- vapply(
    X = x,
    FUN = ht_exists,
    FUN.VALUE = logical(1),
    ht = ht
  )
  x[index]
}

# Merge y into x
ht_merge <- function(x, y) {
  ht_set(x, ht_list(y))
}

# hash-table-based memoization for characters
ht_memo <- function(ht, x, fun, ...) {
  if (is.null(ht)) {
    return(lapply(X = x, FUN = fun, ...))
  }
  vapply(
    X = x,
    FUN = ht_memo_single,
    FUN.VALUE = character(1),
    USE.NAMES = FALSE,
    ht = ht,
    fun = fun,
    ...
  )
}

# x must be a character scalar.
ht_memo_single <- function(ht, x, fun, ...) {
  if (ht_exists(ht = ht, x = x)) {
    ht_get(ht = ht, x = x)
  } else {
    value <- fun(x, ...)
    assign(x = x, value = value, envir = ht, inherits = FALSE)
    value
  }
}
