# Wanted to use reference classes here,
# but they add computational overhead.
# This is a part of the code that really needs to be fast.
ht_new <- function(x = NULL, hash = TRUE) {
  out <- new.env(hash = hash, parent = emptyenv())
  if (!is.null(x)) {
    ht_add(out, x)
  }
  out
}

ht_add <- function(ht, x) {
  lapply(
    X = x,
    FUN = assign,
    value = TRUE,
    envir = ht,
    inherits = FALSE
  )
}

ht_del <- function(ht, x) {
  remove(list = x, envir = ht, inherits = FALSE)
}

ht_exists <- function(ht, x) {
  exists(x, envir = ht, inherits = FALSE)
}

ht_list <- function(ht) {
  ls(envir = ht, all.names = TRUE, sorted = FALSE)
}

ht_clone <- function(ht) {
  list2env(as.list(ht), hash = TRUE, parent = emptyenv())
}

# Merge y into x
ht_merge <- function(x, y) {
  ht_add(x, ht_list(y))
}
