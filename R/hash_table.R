# Wanted to use reference classes here,
# but they add computational overhead.
# This is a part of the code that really needs to be fast.
ht_new <- function(x = NULL) {
  out <- new.env(hash = TRUE, parent = emptyenv())
  if (!is.null(x)) {
    ht_add(out, x)
  }
  out
}

ht_add <- function(ht, x) {
  lapply(
    X = x[nzchar(x)],
    FUN = assign,
    value = TRUE,
    envir = ht,
    inherits = FALSE
  )
  invisible()
}

ht_del <- function(ht, x) {
  if (!nzchar(x)) {
    return()
  }
  remove(list = x, envir = ht, inherits = FALSE)
}

ht_exists <- function(ht, x) {
  if (!nzchar(x)) {
    return(FALSE)
  }
  exists(x, envir = ht, inherits = FALSE)
}

ht_list <- function(ht) {
  ls(envir = ht, all.names = TRUE, sorted = FALSE)
}

ht_clone <- function(ht) {
  list2env(as.list(ht), hash = TRUE, parent = emptyenv())
}
