# Wanted to use reference classes here,
# but they add computational overhead.
# This is a part of the code that really needs to be fast.
ht_new <- function() {
  new.env(hash = TRUE, parent = emptyenv())
}

ht_add <- function(ht, x) {
  lapply(
    X = x,
    FUN = assign,
    value = TRUE,
    envir = ht,
    inherits = FALSE
  )
  invisible()
}

hd_del <- function(ht, x) {
  remove(list = x, envir = ht, inherits = FALSE)
}

ht_exists <- function(ht, x) {
  exists(x, envir = ht, inherits = FALSE)
}

ht_list <- function(ht) {
  ls(envir = ht, all.names = TRUE, sorted = FALSE)
}
