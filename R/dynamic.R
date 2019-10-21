as_dynamic <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  class(x) <- c(x[[1]], "dynamic", class(x))
  x
}

get_dynamic_size <- function(target, config) {
  if (ht_exists(config$ht_dynamic_size, target)) {
    return(ht_get(config$ht_dynamic_size, target))
  }
  size <- config$cache$get(target, namespace = "meta")$size
  stopifnot(size > 0L)
  ht_set(config$ht_dynamic_size, x = target, value = size)
  size
}

get_dynamic_nby <- function(target, config) {
  if (ht_exists(config$ht_dynamic_nby, target)) {
    return(ht_get(config$ht_dynamic_nby, target))
  }
  nby <- length(unique(config$cache$get(target, use_cache = FALSE)))
  stopifnot(nby > 0L)
  ht_set(config$ht_dynamic_nby, x = target, value = nby)
  nby
}

subtarget_names <- function(dynamic, target, config) {
  UseMethod("subtarget_names")
}

subtarget_names.map <- function(dynamic, target, config) {
  vars <- all.vars(dynamic)
  sizes <- lapply(vars, get_dynamic_size, config = config)
  stopifnot(length(unique(sizes)) == 1L)
  index <- seq_len(sizes[[1]])
  paste(target, index, sep = "_")
}

subtarget_names.cross <- function(dynamic, target, config) {
  vars <- all.vars(dynamic)
  sizes <- unlist(lapply(vars, get_dynamic_size, config = config))
  index <- seq_len(prod(sizes))
  paste(target, index, sep = "_")
}

subtarget_names.split <- subtarget_names.combine <- function(
  dynamic,
  target,
  config
) {
  call <- match.call(definition = def_split, call = dynamic)
  .by <- deparse(call$.by)
  nby <- get_dynamic_nby(.by, config)
  index <- seq_len(nby)
  paste(target, index, sep = "_")
}

# nocov start
def_split <- function(target, .by) {
  NULL
}
# nocov end
