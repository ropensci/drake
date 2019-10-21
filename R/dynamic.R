as_dynamic <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  class(x) <- c(x[[1]], "dynamic", class(x))
  match_call(x)
}

match_call <- function(dynamic) {
  UseMethod("match_call")
}

match_call.map <- match_call.cross <- function(dynamic) {
  unname(match.call(definition = def_map, call = dynamic))
}

match_call.split <- match_call.combine <- function(dynamic) {
  match.call(definition = def_split, call = dynamic)
}

# nocov start
def_map <- function(...) {
  NULL
}

def_split <- function(target, .by = NULL) {
  NULL
}
# nocov end

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
  if (!length(dynamic$.by)) {
    return(paste0(target, "_1"))
  }
  .by <- deparse(call$.by)
  nby <- get_dynamic_nby(.by, config)
  index <- seq_len(nby)
  paste(target, index, sep = "_")
}

subtarget_index <- function(dynamic, target, config, index) {
  UseMethod("subtarget_index")
}

subtarget_index.map <- function(dynamic, target, config, index) {
  vars <- all.vars(dynamic)
  out <- as.list(rep(as.integer(index), length(vars)))
  names(out) <- vars
  out
}

subtarget_index.cross <- function(dynamic, target, config, index) {
  vars <- all.vars(dynamic)
  size <- unlist(lapply(vars, get_dynamic_size, config = config))
  out <- grid_index(index, size)
  out <- as.list(out)
  names(out) <- vars
  out
}

subtarget_index.split <- function(dynamic, target, config, index) {
  if (!length(dynamic$.by)) {
    return(1L)
  }
  browser()

}

subtarget_index.combine <- function(dynamic, target, config, index) {
  browser()

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
