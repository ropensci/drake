is_dynamic <- function(target, config) {
  inherits(config$layout[[target]]$dynamic, "dynamic")
}

as_dynamic <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  class(x) <- c(x[[1]], "dynamic", class(x))
  match_call(x)
}

match_call <- function(dynamic) {
  class <- class(dynamic)
  out <- match_call_impl(dynamic)
  class(out) <- class
  out
}

match_call_impl <- function(dynamic) {
  UseMethod("match_call_impl")
}

match_call_impl.map <- match_call_impl.cross <- function(dynamic) {
  unname(match.call(definition = def_map, call = dynamic))
}

match_call_impl.split <- match_call_impl.combine <- function(dynamic) {
  match.call(definition = def_split, call = dynamic)
}

# nocov start
def_map <- function(...) {
  NULL
}

def_split <- function(.x, .by = NULL) {
  NULL
}
# nocov end

subtarget_name <- function(target, index) {
  paste(target, index, sep = "_")
}

number_subtargets <- function(target, config) {
  if (!is_dynamic(target, config)) {
    return(0)
  }
  dynamic <- config$layout[[target]]$dynamic
  number_subtargets_impl(dynamic, target, config)
}

number_subtargets_impl <- function(dynamic, target, config) {
  UseMethod("number_subtargets_impl")
}

number_subtargets_impl.map <- function(dynamic, target, config) {
  vars <- all.vars(dynamic)
  sizes <- lapply(vars, get_dynamic_size, config = config)
  stopifnot(length(unique(sizes)) == 1L)
  sizes[[1]]
}

number_subtargets_impl.cross <- function(dynamic, target, config) {
  vars <- all.vars(dynamic)
  sizes <- unlist(lapply(vars, get_dynamic_size, config = config))
  prod(sizes)
}

number_subtargets_impl.split <- number_subtargets_impl.combine <- function(
  dynamic,
  target,
  config
) {
  if (no_by(dynamic)) {
    return(1L)
  }
  by <- which_by(dynamic)
  get_dynamic_nby(by, config)
}

subtarget_deps <- function(target, index, config) {
  dynamic <- config$layout[[target]]$dynamic
  subtarget_deps_impl(dynamic, target, index, config)
}

subtarget_deps_impl <- function(dynamic, target, index, config) {
  UseMethod("subtarget_deps_impl")
}

subtarget_deps_impl.map <- function(dynamic, target, index, config) {
  vars <- all.vars(dynamic)
  out <- as.list(rep(as.integer(index), length(vars)))
  names(out) <- vars
  out
}

subtarget_deps_impl.cross <- function(dynamic, target, index, config) {
  vars <- all.vars(dynamic)
  size <- unlist(lapply(vars, get_dynamic_size, config = config))
  out <- grid_index(index, size)
  out <- as.list(out)
  names(out) <- vars
  out
}

subtarget_deps_impl.split <- subtarget_deps_impl.combine <- function(
  dynamic,
  target,
  index,
  config
) {
  key <- which_by(dynamic)
  out <- list(1L)
  if (!no_by(dynamic)) {
    value <- get_dynamic_by(key, config)
    out <- list(which(value == unique(value)[[index]]))
  }
  names(out) <- which_var(dynamic)
  out
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

get_dynamic_size <- function(target, config) {
  if (ht_exists(config$ht_dynamic_size, target)) {
    return(ht_get(config$ht_dynamic_size, target))
  }
  size <- config$cache$get(target, namespace = "meta")$size
  stopifnot(size > 0L)
  ht_set(config$ht_dynamic_size, x = target, value = size)
  size
}

get_dynamic_by <- function(target, config) {
  if (exists(target, envir = config$envir, inherits = FALSE)) {
    return(get(target, envir = config$envir, inherits = FALSE))
  }
  load_by(target, config)
  get(target, envir = config$envir_by, inherits = FALSE)
}

load_by <- function(target, config) {
  if (exists(target, envir = config$envir_by, inherits = FALSE)) {
    return()
  }
  value <- config$cache$get(target, use_cache = FALSE)
  assign(target, value, envir = config$envir_by)
  invisible()
}

no_by <- function(dynamic) {
  is.null(dynamic$.by)
}

which_var <- function(dynamic) {
  deparse(dynamic$.x)
}

which_by <- function(dynamic) {
  deparse(dynamic$.by)
}
