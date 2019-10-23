# local dynamic branching

local_dynamic <- function(target, config) {
  dynamic <- config$layout[[target]]$dynamic
  local_dynamic_impl(dynamic, target, config)
}

local_dynamic_impl <- function(dynamic, target, config) {
  UseMethod("local_dynamic_impl")
}

local_dynamic_impl.default <- function(dynamic, target, config) {
  return()
}

local_dynamic_impl.dynamic <- function(dynamic, target, config) {
  subtargets <- number_subtargets_impl(dynamic, target, config)
  lapply(
    seq_len(subtargets),
    local_subtarget,
    dynamic = dynamic,
    target = target,
    config = config
  )
}

local_subtarget <- function(index, dynamic, target, config) {
  deps <- subtarget_deps_impl(dynamic, target, index, config)
  manage_memory_dynamic(dynamic, deps, config)


  browser()
  # Continue here. Run the sub-target and store the value.
}

manage_memory_dynamic <- function(dynamic, deps, config) {
  args <- list(key = names(deps), value = deps)
  lapply(names(deps), function(dep) {
    load_dynamic_dep(dynamic, dep = dep, index = deps[[dep]], config = config)
  })
}

load_dynamic_dep <- function(dynamic, dep, index, config) {
  UseMethod("load_dynamic_dep")
}

load_dynamic_dep.map <- function(dynamic, dep, index, config) {
  if (exists(dep, envir = config$envir_targets, inherits = FALSE)) {
    envir <- config$envir_targets
  } else {
    envir <- config$envir
  }
  value <- get(dep, envir = envir, inherits = FALSE)
  value <- dynamic_elt(value, index)
  assign(dep, value, envir = config$envir_dynamic, inherits = FALSE)
}

load_dynamic_dep.cross <- load_dynamic_dep.map

# utilities

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

get_dynamic <- function(target, config) {
  config$layout[[target]]$dynamic
}

dynamic_elt <- function(value, index) {
  if (is.null(dim(value))) {
    value[[index]]
  } else {
    drake_slice(value, slices = dim(value)[1], index = index)
  }
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
