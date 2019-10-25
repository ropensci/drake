register_subtargets <- function(target, config) {
  if (!is_dynamic(target, config)) {
    return(config)
  }
  subtargets <- subtarget_names(target, config)
  edgelist <- do.call(rbind, lapply(subtargets, c, target))
  subgraph <- igraph::graph_from_edgelist(edgelist)
  subgraph <- igraph::set_vertex_attr(
    subgraph,
    name = "imported",
    value = FALSE
  )
  config$graph <- igraph::graph.union(config$graph, subgraph)
  subtarget_layouts <- lapply(
    subtargets,
    subtarget_layout,
    parent = target,
    config = config
  )
  names(subtarget_layouts) <- subtargets
  config$layout <- c(config$layout, subtarget_layouts)
  config
}

subtarget_layout <- function(subtarget, parent, config) {
  layout <- config$layout[[parent]]
  layout$target <- subtarget
  layout$dynamic <- NULL
  layout$subtarget <- TRUE
  layout$seed <- seed_from_basic_types(config$seed, layout$seed, subtarget)
  layout
}

is_dynamic <- function(target, config) {
  inherits(config$layout[[target]]$dynamic, "dynamic")
}

is_subtarget <- function(target, config) {
  config$layout[[target]]$subtarget
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

dynamic_dep_elt <- function(value, index) {
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

subtarget_names <- function(target, config) {
  if (!is_dynamic(target, config)) {
    return(character(0))
  }
  subtarget_name(target, seq_len(number_subtargets(target, config)))
}

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
