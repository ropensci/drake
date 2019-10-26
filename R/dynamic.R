register_subtargets <- function(target, config) {
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
    seq_along(subtargets),
    subtarget_layout,
    parent = target,
    subtargets = subtargets,
    config = config
  )
  names(subtarget_layouts) <- subtargets
  config$layout[[target]]$subtargets <- subtargets
  config$layout <- c(config$layout, subtarget_layouts)
  config
}

subtarget_layout <- function(index, parent, subtargets, config) {
  subtarget <- subtargets[index]
  layout <- config$layout[[parent]]
  layout$target <- subtarget
  layout$dynamic <- NULL
  layout$subtarget <- TRUE
  layout$subtarget_index <- index
  layout$subtarget_parent <- parent
  layout$seed <- seed_from_basic_types(config$seed, layout$seed, subtarget)
  layout <- register_dynamic_subdeps(layout, index, parent, config)
  layout
}

register_dynamic_subdeps <- function(layout, index, parent, config) {
  index_deps <- subtarget_deps(parent, index, config)
  for (dep in layout$deps_dynamic) {
    if (is_dynamic(dep, config)) {
      subdep <- config$layout[[dep]]$subtargets[[index_deps[[dep]]]]
      layout$deps_build$memory <- c(layout$deps_build$memory, subdep)
      layout$deps_build$memory <- setdiff(layout$deps_build$memory, dep)
    }
  }
  layout
}

is_dynamic <- function(target, config) {
  inherits(config$layout[[target]]$dynamic, "dynamic")
}

is_dynamic_dep <- function(target, config) {
  ht_exists(config$ht_dynamic_deps, target)
}

is_subtarget <- function(target, config) {
  config$layout[[target]]$subtarget %||% FALSE
}

as_dynamic <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  class(x) <- c(x[[1]], "dynamic", class(x))
  match_call(x)
}

dynamic_subvalue <- function(value, index) {
  UseMethod("dynamic_subvalue")
}

dynamic_subvalue.data.frame <- function(value, index) {
  value[index,, drop = FALSE] # nolint
}

dynamic_subvalue.default <- function(value, index) {
  if (is.null(dim(value))) {
    dynamic_subvalue_vector(value, index)
  } else {
    dynamic_subvalue_array(value, index)
  }
}

dynamic_subvalue_array <- function(value, index) {
  ref <- slice.index(value, 1L)
  out <- value[ref %in% index]
  dim <- dim(value)
  dim[1] <- length(index)
  dim(out) <- dim
  out
}

dynamic_subvalue_vector <- function(value, index) {
  value[index]
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
  deps <- config$layout[[target]]$deps_dynamic
  hashes <- lapply(deps, read_dynamic_hashes, config = config)
  hashes <- do.call(paste, hashes)
  hashes <- vapply(hashes, shorten_dynamic_hash, FUN.VALUE = character(1))
  paste(target, hashes, sep = "_")
}

shorten_dynamic_hash <- function(hash) {
  digest::digest(hash, algo = "murmur32", serialize = FALSE)
}

read_dynamic_hashes <- function(target, config) {
  meta <- config$cache$get(target, namespace = "meta")
  if (is.null(meta$dynamic_hashes)) {
    value <- config$cache$get(target)
    meta$dynamic_hashes <- dynamic_hashes(meta$size, value, config)
  }
  meta$dynamic_hashes
}

number_subtargets <- function(target, config) {
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
