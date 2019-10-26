#' @title List sub-targets
#' @description List the sub-targets of a dynamic target.
#' @export
#' @return Character vector of sub-target names
#' @inheritParams diagnose
#' @param target Character string or symbol, depending on `character_only`.
#'   Name of a dynamic target.
#' @examples
#' plan <- drake_plan(
#'   x = seq_len(4),
#'   y = target(x + 1, dynamic = map(x))
#' )
#' make(plan)
#' subtargets(y)
#' readd(subtargets(y)[1], character_only = TRUE)
#' readd(subtargets(y)[2], character_only = TRUE)
subtargets <- function(
  target = NULL,
  character_only = FALSE,
  cache = drake::drake_cache(path = path),
  path = NULL
) {
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  diagnose(
    target = target,
    character_only = TRUE,
    cache = cache,
    path = path
  )$subtargets
}

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
      subdeps <- config$layout[[dep]]$subtargets[index_deps[[dep]]]
      layout$deps_build$memory <- c(layout$deps_build$memory, subdeps)
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

match_call_impl.combine <- function(dynamic) {
  match.call(definition = def_combine, call = dynamic)
}

# nocov start
def_map <- function(...) {
  NULL
}

def_combine <- function(..., .by = NULL) {
  NULL
}
# nocov end

subtarget_names <- function(target, config) {
  dynamic <- config$layout[[target]]$dynamic
  hashes <- dynamic_hash_list(dynamic, target, config)
  hashes <- subtarget_hashes(dynamic, target, hashes, config)
  hashes <- vapply(hashes, shorten_dynamic_hash, FUN.VALUE = character(1))
  out <- paste(target, hashes, sep = "_")
  make_unique(out)
}

shorten_dynamic_hash <- function(hash) {
  digest::digest(hash, algo = "murmur32", serialize = FALSE)
}

dynamic_hash_list <- function(dynamic, target, config) {
  UseMethod("dynamic_hash_list")
}

dynamic_hash_list.map <- function(dynamic, target, config) {
  deps <- config$layout[[target]]$deps_dynamic
  hashes <- lapply(deps, read_dynamic_hashes, config = config)
}

dynamic_hash_list.cross <- dynamic_hash_list.map

dynamic_hash_list.combine <- function(dynamic, target, config) {
  out <- lapply(which_vars(dynamic), read_dynamic_hashes, config = config)
  if (!is.null(dynamic$.by)) {
    out$by <- read_dynamic_hashes(deparse(dynamic$.by), config)
  }
  out
}

read_dynamic_hashes <- function(target, config) {
  meta <- config$cache$get(target, namespace = "meta")
  if (is.null(meta$dynamic_hashes)) {
    value <- config$cache$get(target)
    meta$dynamic_hashes <- dynamic_hashes(meta$size, value, config)
  }
  meta$dynamic_hashes
}

subtarget_hashes <- function(dynamic, target, hashes, config) {
  UseMethod("subtarget_hashes")
}

subtarget_hashes.map <- function(dynamic, target, hashes, config) {
  do.call(paste, hashes)
}

subtarget_hashes.cross <- function(dynamic, target, hashes, config) {
  hashes <- rev(expand.grid(rev(hashes)))
  apply(hashes, 1, paste, collapse = " ")
}

subtarget_hashes.combine <- function(dynamic, target, hashes, config) {
  if (is.null(hashes$by)) {
    return(lapply(hashes, paste, collapse = " "))
  }
  by <- hashes$by
  by <- ordered(by, levels = unique(by))
  hashes$by <- NULL
  hashes <- do.call(paste, hashes)
  unname(tapply(hashes, INDEX = by, FUN = paste, collapse = " "))
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

subtarget_deps_impl.combine <- function(
  dynamic,
  target,
  index,
  config
) {
  vars <- which_vars(dynamic)
  if (no_by(dynamic)) {
    subtarget_index <- seq_len(get_dynamic_size(vars[1], config))
  } else {
    value <- get_dynamic_by(which_by(dynamic), config)
    subtarget_index <- which(value == unique(value)[[index]])
  }
  out <- replicate(length(vars), subtarget_index, simplify = FALSE)
  names(out) <- vars
  out
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

which_vars <- function(dynamic) {
  dynamic$.by <- NULL
  all.vars(dynamic)
}

which_by <- function(dynamic) {
  deparse(dynamic$.by)
}
