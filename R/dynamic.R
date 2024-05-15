#' @title List sub-targets `r lifecycle::badge("stable")`
#' @description List the sub-targets of a dynamic target.
#' @export
#' @seealso [get_trace()], [read_trace()]
#' @return Character vector of sub-target names
#' @inheritParams diagnose
#' @param target Character string or symbol, depending on `character_only`.
#'   Name of a dynamic target.
#' @examples
#' \dontrun{
#' isolate_example("dynamic branching", {
#' plan <- drake_plan(
#'   w = c("a", "a", "b", "b"),
#'   x = seq_len(4),
#'   y = target(x + 1, dynamic = map(x)),
#'   z = target(sum(x) + sum(y), dynamic = group(x, y, .by = w))
#' )
#' make(plan)
#' subtargets(y)
#' subtargets(z)
#' readd(x)
#' readd(y)
#' readd(z)
#' })
#' }
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

#' @title Read a trace of a dynamic target.
#' `r lifecycle::badge("stable")`
#' @export
#' @seealso [get_trace()], [subtargets()]
#' @description Read a target's dynamic trace from the cache.
#'   Best used on its own outside a `drake` plan.
#' @details In dynamic branching, the trace keeps track
#'   of how the sub-targets were generated.
#'   It reminds us the values of grouping variables
#'   that go with individual sub-targets.
#' @return The dynamic trace of one target in another:
#'   a vector of values from a grouping variable.
#' @inheritParams readd
#' @param trace Character, name of the trace
#'   you want to extract. Such trace names are declared
#'   in the `.trace` argument of `map()`, `cross()` or `group()`.
#' @param target Symbol or character,
#'   depending on the value of `character_only`.
#'   `target` is T=the name of a dynamic target with one or more traces
#'   defined using the `.trace` argument of dynamic `map()`, `cross()`,
#'   or `group()`.
#' @examples
#' \dontrun{
#' isolate_example("demonstrate dynamic trace", {
#' plan <- drake_plan(
#'   w = LETTERS[seq_len(3)],
#'   x = letters[seq_len(2)],
#'
#'   # The first trace lets us see the values of w
#'   # that go with the sub-targets of y.
#'   y = target(paste0(w, x), dynamic = cross(w, x, .trace = w)),
#'
#'   # We can use the trace as a grouping variable for the next
#'   # group().
#'   w_tr = read_trace("w", y),
#'
#'   # Now, we use the trace again to keep track of the
#'   # values of w corresponding to the sub-targets of z.
#'   z = target(
#'     paste0(y, collapse = "-"),
#'     dynamic = group(y, .by = w_tr, .trace = w_tr)
#'   )
#' )
#' make(plan)
#'
#' # We can read the trace outside make().
#' # That way, we know which values of `w` correspond
#' # to the sub-targets of `y`.
#' readd(y)
#' read_trace("w", y)
#'
#' # And we know which values of `w_tr` (and thus `w`)
#' # match up with the sub-targets of `y`.
#' readd(z)
#' read_trace("w_tr", z)
#' })
#' }
read_trace <- function(
  trace,
  target,
  cache = drake::drake_cache(path = path),
  path = NULL,
  character_only = FALSE
) {
  if (is.null(cache)) {
    stop("cannot find drake cache.")
  }
  cache <- decorate_storr(cache)
  if (!character_only) {
    target <- as.character(substitute(target))
  }
  value <- cache$get(standardize_key(target), use_cache = FALSE)
  attr(value, "dynamic_trace")[[trace]]
}

dynamic_build <- function(target, meta, config) {
  subtargets <- config$spec[[target]]$subtargets
  if (config$settings$log_build_times) {
    meta$time_command <- proc_time() - meta$time_start
  }
  value <- config$cache$mget_hash(subtargets)
  value <- append_trace(target, value, config)
  class(value) <- "drake_dynamic"
  list(target = target, meta = meta, value = value)
}

append_trace <- function(target, value, config) {
  spec <- config$spec[[target]]
  if (!length(spec$deps_dynamic_trace)) {
    return(value)
  }
  dynamic <- spec$dynamic
  trace <- get_trace_impl(dynamic, value, spec, config)
  trace <- subset_trace(trace, config)
  attr(value, "dynamic_trace") <- trace
  value
}

subset_trace <- function(trace, config) {
  if (is.null(config$max_expand)) {
    return(trace)
  }
  lapply(trace, function(x) {
    dynamic_subvalue(x, index = seq_len(config$max_expand))
  })
}

get_trace_impl <- function(dynamic, value, spec, config) {
  UseMethod("get_trace_impl")
}

#' @export
get_trace_impl.map <- function(dynamic, value, spec, config) {
  trace <- lapply(
    spec$deps_dynamic_trace,
    get,
    envir = config$envir_targets,
    inherits = FALSE
  )
  trace <- lapply(trace, chr_dynamic)
  names(trace) <- spec$deps_dynamic_trace
  trace
}

#' @export
get_trace_impl.cross <- function(dynamic, value, spec, config) {
  size <- lapply(
    spec$deps_dynamic,
    get_dynamic_size,
    config = config
  )
  index <- lapply(size, seq_len)
  names(index) <- spec$deps_dynamic
  index <- rev(expand.grid(rev(index)))
  trace <- lapply(
    spec$deps_dynamic_trace,
    get,
    envir = config$envir_targets,
    inherits = FALSE
  )
  trace <- lapply(trace, chr_dynamic)
  names(trace) <- spec$deps_dynamic_trace
  trace <- lapply(spec$deps_dynamic_trace, function(key) {
    trace[[key]][index[[key]]]
  })
  names(trace) <- spec$deps_dynamic_trace
  trace
}

#' @export
get_trace_impl.group <- function(dynamic, value, spec, config) {
  by_key <- which_by(dynamic)
  by_value <- get(by_key, envir = config$envir_targets, inherits = FALSE)
  trace <- list(unique(by_value))
  names(trace) <- by_key
  trace
}

chr_dynamic <- function(x) {
  chr_dynamic_impl(x)
}

chr_dynamic_impl <- function(x) {
  UseMethod("chr_dynamic_impl")
}

#' @export
chr_dynamic_impl.drake_dynamic <- function(x) {
  as.character(x)
}

#' @export
chr_dynamic_impl.default <- function(x) {
  x
}

register_subtargets <- function(target, static_ok, dynamic_ok, config) {
  on.exit(register_dynamic(target, config))
  subtargets_build <- subtargets_all <- subtarget_names(target, config)
  preregister_subtargets(target, subtargets_all, config)
  if (static_ok) {
    subtargets_build <- filter_subtargets(target, subtargets_all, config)
  }
  namespace <- config$meta[[target]]$dynamic_progress_namespace
  already_done <- config$cache$list(namespace = namespace)
  already_done <- intersect(already_done, ht_list(config$ht_target_exists))
  subtargets_build <- setdiff(subtargets_build, already_done)
  config$spec[[target]]$subtargets_build <- subtargets_build
  if (length(subtargets_all)) {
    register_in_graph(target, subtargets_all, config)
    register_in_spec(target, subtargets_all, config)
  }
  ndeps <- length(subtargets_build)
  if (ndeps) {
    announce_build(target, config)
  }
  if (ndeps) {
    config$logger$disk("register", ndeps, "subtargets", target = target)
    config$logger$inc_progress_total(ndeps)
    ht_set(config$ht_is_subtarget, subtargets_build)
    register_in_loop(subtargets_build, config)
    register_in_queue(subtargets_build, 0, config)
    register_in_counter(subtargets_build, config)
  }
  if (!static_ok || !dynamic_ok || ndeps) {
    register_in_loop(target, config)
    register_in_queue(target, ndeps, config)
    register_in_counter(target, config)
    dynamic_pad_revdep_keys(target, config)
  }
}

preregister_subtargets <- function(target, subtargets, config) {
  register_subtarget_parents(target, subtargets, config)
}

register_subtarget_parents <- function(target, subtargets, config) {
  ht_set(config$ht_subtarget_parents, x = subtargets, value = target)
}

subtarget_parent <- function(subtarget, config) {
  ht_get(config$ht_subtarget_parents, subtarget)
}

filter_subtargets <- function(target, subtargets, config) {
  ht_set(config$ht_is_subtarget, subtargets)
  index <- check_subtarget_triggers(target, subtargets, config)
  subtargets <- subtargets[index]
  if (!config$settings$recover || !length(subtargets)) {
    return(subtargets)
  }
  recovered <- lightly_parallelize(
    subtargets,
    recover_subtarget,
    jobs = config$settings$jobs_preprocess,
    config = config,
    parent = target
  )
  subtargets[!unlist(recovered)]
}

register_dynamic <- function(target, config) {
  ht_set(config$ht_dynamic, target)
}

is_registered_dynamic <- function(target, config) {
  ht_exists(config$ht_dynamic, target)
}

register_in_graph <- function(target, subtargets, config) {
  edgelist <- do.call(rbind, lapply(subtargets, c, target))
  subgraph <- igraph::graph_from_edgelist(edgelist)
  subgraph <- igraph::set_vertex_attr(
    subgraph,
    name = "imported",
    value = FALSE
  )
  config$envir_graph$graph <- igraph::union(
    config$envir_graph$graph,
    subgraph
  )
}

register_in_spec <- function(target, subtargets, config) {
  spec <- config$spec[[target]]
  dynamic <- spec$dynamic
  lapply(
    seq_along(subtargets),
    register_subtarget_spec,
    parent = target,
    subtargets = subtargets,
    spec = spec,
    dynamic = dynamic,
    config = config
  )
  config$spec[[target]]$subtargets <- subtargets
}

register_subtarget_spec <- function(
  index,
  parent,
  subtargets,
  dynamic,
  spec,
  config
) {
  subtarget <- subtargets[index]
  spec$target <- subtarget
  spec$subtarget_index <- index
  spec$subtarget_parent <- parent
  mem_deps <- all.vars(spec$dynamic)
  spec$dynamic <- NULL
  spec$deps_build$memory <- unique(c(spec$deps_build$memory, mem_deps))
  spec$seed <- seed_from_basic_types(config$settings$seed, spec$seed, subtarget)
  spec <- register_dynamic_subdeps(dynamic, spec, index, parent, config)
  assign(
    x = subtarget,
    value = spec,
    envir = config$spec,
    inherits = FALSE
  )
}

register_in_loop <- function(targets, config) {
  if (is.null(config$envir_loop)) {
    return()
  }
  config$envir_loop$targets <- c(targets, config$envir_loop$targets)
}

register_in_queue <- function(targets, ndeps, config) {
  if (is.null(config$queue)) {
    return()
  }
  config$queue$push(targets, ndeps)
}

register_in_counter <- function(targets, config) {
  if (is.null(config$counter)) {
    return()
  }
  config$counter$remaining <- config$counter$remaining + length(targets)
}

dynamic_pad_revdep_keys <- function(target, config) {
  if (is.null(config$queue)) {
    return()
  }
  increase_revdep_keys(config$queue, target, config)
}

register_dynamic_subdeps <- function(dynamic, spec, index, parent, config) {
  index_deps <- subtarget_deps(dynamic, parent, index, config)
  for (dep in spec$deps_dynamic) {
    assert_legal_branching_format(parent, dep, config)
    if (is_dynamic(dep, config)) {
      subdeps <- config$spec[[dep]]$subtargets[index_deps[[dep]]]
      spec$deps_build$memory <- c(spec$deps_build$memory, subdeps)
      spec$deps_build$memory <- setdiff(spec$deps_build$memory, dep)
    }
  }
  spec
}

assert_legal_branching_format <- function(parent, dep, config) {
  bad_dep <- !is_dynamic(dep, config) &&
    identical(config$spec[[dep]]$format, "file")
  if (bad_dep) {
    stop0(
      "Illegal dynamic branching of target `", parent, "` over `", dep, "`. ",
      "If `", parent, "` dynamically branches over the dynamic file target `",
      dep, "`, then `", dep, "` must also use dynamic branching."
    )
  }
}

is_dynamic <- function(target, config) {
  ht_exists(config$ht_is_dynamic, target)
}

is_dynamic_dep <- function(target, config) {
  ht_exists(config$ht_dynamic_deps, target)
}

is_subtarget <- function(target, config) {
  ht_exists(config$ht_is_subtarget, target)
}

as_dynamic <- function(x) {
  if (!is.call(x)) {
    return(x)
  }
  class(x) <- c(x[[1]], "dynamic", class(x))
  match_dynamic_call(x)
}

dynamic_subvalue <- function(value, index) {
  value <- undecorate_format_value(value)
  vec_slice(x = value, i = index)
}

match_dynamic_call <- function(dynamic) {
  class <- class(dynamic)
  out <- match_dynamic_call_impl(dynamic)
  class(out) <- class
  out
}

match_dynamic_call_impl <- function(dynamic) {
  UseMethod("match_dynamic_call_impl")
}

#' @export
match_dynamic_call_impl.map <- function(dynamic) {
  match.call(definition = def_map, call = dynamic)
}

#' @export
match_dynamic_call_impl.cross <- match_dynamic_call_impl.map

#' @export
match_dynamic_call_impl.combine <- function(dynamic) { # nolint
  stop0(
    "Dynamic combine() does not exist. ",
    "use group() instead. ",
    "Ref: https://github.com/ropensci/drake/issues/1065"
  )
}

#' @export
match_dynamic_call_impl.group <- function(dynamic) {
  match.call(definition = def_group, call = dynamic)
}

# nocov start
def_map <- function(..., .trace = NULL) {
  NULL
}

def_group <- function(..., .by = NULL, .trace = NULL) {
  NULL
}
# nocov end

subtarget_names <- function(target, config) {
  spec <- config$spec[[target]]
  dynamic <- spec$dynamic
  hashes <- dynamic_hash_list(dynamic, target, config)
  hashes <- subtarget_hashes(dynamic, target, hashes, config)
  hashes <- vapply(hashes, shorten_dynamic_hash, FUN.VALUE = character(1))
  out <- paste(target, hashes, sep = "_")
  out <- make_unique(out)
  max_expand_dynamic(out, spec, config)
}

max_expand_dynamic <- function(targets, spec, config) {
  max_expand <- spec$max_expand %||NA% config$max_expand
  if (is.null(max_expand)) {
    return(targets)
  }
  size <- min(length(targets), max_expand)
  targets[seq_len(size)]
}

shorten_dynamic_hash <- function(hash) {
  digest_murmur32(hash, serialize = FALSE)
}

dynamic_hash_list <- function(dynamic, target, config) {
  UseMethod("dynamic_hash_list")
}

#' @export
dynamic_hash_list.map <- function(dynamic, target, config) {
  deps <- sort(config$spec[[target]]$deps_dynamic)
  hashes <- lapply(deps, read_dynamic_hashes, config = config)
  assert_equal_branches(target, deps, hashes)
  names(hashes) <- deps
  hashes
}

#' @export
dynamic_hash_list.cross <- function(dynamic, target, config) {
  deps <- config$spec[[target]]$deps_dynamic
  hashes <- lapply(deps, read_dynamic_hashes, config = config)
  names(hashes) <- deps
  hashes
}

#' @export
dynamic_hash_list.group <- function(dynamic, target, config) {
  deps <- sort(which_vars(dynamic))
  out <- lapply(deps, read_dynamic_hashes, config = config)
  if (!is.null(dynamic$.by)) {
    key <- direct_deparse(
      dynamic$.by,
      control = deparse_control_default,
      backtick = FALSE
    )
    out[["_by"]] <- read_dynamic_hashes(key, config)
  }
  assert_equal_branches(target, which_vars(dynamic), out)
  out
}

assert_equal_branches <- function(target, deps, hashes) {
  lengths <- vapply(hashes, length, FUN.VALUE = integer(1))
  if (length(unique(lengths)) == 1L) {
    return()
  }
  keep <- !duplicated(lengths)
  deps <- deps[keep]
  lengths <- lengths[keep]
  deps[is.na(deps)] <- ".by"
  stop0(
    "for dynamic map() and group(), all grouping variables ",
    "must have equal lengths. For target ", target,
    ", the lengths of ", paste(deps, collapse = ", "),
    " were ", paste0(lengths, collapse = ", "),
    ", respectively."
  )
}

read_dynamic_hashes <- function(target, config) {
  meta <- config$cache$get(target, namespace = "meta", use_cache = FALSE)
  if (is.null(meta$dynamic_hashes)) {
    value <- config$cache$get(target, use_cache = FALSE)
    meta$dynamic_hashes <- dynamic_hashes(value, meta$size_vec, config)
  }
  meta$dynamic_hashes
}

subtarget_hashes <- function(dynamic, target, hashes, config) {
  UseMethod("subtarget_hashes")
}

#' @export
subtarget_hashes.map <- function(dynamic, target, hashes, config) {
  do.call(paste, hashes)
}

#' @export
subtarget_hashes.cross <- function(dynamic, target, hashes, config) {
  deps <- all.vars(dynamic)
  hashes <- hashes[deps]
  hashes <- expand.grid(rev(hashes))
  hashes <- hashes[, sort(colnames(hashes)), drop = FALSE]
  apply(hashes, 1, paste, collapse = " ")
}

#' @export
subtarget_hashes.group <- function(dynamic, target, hashes, config) {
  if (is.null(hashes[["_by"]])) {
    return(lapply(hashes, paste, collapse = " "))
  }
  by <- hashes[["_by"]]
  by <- ordered(by, levels = unique(by))
  hashes[["_by"]] <- NULL
  hashes <- do.call(paste, hashes)
  unname(tapply(hashes, INDEX = by, FUN = paste, collapse = " "))
}

subtarget_deps <- function(dynamic, target, index, config) {
  UseMethod("subtarget_deps")
}

#' @export
subtarget_deps.map <- function(dynamic, target, index, config) {
  vars <- all.vars(dynamic)
  out <- as.list(rep(as.integer(index), length(vars)))
  names(out) <- vars
  out
}

#' @export
subtarget_deps.cross <- function(dynamic, target, index, config) {
  vars <- all.vars(dynamic)
  size <- unlist(lapply(vars, get_dynamic_size, config = config))
  out <- grid_index(index, size)
  out <- as.list(out)
  names(out) <- vars
  out
}

# Get a row of expand_grid from tidyr
# without actually expanding the grid.
grid_index <- function(index, size) {
  reps <- prod(size) / cumprod(size)
  inc <- ceiling(index / reps) - 1L
  (inc %% size) + 1L
}

#' @export
subtarget_deps.group <- function(
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
  size <- config$cache$get(
    target,
    namespace = "meta",
    use_cache = FALSE
  )$size_vec
  assert_dynamic_size(target, size)
  ht_set(config$ht_dynamic_size, x = target, value = size)
  size
}

assert_dynamic_size <- function(target, size) {
  if (size < 1L) {
    stop0(
      "cannot dynamically branch over ", target, " because NROW(",
      target, ") is 0."
    )
  }
}

get_dynamic_by <- function(target, config) {
  if (exists(target, envir = config$envir, inherits = FALSE)) {
    return(get(target, envir = config$envir, inherits = FALSE))
  }
  load_by(target, config)
  get(target, envir = config$envir_targets, inherits = FALSE)
}

load_by <- function(target, config) {
  if (exists(target, envir = config$envir_targets, inherits = FALSE)) {
    return()
  }
  value <- config$cache$get(target, use_cache = FALSE)
  assign(target, value, envir = config$envir_targets)
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
  direct_deparse(
    dynamic$.by,
    control = deparse_control_default,
    backtick = FALSE
  )
}
