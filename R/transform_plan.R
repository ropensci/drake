#' @title Transformations in `drake_plan()`. `r lifecycle::badge("stable")`
#' @name transformations
#' @aliases map split cross combine group
#' @description In [drake_plan()], you can define whole batches
#'   of targets with transformations such as
#'   `map()`, `split()`, `cross()`, and `combine()`.
#' @details For details, see
#'   `https://books.ropensci.org/drake/plans.html#large-plans`.
#' @section Transformations:
#'  `drake` has special syntax for generating large plans.
#'  Your code will look something like
#'  `drake_plan(y = target(f(x), transform = map(x = c(1, 2, 3)))`
#'  You can read about this interface at
#'  `https://books.ropensci.org/drake/plans.html#large-plans`. # nolint
#' @section Static branching:
#'   In static branching, you define batches of targets
#'   based on information you know in advance.
#'   Overall usage looks like
#'   `drake_plan(<x> = target(<...>, transform = <call>)`,
#'   where
#'   - `<x>` is the name of the target or group of targets.
#'   - `<...>` is optional arguments to [target()].
#'   - `<call>` is a call to one of the transformation functions.
#'
#'   Transformation function usage:
#'   - `map(..., .data, .names, .id, .tag_in, .tag_out)`
#'   - `split(..., slices, margin = 1L, drop = FALSE, .names, .tag_in, .tag_out)` # nolint
#'   - `cross(..., .data, .names, .id, .tag_in, .tag_out)`
#'   - `combine(..., .by, .names, .id, .tag_in, .tag_out)`
#' @section Dynamic branching:
#'   - `map(..., .trace)`
#'   - `cross(..., .trace)`
#'   - `group(..., .by, .trace)`
#'
#'  `map()` and `cross()` create dynamic sub-targets from the variables
#'  supplied to the dots. As with static branching, the variables
#'  supplied to `map()` must all have equal length.
#'   `group(f(data), .by = x)` makes new dynamic
#'   sub-targets from `data`. Here, `data` can be either static or dynamic.
#'   If `data` is dynamic, `group()` aggregates existing sub-targets.
#'   If `data` is static, `group()` splits `data` into multiple
#'   subsets based on the groupings from `.by`.
#'
#'  Differences from static branching:
#'  - `...` must contain *unnamed* symbols with no values supplied,
#'    and they must be the names of targets.
#'  - Arguments `.id`, `.tag_in`, and `.tag_out` no longer apply.
#'
#' @param ... Grouping variables. New grouping variables must be
#'   supplied with their names and values, existing grouping variables
#'   can be given as symbols without any values assigned.
#'   For dynamic branching, the entries in `...` must be unnamed symbols
#'   with no values supplied, and they must be the names of targets.
#' @param .data A data frame of new grouping variables with
#'   grouping variable names as column names and values as elements.
#' @param .names Literal character vector of names for the targets.
#'   Must be the same length as the targets generated.
#' @param .id Symbol or vector of symbols naming grouping variables
#'   to incorporate into target names. Useful for creating short target
#'   names. Set `.id = FALSE` to use integer indices as target name suffixes.
#' @param .tag_in A symbol or vector of symbols. Tags assign targets
#'   to grouping variables. Use `.tag_in` to assign *untransformed*
#'   targets to grouping variables.
#' @param .tag_out Just like `.tag_in`, except that `.tag_out`
#'   assigns *transformed* targets to grouping variables.
#' @param slice Number of slices into which `split()` partitions the data.
#' @param margin Which margin to take the slices in `split()`. Same meaning
#'   as the `MARGIN` argument of `apply()`.
#' @param drop Logical, whether to drop a dimension if its length is 1.
#'   Same meaning as `mtcars[, 1L, drop = TRUE]` versus
#'   `mtcars[, 1L, drop = TRUE]`.
#' @param .by Symbol or vector of symbols of grouping variables.
#'   `combine()` aggregates/groups targets by the grouping variables in `.by`.
#'   For dynamic branching, `.by` can only take one variable at a time,
#'   and that variable must be a vector. Ideally, it should take
#'   little space in memory.
#' @param .trace Symbol or vector of symbols for the dynamic trace.
#'   The dynamic trace allows you to keep track of the values of
#'   dynamic dependencies are associated with individual sub-targets.
#'   For `combine()`, `.trace` must either be empty or the same as the
#'   variable given for `.by`.
#'   See [get_trace()] and [read_trace()] for examples and other details.
#' @examples
#' # Static branching
#' models <- c("glm", "hierarchical")
#' plan <- drake_plan(
#'   data = target(
#'     get_data(x),
#'     transform = map(x = c("simulated", "survey"))
#'   ),
#'   analysis = target(
#'     analyze_data(data, model),
#'     transform = cross(data, model = !!models, .id = c(x, model))
#'   ),
#'   summary = target(
#'     summarize_analysis(analysis),
#'     transform = map(analysis, .id = c(x, model))
#'   ),
#'   results = target(
#'     bind_rows(summary),
#'     transform = combine(summary, .by = data)
#'   )
#' )
#' plan
#' if (requireNamespace("styler")) {
#'   print(drake_plan_source(plan))
#' }
#' # Static splitting
#' plan <- drake_plan(
#'   analysis = target(
#'     analyze(data),
#'     transform = split(data, slices = 3L, margin = 1L, drop = FALSE)
#'   )
#' )
#' print(plan)
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   print(drake_plan_source(plan))
#' }
#' # Static tags:
#' drake_plan(
#'   x = target(
#'     command,
#'     transform = map(y = c(1, 2), .tag_in = from, .tag_out = c(to, out))
#'   ),
#'   trace = TRUE
#' )
#' plan <- drake_plan(
#'   survey = target(
#'     survey_data(x),
#'     transform = map(x = c(1, 2), .tag_in = source, .tag_out = dataset)
#'   ),
#'   download = target(
#'     download_data(),
#'     transform = map(y = c(5, 6), .tag_in = source, .tag_out = dataset)
#'   ),
#'   analysis = target(
#'     analyze(dataset),
#'     transform = map(dataset)
#'   ),
#'   results = target(
#'     bind_rows(analysis),
#'     transform = combine(analysis, .by = source)
#'   )
#' )
#' plan
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   print(drake_plan_source(plan))
#' }
NULL

#' @title Transform a plan
#' `r lifecycle::badge("stable")`
#' @description Evaluate the `map()`, `cross()`, `split()` and
#'   `combine()` operations in the `transform` column of a
#'   `drake` plan.
#' @details `https://books.ropensci.org/drake/plans.html#large-plans` # nolint
#' @export
#' @seealso drake_plan, map, split, cross, combine
#' @param plan A `drake` plan with a `transform` column
#' @param envir Environment for tidy evaluation.
#' @param trace Logical, whether to add columns to show
#'   what happens during target transformations.
#' @param max_expand Positive integer, optional.
#'   `max_expand` is the maximum number of targets to generate in each
#'   `map()`, `split()`, or `cross()` transform.
#'   Useful if you have a massive plan and you want to
#'   test and visualize a strategic subset of targets
#'   before scaling up.
#'   Note: the `max_expand` argument of `drake_plan()` and
#'   `transform_plan()` is for static branching only.
#'   The dynamic branching `max_expand`
#'   is an argument of `make()` and `drake_config()`.
#' @param tidy_eval Logical, whether to use tidy evaluation
#'   (e.g. unquoting/`!!`) when resolving commands.
#'   Tidy evaluation in transformations is always turned on
#'   regardless of the value you supply to this argument.
#' @examples
#' plan1 <- drake_plan(
#'   y = target(
#'     f(x),
#'     transform = map(x = c(1, 2))
#'   ),
#'   transform = FALSE
#' )
#' plan2 <- drake_plan(
#'   z = target(
#'     g(y),
#'     transform = map(y, .id = x)
#'   ),
#'   transform = FALSE
#' )
#' plan <- bind_plans(plan1, plan2)
#' transform_plan(plan)
#' models <- c("glm", "hierarchical")
#' plan <- drake_plan(
#'   data = target(
#'     get_data(x),
#'     transform = map(x = c("simulated", "survey"))
#'   ),
#'   analysis = target(
#'     analyze_data(data, model),
#'     transform = cross(data, model = !!models, .id = c(x, model))
#'   ),
#'   summary = target(
#'     summarize_analysis(analysis),
#'     transform = map(analysis, .id = c(x, model))
#'   ),
#'   results = target(
#'     bind_rows(summary),
#'     transform = combine(summary, .by = data)
#'   )
#' )
#' plan
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   print(drake_plan_source(plan))
#' }
#' # Tags:
#' drake_plan(
#'   x = target(
#'     command,
#'     transform = map(y = c(1, 2), .tag_in = from, .tag_out = c(to, out))
#'   ),
#'   trace = TRUE
#' )
#' plan <- drake_plan(
#'   survey = target(
#'     survey_data(x),
#'     transform = map(x = c(1, 2), .tag_in = source, .tag_out = dataset)
#'   ),
#'   download = target(
#'     download_data(),
#'     transform = map(y = c(5, 6), .tag_in = source, .tag_out = dataset)
#'   ),
#'   analysis = target(
#'     analyze(dataset),
#'     transform = map(dataset)
#'   ),
#'   results = target(
#'     bind_rows(analysis),
#'     transform = combine(analysis, .by = source)
#'   )
#' )
#' plan
#' if (requireNamespace("styler", quietly = TRUE)) {
#'   print(drake_plan_source(plan))
#' }
transform_plan <- function(
  plan,
  envir = parent.frame(),
  trace = FALSE,
  max_expand = NULL,
  tidy_eval = TRUE
) {
  force(envir)
  transform_plan_(
    plan = plan,
    envir = envir,
    trace = trace,
    max_expand = max_expand,
    tidy_eval = tidy_eval,
    sanitize = TRUE
  )
}

transform_plan_ <- function(
  plan,
  envir,
  trace,
  max_expand,
  tidy_eval,
  sanitize
) {
  if (!("transform" %in% names(plan))) {
    return(plan)
  }
  old_cols(plan) <- old_cols <- colnames(plan)
  plan$transform <- tidyeval_exprs(plan$transform, envir = envir)
  plan <- convert_splits_to_maps(plan)
  plan$transform <- lapply(plan$transform, parse_transform)
  graph <- dsl_graph(plan)
  order <- igraph::topo_sort(graph)$name
  subplans <- split(plan, f = plan$target)
  for (target in order) {
    upstream_plan <- dsl_upstream_plan(target, graph, subplans)
    index <- which(target == upstream_plan$target)
    old_cols(upstream_plan) <- old_cols
    subplans[[target]] <- transform_row(index, upstream_plan, graph, max_expand)
  }
  plan <- drake_bind_rows(subplans)
  old_cols(plan) <- old_cols
  plan <- dsl_trace(plan = plan, trace = trace)
  old_cols(plan) <- plan$transform <- NULL
  plan <- dsl_tidy_eval(plan = plan, tidy_eval = tidy_eval, envir = envir)
  plan <- dsl_sanitize(plan = plan, sanitize = sanitize, envir = envir)
  plan
}

dsl_upstream_plan <- function(target, graph, subplans) {
  upstream_targets <- upstream_nodes(graph, target)
  drake_bind_rows(subplans[upstream_targets])
}

dsl_trace <- function(plan, trace) {
  if (!trace) {
    keep <- as.character(intersect(colnames(plan), old_cols(plan)))
    plan <- plan[, intersect(colnames(plan), old_cols(plan)), drop = FALSE]
  }
  plan
}

dsl_tidy_eval <- function(plan, tidy_eval, envir) {
  if (tidy_eval) {
    for (col in setdiff(colnames(plan), c("target", "transform"))) {
      plan[[col]] <- tidyeval_exprs(plan[[col]], envir = envir)
    }
  }
  plan
}

dsl_sanitize <- function(plan, sanitize, envir) {
  if (sanitize) {
    plan <- sanitize_plan(plan, envir = envir)
  }
  plan
}

convert_splits_to_maps <- function(plan) {
  fields <- c("target", "command", "transform")
  for (i in seq_len(nrow(plan))) {
    skip <- safe_is_na(plan$transform[[i]]) ||
      plan$transform[[i]][[1]] != quote(split)
    if (skip) {
      next
    }
    out <- convert_split_to_map(
      target = plan$target[[i]],
      command = plan$command[[i]],
      transform = plan$transform[[i]]
    )
    plan$command[[i]] <- out$command
    plan$transform[[i]] <- out$transform
  }
  plan
}

convert_split_to_map <- function(target, command, transform) {
  slice <- transform
  slice[[1]] <- quote(drake_slice)
  index <- paste0(target, "_index")
  slice$index <- as.symbol(index)
  args <- setdiff(names(transform), names(formals(drake_slice)))
  args <- args[nzchar(args)]
  for (arg in args) {
    slice[[arg]] <- NULL
  }
  arglist <- as.list(transform)[args]
  slice <- match.call(drake_slice, slice)
  sub <- list(slice = slice)
  names(sub) <- safe_deparse(slice$data, backtick = TRUE)
  command <- eval(call("substitute", command, sub), envir = baseenv())
  transform <- as.call(c(quote(map), slice$data))
  transform[[index]] <- as.numeric(seq_len(slice$slices))
  for (arg in args) {
    transform[[arg]] <- arglist[[arg]]
  }
  list(command = command, transform = transform)
}

#' @title Take a strategic subset of a dataset.
#' `r lifecycle::badge("stable")`
#' @description `drake_slice()` is similar to `split()`.
#'   Both functions partition data into disjoint subsets,
#'   but whereas `split()` returns *all* the subsets, `drake_slice()`
#'   returns just *one*. In other words, `drake_slice(..., index = i)`
#'   returns `split(...)[[i]]`.
#'   Other features:
#'     1. `drake_slice()` works on vectors, data frames,
#'        matrices, lists, and arbitrary arrays.
#'     2. Like `parallel::splitIndices()`, `drake_slice()` tries to
#'        distribute the data uniformly across subsets.
#' See the examples to learn why splitting is useful in `drake`.
#' @export
#' @return A subset of `data`.
#' @param data A list, vector, data frame, matrix, or arbitrary array.
#'   Anything with a `length()` or `dim()`.
#' @param slices Integer of length 1, number of slices (i.e. pieces)
#'   of the whole dataset. Remember, `drake_slice(index = i)` returns
#'   only slice number `i`.
#' @param index Integer of length 1, which piece of the partition to return.
#' @param margin Integer of length 1, margin over which to split the data.
#'   For example, for a data frame or matrix,
#'   use `margin = 1` to split over rows and `margin = 2`
#'   to split over columns. Similar to `MARGIN` in `apply()`.
#' @param drop Logical, for matrices and arrays.
#'   If `TRUE`,` the result is coerced to the lowest possible dimension.
#'   See ?`[` for details.
#' @examples
#' # Simple usage
#' x <- matrix(seq_len(20), nrow = 5)
#' x
#' drake_slice(x, slices = 3, index = 1)
#' drake_slice(x, slices = 3, index = 2)
#' drake_slice(x, slices = 3, index = 3)
#' drake_slice(x, slices = 3, margin = 2, index = 1)
#' # In drake, you can split a large dataset over multiple targets.
#' \dontrun{
#' isolate_example("contain side effects", {
#' plan <- drake_plan(
#'   large_data = mtcars,
#'   data_split = target(
#'     drake_slice(large_data, slices = 32, index = i),
#'     transform = map(i = !!seq_len(32))
#'   )
#' )
#' plan
#' cache <- storr::storr_environment()
#' make(plan, cache = cache, session_info = FALSE, verbose = FALSE)
#' readd(data_split_1L, cache = cache)
#' readd(data_split_2L, cache = cache)
#' })
#' }
drake_slice <- function(data, slices, index, margin = 1L, drop = FALSE) {
  check_drake_slice_args(margin, slices, index)
  args <- list(data)
  dim <- dim(data) %|||% length(data)
  margin <- ifelse(is.null(dim(data)), 1L, margin)
  for (m in seq_along(dim)) {
    if (m == margin) {
      args[[m + 1]] <- slice_indices(dim[m], slices, index)
    } else {
      args[[m + 1]] <- substitute()
    }
  }
  args$drop <- drop
  do.call(`[`, args)
}

check_drake_slice_args <- function(slices, index, margin) {
  sclr <- length(slices) == 1L && length(index) && length(margin) == 1L
  if (sclr) {
    return()
  }
  stop0(
    "In drake_slice(), arguments margin, slices, ",
    "and index must each have length 1."
  )
}

slice_indices <- function(length, slices, index) {
  if (length < 1L || slices < 1L || index < 1L || index > slices) {
    return(integer(0))
  }
  inc <- as.integer(length / slices)
  mod <- length %% slices
  n <- inc + as.integer(index <= mod)
  from <- 1L + inc * (index - 1L) + min(index - 1L, mod)
  seq(from = from, length.out = n)
}

dsl_graph <- function(plan) {
  edges <- lapply(seq_len(nrow(plan)), function(index) {
    dsl_target_edges(plan$transform[[index]], plan$target[[index]])
  })
  edges <- do.call(rbind, edges)
  if (!length(edges) || !nrow(edges)) {
    # Not run but better to keep:
    return(igraph::make_empty_graph()) # nocov
  }
  keep <- !vapply(
    plan$transform,
    safe_is_na,
    FUN.VALUE = logical(1)
  )
  names(keep) <- plan$target
  keep <- names(which(keep, useNames = TRUE))
  edges <- trim_vs_protect_cons(edges, keep)
  graph <- igraph::graph_from_data_frame(edges)
  graph <- igraph::simplify(graph)
  stopifnot(igraph::is_dag(graph))
  transforms <- plan$transform
  names(transforms) <- plan$target
  transforms <- transforms[igraph::V(graph)$name]
  igraph::set_vertex_attr(
    graph,
    name = "transform",
    index = igraph::V(graph)$name,
    value = transforms
  )
}

trim_vs_protect_cons <- function(edges, keep) {
  delete <- unique(setdiff(c(edges$from, edges$to), keep))
  for (v in delete) {
    edges <- delete_v_protect_con(edges, v)
  }
  edges
}

delete_v_protect_con <- function(edges, v) {
  from <- edges$from[edges$to == v]
  to <- edges$to[edges$from == v]
  edges <- edges[edges$from != v & edges$to != v, ]
  if (!length(from) || !length(to)) {
    return(edges)
  }
  nbhd_edges <- expand.grid(from = from, to = to, stringsAsFactors = FALSE)
  rbind(edges, nbhd_edges)
}

dsl_target_edges <- function(transform, target) {
  if (safe_is_na(transform)) {
    return(NULL)
  }
  from <- dsl_deps(transform)
  to <- dsl_revdeps(transform)
  edges <- data.frame(
    from = target, to = target, stringsAsFactors = FALSE
  )
  if (length(from)) {
    edges <- rbind(
      edges,
      data.frame(from = from, to = target, stringsAsFactors = FALSE)
    )
  }
  if (length(to)) {
    edges <- rbind(
      edges,
      data.frame(from = target, to = to, stringsAsFactors = FALSE)
    )
  }
  edges
}

parse_transform <- function(transform, target) {
  if (safe_is_na(transform)) {
    return(NA)
  }
  text <- direct_deparse(
    transform[[1]],
    control = deparse_control_default,
    backtick = TRUE
  )
  transform <- structure(
    as.expression(transform),
    class = unique(c(text, "transform", class(transform)))
  )
  assert_good_transform(transform)
  transform <- structure(
    transform,
    id = dsl_id(transform),
    .names = dsl_names(transform),
    tag_in = tag_in(transform),
    tag_out = tag_out(transform)
  )
  transform <- interpret_transform(transform)
  structure(
    transform,
    deps = dsl_deps(transform),
    revdeps = dsl_revdeps(transform)
  )
}

assert_good_transform <- function(transform, target)
  UseMethod("assert_good_transform")

#' @export
assert_good_transform.map <-
  assert_good_transform.cross <-
  assert_good_transform.combine <- function(transform, target) NULL

#' @export
assert_good_transform.default <- function(transform, target) {
  stop0(
    "invalid transform: ", lang(transform),
    ". Expected: one of map(), cross(), or combine()."
  )
}

interpret_transform <- function(transform) UseMethod("interpret_transform")

#' @export
interpret_transform.map <- function(transform) {
  structure(
    transform,
    new_groupings = new_groupings(transform)
  )
}

#' @export
interpret_transform.cross <- interpret_transform.map

#' @export
interpret_transform.combine <- function(transform) {
  structure(
    transform,
    combine = dsl_combine(transform),
    by = dsl_by(transform)
  )
}

transform_row <- function(index, plan, graph, max_expand) {
  row <- plan[index,, drop = FALSE] # nolint
  target <- row$target
  transform <- set_old_groupings(plan$transform[[index]], plan)
  new_cols <- c(
    target,
    tag_in(transform),
    tag_out(transform),
    group_names(transform)
  )
  check_group_names(new_cols, old_cols(plan))
  out <- dsl_transform(transform, target, row, plan, graph, max_expand)
  out[[target]] <- out$target
  for (col in tag_in(transform)) {
    out[[col]] <- target
  }
  for (col in tag_out(transform)) {
    out[[col]] <- out$target
  }
  out
}

check_group_names <- function(groups, protect) {
  groups <- intersect(groups, protect)
  if (length(groups)) {
    stop0(
      "grouping variables cannot also be custom column names in the plan:\n",
      multiline_message(groups)
    )
  }
}

dsl_transform <- function(
  transform,
  target,
  row,
  plan,
  graph,
  max_expand,
  ...
) {
  UseMethod("dsl_transform")
}

#' @export
dsl_transform.map <- dsl_transform.cross <- function(
  transform,
  target,
  row,
  plan,
  graph,
  max_expand,
  ...
) {
  groupings <- groupings(transform)
  grid <- dsl_grid(transform, groupings)
  if (any(dim(grid) < 1L)) {
    error_nonempty_transform(target)
  }
  grid <- dsl_map_join_plan(
    grid = grid,
    plan = plan,
    graph = graph,
    target = target,
    transform = transform
  )
  dsl_map_new_targets(
    transform = transform,
    target = target,
    row = row,
    grid = grid,
    plan = plan,
    max_expand = max_expand
  )
}

dsl_map_join_plan <- function(grid, plan, graph, target, transform) {
  cols <- upstream_trace_vars(target, plan, graph)
  gridlist <- lapply(grid, as.data.frame, stringsAsFactors = FALSE)
  for (i in seq_len(ncol(grid))) {
    colnames(gridlist[[i]]) <- colnames(grid)[i]
    gridlist[[i]] <- dsl_left_outer_join(
      gridlist[[i]],
      plan[, cols, drop = FALSE]
    )
  }
  grid <- do.call(cbind, unname(gridlist))
  grid_cols <- c(names(groupings), setdiff(colnames(grid), names(groupings)))
  grid[, grid_cols, drop = FALSE]
}

dsl_map_new_targets <- function(
  transform,
  target,
  row,
  grid,
  plan,
  max_expand
) {
  sub_cols <- intersect(colnames(grid), group_names(transform))
  new_target_names <- new_target_names(
    target,
    grid,
    cols = sub_cols,
    id = dsl_id(transform),
    names = dsl_names(transform)
  )
  if (length(new_target_names) != nrow(grid)) {
    stop0("target names must be the same length as the number of new targets.")
  }
  out <- data.frame(target = new_target_names, stringsAsFactors = FALSE)
  grid$.id_chr <- sprintf("\"%s\"", new_target_names)
  for (col in setdiff(old_cols(plan), c("target", "transform"))) {
    if (is.language(row[[col]][[1]])) {
      out[[col]] <- grid_subs(row[[col]][[1]], grid)
    } else {
      out[[col]] <- row[[col]][[1]]
    }
  }
  grid$.id_chr <- NULL
  out <- cbind(out, grid)
  df_max_expand(out, max_expand)
}

group_names <- function(transform) {
  as.character(names(groupings(transform)))
}

dsl_left_outer_join <- function(x, y) {
  by <- intersect(colnames(x), colnames(y))
  if (!length(by)) {
    return(x)
  }
  # The output must have the same number of rows as x,
  # and the ID variables must all be nonmissing.
  rows_keep <- complete_cases(y[, by, drop = FALSE])
  y <- y[rows_keep,, drop = FALSE] # nolint
  # Drop the columns that only partially tag along.
  # These grouping variables never truly belonged to the
  # ID variables in x.
  cols_keep <- !vapply(y, anyNA, FUN.VALUE = logical(1))
  y <- y[, cols_keep, drop = FALSE]
  # Just a precaution. We should actually be okay by now.
  y <- y[!duplicated(y[, by, drop = FALSE]),, drop = FALSE] # nolint
  # Need to recover the original row order
  key <- basename(tempfile(fileext = "drake_transform_plan_col"))
  x[[key]] <- seq_len(nrow(x))
  out <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
  out <- out[, !duplicated(colnames(out)), drop = FALSE]
  is_na_col <- vlapply(out, function(x) {
    all(is.na(x))
  })
  out <- out[, !is_na_col, drop = FALSE]
  out <- out[order(out[[key]]),, drop = FALSE] # nolint
  out[[key]] <- NULL
  out
}

upstream_trace_vars <- function(target, plan, graph) {
  targets <- igraph::subcomponent(graph, v = target, mode = "in")$name
  targets <- setdiff(targets, target)
  if (!length(targets)) {
    return(character(0))
  }
  transforms <- igraph::vertex_attr(graph, "transform", index = targets)
  revdeps <- unique(unlist(lapply(transforms, function(x) dsl_revdeps(x))))
  out <- c(targets, revdeps)
  intersect(out, colnames(plan))
}

df_max_expand <- function(df, max_expand) {
  if (is.null(max_expand)) {
    return(df)
  }
  rows <- seq_max_expand(nrow(df), max_expand)
  df[rows,, drop = FALSE] # nolint
}

seq_max_expand <- function(n, max_expand) {
  max_expand <- min(n, max_expand)
  i <- seq(from = 1L, to = n, length.out = max_expand)
  i <- floor(i)
  unique(i)
}

#' @export
dsl_transform.combine <- function(transform, target, row, plan, graph, ...) {
  plan <- valid_splitting_plan(plan, transform)
  if (!nrow(plan)) {
    error_nonempty_transform(target)
  }
  out <- dsl_commands_combine(transform = transform, row = row, plan = plan)
  if (!nrow(out)) {
    error_nonempty_transform(target)
  }
  out$target <- new_target_names(
    target,
    out,
    cols = dsl_by(transform),
    id = dsl_id(transform),
    names = dsl_names(transform)
  )
  out <- id_chr_sub(plan = out, cols = old_cols(plan), .id_chr = out$target)
  out
}

dsl_commands_combine <- function(transform, row, plan) {
  map_by(
    .x = plan,
    .by = dsl_by(transform),
    .f = combine_step,
    row = row,
    transform = transform,
    old_cols = old_cols(plan)
  )
}

error_nonempty_transform <- function(target) {
  stop0(
    "A grouping variable for target ", target,
    " is either undefined or improperly invoked. Details: ",
    "https://books.ropensci.org/drake/static.html#grouping-variables"
  )
}

valid_splitting_plan <- function(plan, transform) {
  cols <- dsl_by(transform)
  if (!length(cols)) {
    return(plan)
  }
  rows_keep <- complete_cases(plan[, cols, drop = FALSE])
  old_cols <- old_cols(plan)
  out <- plan[rows_keep,, drop = FALSE] # nolint
  old_cols(out) <- old_cols
  out
}

map_by <- function(.x, .by, .f, ...) {
  splits <- split_by(.x, .by = .by)
  out <- lapply(
    X = splits,
    FUN = function(split) {
      out <- .f(split, ...)
      if (nrow(out)) {
        out[, .by] <- split[replicate(nrow(out), 1), .by]
      }
      out
    }
  )
  do.call(what = drake_bind_rows, args = out)
}

split_by <- function(.x, .by = character(0)) {
  if (!length(.by)) {
    return(list(.x))
  }
  fact <- lapply(.x[, .by, drop = FALSE], factor, exclude = c())
  splits <- base::split(x = .x, f = fact)
  Filter(x = splits, f = nrow)
}

id_chr_sub <- function(plan, cols, .id_chr) {
  grid <- data.frame(.id_chr = .id_chr, stringsAsFactors = FALSE)
  for (col in setdiff(cols, c("target", "transform"))) {
    if (is.language(plan[[col]][[1]])) {
      plan[[col]] <- lapply(
        seq_len(nrow(plan)),
        function(index) {
          grid_sub(index, expr = plan[[col]][[index]], grid = grid)
        }
      )
    }
  }
  plan
}

grid_subs <- function(expr, grid) {
  keep <- intersect(all.vars(expr, functions = TRUE), colnames(grid))
  grid <- grid[, keep, drop = FALSE]
  for (i in seq_along(grid)) {
    grid[[i]] <- dsl_syms(grid[[i]])
  }
  lapply(
    seq_len(nrow(grid)),
    grid_sub,
    expr = expr,
    grid = grid
  )
}

grid_sub <- function(index, expr, grid) {
  sub <- lapply(grid, `[[`, index)
  eval(call("substitute", expr, sub), envir = baseenv())
}

dsl_syms <- function(x) {
  out <- lapply(as.character(x), dsl_sym)
}

dsl_sym <- function(x) {
  tryCatch(parse(text = x)[[1]], error = function(e) as.symbol(x))
}

new_target_names <- function(target, grid, cols, id, names) {
  if (!is.null(names)) {
    return(names)
  }
  if (is.character(id)) {
    cols <- intersect(id, colnames(grid))
  }
  grid <- grid[, cols, drop = FALSE]
  if (identical(id, FALSE) || any(dim(grid) < 1L)) {
    out <- rep(target, nrow(grid))
    return(make_unique(make.names(out, unique = FALSE, allow_ = TRUE)))
  }
  suffixes <- apply(grid, 1, paste, collapse = "_")
  suffixes <- gsub("\"", "", suffixes, fixed = TRUE)
  out <- paste0(target, "_", suffixes)
  make_unique(make.names(out, unique = FALSE, allow_ = TRUE))
}

make_unique <- function(x) {
  if (!length(x)) {
    return(character(0))
  }
  ord <- order(x)
  y <- x[ord]
  dup <- duplicated(y)
  if (!any(dup)) {
    return(x)
  }
  suffix <- as.integer(
    do.call(c, tapply(dup, y, FUN = cumsum, simplify = FALSE))
  )
  i <- suffix > 0L
  suffix <- suffix + i
  y[i] <- paste(y[i], suffix[i], sep = "_")
  y[order(ord)]
}

combine_step <- function(plan, row, transform, old_cols) {
  args <- args_combine(plan, transform)
  any_empty <- any(!vapply(args, length, FUN.VALUE = integer(1)))
  if (!length(args) || any_empty) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  out <- data.frame(command = NA, stringsAsFactors = FALSE)
  for (col in setdiff(old_cols, c("target", "transform"))) {
    if (is.language(row[[col]][[1]])) {
      out[[col]] <- list(splice_args(row[[col]][[1]], args))
    } else {
      out[[col]] <- row[[col]]
    }
  }
  groupings <- dsl_combine_join_plan(plan, transform, old_cols)
  if (nrow(groupings) == 1L) {
    out <- cbind(out, groupings)
  }
  out
}

old_cols <- function(plan) {
  attr(plan, "old_cols")
}

`old_cols<-` <-  function(plan, value) {
  attr(plan, "old_cols") <- value
  plan
}

args_combine <- function(plan, transform) {
  out <- lapply(
    dsl_combine(transform),
    args_combine_entry,
    plan = plan,
    transform = transform
  )
  names(out) <- dsl_combine(transform)
  out
}

args_combine_entry <- function(name, transform, plan) {
  lapply(as.character(na_omit(unique(plan[[name]]))), dsl_sym)
}

splice_args <- function(x, replacements) {
  out <- splice_inner(x, replacements)
  # Avoid edge cases like #715
  out <- parse(text = safe_deparse(out, backtick = TRUE))
  if (length(out)) {
    out <- out[[1]]
  }
  out
}

# From https://stackoverflow.com/a/54623901/3704549
splice_inner <- function(x, replacements) {
  if (is.call(x)) {
    splice_call(x, replacements)
  } else if (is.name(x)) {
    splice_name(x, replacements)
  } else {
    list(x)
  }
}

splice_call <- function(x, replacements) {
  args <- lapply(as.list(x), splice_inner, replacements = replacements)
  use_names <- args$use.names
  recursive <- args$recursive
  args <- do.call("c", args, quote = TRUE)
  args$use.names <- unlist(use_names)
  args$recursive <- unlist(recursive)
  as.call(args)
}

splice_name <- function(x, replacements) {
  nm <- direct_deparse(
    x,
    control = deparse_control_default,
    backtick = FALSE
  )
  if (nm %in% names(replacements)) {
    replacements[[nm]]
  } else {
    list(x)
  }
}

dsl_combine_join_plan <- function(plan, transform, old_cols) {
  combined_plan <- plan[, dsl_combine(transform), drop = FALSE]
  out <- plan[complete_cases(combined_plan),, drop = FALSE] # nolint
  drop <- c(old_cols, dsl_combine(transform), dsl_by(transform))
  keep <- setdiff(colnames(out), drop)
  out <- out[, keep, drop = FALSE]
  keep <- !vapply(out, anyNA, FUN.VALUE = logical(1))
  out <- out[, keep, drop = FALSE]
  keep <- viapply(out, function(x) {
    length(unique(x))
  }) == 1L
  utils::head(out[, keep, drop = FALSE], n = 1)
}

dsl_deps <- function(transform) UseMethod("dsl_deps")

#' @export
dsl_deps.map <- function(transform) {
  attr(transform, "deps") %|||% c(
    as.character(unnamed(as.list(transform[[1]][-1]))),
    unname(unlist(new_groupings(transform)))
  )
}

#' @export
dsl_deps.cross <- dsl_deps.map

#' @export
dsl_deps.combine <- function(transform) {
  attr(transform, "deps") %|||% c(
    dsl_combine(transform),
    dsl_by(transform)
  )
}

dsl_revdeps <- function(transform) UseMethod("dsl_revdeps")

#' @export
dsl_revdeps.map <- function(transform) {
  attr(transform, "revdeps") %|||% c(
    names(new_groupings(transform)),
    tag_in(transform),
    tag_out(transform)
  )
}

#' @export
dsl_revdeps.cross <- dsl_revdeps.map

#' @export
dsl_revdeps.combine <- function(transform) {
  attr(transform, "revdeps") %|||% c(
    tag_in(transform),
    tag_out(transform)
  )
}

dsl_grid <- function(transform, groupings) UseMethod("dsl_grid")

#' @export
dsl_grid.map <- function(transform, groupings) {
  tryCatch(
    as.data.frame(groupings, stringsAsFactors = FALSE),
    error = function(e) {
      map_grid_error(transform, groupings)
    }
  )
}

map_grid_error <- function(transform, groupings) {
  stop0(
    "Failed to make a grid of grouping variables for map().\n",
    "Grouping variables in map() must have suitable lengths ",
    "for coercion to a data frame.\n",
    "Possibly uneven groupings detected in ", char(transform), ":\n",
    multiline_message(groupings)
  )
}

#' @export
dsl_grid.cross <- function(transform, groupings) {
  do.call(expand.grid, c(groupings, stringsAsFactors = FALSE))
}

groupings <- function(transform) {
  UseMethod("groupings")
}

#' @export
groupings.map <- groupings.cross <- function(transform) {
  c(new_groupings(transform), old_groupings(transform))
}

#' @export
groupings.combine <- function(transform) character(0)

old_groupings <- function(transform, plan = NULL) UseMethod("old_groupings")

#' @export
old_groupings.map <- old_groupings.cross <- function(transform, plan = NULL) {
  attr(transform, "old_groupings") %|||%
    find_old_groupings(transform, plan)
}

set_old_groupings <- function(transform, plan) {
  attr(transform, "old_groupings") <- find_old_groupings(transform, plan)
  transform
}

find_old_groupings <- function(transform, plan) UseMethod("find_old_groupings")

#' @export
find_old_groupings.map <- function(transform, plan) {
  group_names <- as.character(unnamed(lang(transform))[-1])
  group_names <- intersect(group_names, names(plan))
  subplan <- plan[, group_names, drop = FALSE]
  keep <- apply(subplan, 1, function(x) {
    !all(is.na(x))
  })
  subplan <- subplan[keep,, drop = FALSE] # nolint
  if (any(dim(subplan) < 1L)) {
    return(list())
  }
  # Look for blocks of nested grouping variables.
  blocks <- column_components(subplan)
  blocks <- lapply(blocks, function(x) {
    as.list(x[complete_cases(x),, drop = FALSE]) # nolint
  })
  names(blocks) <- NULL
  out <- do.call(c, blocks)
  out <- select_nonempty(lapply(out, na_omit))
  min_length <- min(vapply(out, length, FUN.VALUE = integer(1)))
  out <- as.data.frame(
    lapply(out, head, n = min_length),
    stringsAsFactors = FALSE
  )
  as.list(out[!duplicated(out),, drop = FALSE]) # nolint
}

column_components <- function(x) {
  adj <- crossprod(!is.na(x)) > 0L
  graph <- igraph::graph_from_adjacency_matrix(adj)
  membership <- sort(igraph::components(graph)$membership)
  tapply(
    X = names(membership),
    INDEX = membership,
    function(cols) {
      x[, cols, drop = FALSE]
    },
    simplify = FALSE
  )
}

#' @export
find_old_groupings.cross <- function(transform, plan) {
  group_names <- as.character(unnamed(lang(transform))[-1])
  group_names <- intersect(group_names, names(plan))
  lapply(plan[, group_names, drop = FALSE], function(x) {
    unique(na_omit(x))
  })
}

na_omit <- function(x) {
  x[!is.na(x)]
}

#' @export
find_old_groupings.combine <- function(transform, plan) NULL

new_groupings <- function(transform) UseMethod("new_groupings")

#' @export
new_groupings.map <- function(transform) {
  attr <- attr(transform, "new_groupings")
  if (!is.null(attr)) {
    return(attr)
  }
  transform <- lang(transform)
  explicit <- explicit_new_groupings(
    transform,
    exclude = c(".data", dsl_all_special)
  )
  data_arg <- data_arg_groupings(transform[[".data"]])
  c(explicit, data_arg)
}

data_arg_groupings <- function(data_arg) {
  if (is.null(data_arg)) {
    return(list())
  }
  lapply(data_arg, function(x) {
    x <- factor_to_character(x)
    vapply(x, long_deparse, FUN.VALUE = character(1))
  })
}

dsl_all_special <- c(".id", ".names", ".tag_in", ".tag_out")

#' @export
new_groupings.cross <- new_groupings.map

explicit_new_groupings <- function(code, exclude = character(0)) {
  list <- named(as.list(code), exclude)
  lapply(list, function(x) {
    if (is.call(x)) {
      x <- x[-1]
    }
    as.character(lapply(as.list(x), long_deparse))
  })
}

long_deparse <- function(x, collapse = "\n") {
  out <- direct_deparse(x, control = deparse_control_default, backtick = TRUE)
  if (length(out) > 1L) {
    out <- paste(out, collapse = collapse)
  }
  out
}

dsl_combine <- function(transform) UseMethod("dsl_combine")

#' @export
dsl_combine.combine <- function(transform) {
  attr(transform, "combine") %|||%
    as.character(unnamed(lang(transform))[-1])
}

dsl_by <- function(transform) UseMethod("dsl_by")

#' @export
dsl_by.combine <- function(transform) {
  attr(transform, "by") %|||%
    all.vars(lang(transform)[[".by"]], functions = FALSE)
}

dsl_id <- function(transform) UseMethod("dsl_id")

#' @export
dsl_id.transform <- function(transform) {
  if (!is.null(attr(transform, "id"))) {
    return(attr(transform, "id"))
  }
  out <- lang(transform)[[".id"]]
  if (all(is.logical(out))) {
    return(out)
  }
  all.vars(out, functions = FALSE) %||% TRUE
}

dsl_names <- function(transform) UseMethod("dsl_names")

#' @export
dsl_names.transform <- function(transform) {
  if (!is.null(attr(transform, ".names"))) {
    return(attr(transform, ".names"))
  }
  eval(lang(transform)[[".names"]])
}

tag_in <- function(transform) UseMethod("tag_in")

#' @export
tag_in.transform <- function(transform) {
  attr(transform, "tag_in") %|||%
    all.vars(lang(transform)[[".tag_in"]], functions = FALSE)
}

tag_out <- function(transform) UseMethod("tag_out")

#' @export
tag_out.transform <- function(transform) {
  attr(transform, "tag_out") %|||%
    all.vars(lang(transform)[[".tag_out"]], functions = FALSE)
}

lang <- function(x) UseMethod("lang")

#' @export
lang.command <- lang.transform <- function(x) x[[1]]

char <- function(x) UseMethod("char")

#' @export
char.transform <- function(x) safe_deparse(lang(x), backtick = TRUE)

named <- function(x, exclude = character(0)) {
  if (is.null(names(x))) return(NULL)
  x[!(names(x) %in% c("", exclude))]
}

unnamed <- function(x) {
  if (is.null(names(x))) return(x)
  x[!nzchar(names(x))]
}
