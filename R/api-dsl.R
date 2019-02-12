transform_plan <- function(plan, envir, trace = FALSE) {
  if (!("transform" %in% names(plan))) {
    return(plan)
  }
  old_cols(plan) <- old_cols <- colnames(plan)
  plan[["transform"]] <- tidyeval_exprs(plan[["transform"]], envir = envir)
  plan[["transform"]] <- lapply(plan[["transform"]], parse_transform)
  graph <- dsl_graph(plan)
  while (gorder(graph)) {
    targets <- leaf_nodes(graph)
    graph <- igraph::delete_vertices(graph, v = targets)
    index <- which(plan$target %in% targets)
    rows <- lapply(index, transform_row, plan = plan)
    plan <- sub_in_plan(plan, rows, at = index)
    old_cols(plan) <- old_cols
  }
  if (!trace) {
    keep <- as.character(intersect(colnames(plan), old_cols(plan)))
    plan <- plan[, intersect(colnames(plan), old_cols(plan)), drop = FALSE]
  }
  old_cols(plan) <- plan[["transform"]] <- NULL
  plan
}

dsl_graph <- function(plan) {
  edges <- lapply(seq_len(nrow(plan)), function(index) {
    dsl_target_edges(plan[["transform"]][[index]], plan[["target"]][[index]])
  })
  edges <- do.call(rbind, edges)
  if (!length(edges) || !nrow(edges)) {
    # Not run but better to keep:
    return(igraph::make_empty_graph()) # nocov
  }
  keep <- !vapply(
    plan[["transform"]],
    safe_is_na,
    FUN.VALUE = logical(1)
  )
  names(keep) <- plan$target
  keep <- names(which(keep, useNames = TRUE))
  edges <- trim_vs_protect_cons(edges, keep)
  graph <- igraph::graph_from_data_frame(edges)
  graph <- igraph::simplify(graph)
  stopifnot(igraph::is_dag(graph))
  graph
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

transform_row <- function(plan, index) {
  row <- plan[index,, drop = FALSE] # nolint
  target <- row$target
  transform <- set_old_groupings(plan[["transform"]][[index]], plan)
  new_cols <- c(
    target,
    tag_in(transform),
    tag_out(transform),
    group_names(transform)
  )
  check_group_names(new_cols, old_cols(plan))
  out <- dsl_transform(transform, target, row, plan)
  if (is.null(out)) {
    return()
  }
  out[[target]] <- out$target
  for (col in tag_in(transform)) {
    out[[col]] <- target
  }
  for (col in tag_out(transform)) {
    out[[col]] <- out$target
  }
  out
}

map_to_grid <- function(transform, target, row, plan) {
  groupings <- groupings(transform)
  grid <- dsl_grid(transform, groupings)
  if (any(dim(grid) < 1L)) {
    warn_empty_transform(target)
    return()
  }
  ncl <- c(names(new_groupings(transform)), old_cols(plan))
  old_cols <- old_cols(plan)
  plan <- plan[, setdiff(colnames(plan), ncl), drop = FALSE]
  grid <- dsl_left_outer_join(grid, plan)
  sub_cols <- intersect(colnames(grid), group_names(transform))
  sub_grid <- grid[, sub_cols, drop = FALSE]
  new_targets <- new_targets(target, sub_grid, dsl_id(transform))
  out <- data.frame(target = new_targets, stringsAsFactors = FALSE)
  for (col in setdiff(old_cols, c("target", "transform"))) {
    if (is.language(row[[col]][[1]])) {
      out[[col]] <- grid_subs(row[[col]][[1]], grid)
    } else {
      out[[col]] <- row[[col]][[1]]
    }
  }
  cbind(out, grid)
}

dsl_grid <- function(...) UseMethod("dsl_grid")

dsl_grid.cross <- function(transform, groupings) {
  do.call(expand.grid, c(groupings, stringsAsFactors = FALSE))
}

dsl_grid.map <- function(transform, groupings) {
  tryCatch(
    as.data.frame(groupings, stringsAsFactors = FALSE),
    error = function(e) {
      map_grid_error(transform, groupings)
    }
  )
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

new_targets <- function(target, grid, id) {
  if (is.null(dim(grid)) || any(dim(grid) < 1L)) {
    return(target)
  }
  if (is.character(id)) {
    cols <- intersect(id, colnames(grid))
    grid <- grid[, cols, drop = FALSE]
  }
  if (identical(id, FALSE) || any(dim(grid) < 1L)) {
    out <- rep(target, nrow(grid))
    return(make.names(out, unique = TRUE))
  }
  suffixes <- apply(grid, 1, paste, collapse = "_")
  out <- paste0(target, "_", suffixes)
  make.names(out, unique = TRUE)
}

dsl_transform <- function(...) {
  UseMethod("dsl_transform")
}

dsl_transform.cross <- dsl_transform.map <- map_to_grid

dsl_transform.combine <- function(transform, target, row, plan) {
  plan <- valid_splitting_plan(plan, transform)
  if (!nrow(plan)) {
    warn_empty_transform(target)
    return()
  }
  out <- map_by(
    .x = plan,
    .by = dsl_by(transform),
    .f = combine_step,
    row = row,
    transform = transform,
    old_cols = old_cols(plan)
  )
  if (!nrow(out)) {
    warn_empty_transform(target)
    return()
  }
  out$target <- new_targets(
    target, out[, dsl_by(transform), drop = FALSE], dsl_id(transform)
  )
  out
}

valid_splitting_plan <- function(plan, transform) {
  cols <- dsl_by(transform)
  if (!length(cols)) {
    return(plan)
  }
  rows_keep <- complete_cases(plan[, cols, drop = FALSE])
  plan[rows_keep,, drop = FALSE] # nolint
}

combine_step <- function(plan, row, transform, old_cols) {
  args <- args_combine(plan, transform)
  any_empty <- any(!vapply(args, length, FUN.VALUE = integer(1)))
  if (!length(args) || any_empty) {
    return(data.frame())
  }
  out <- data.frame(command = NA, stringsAsFactors = FALSE)
  for (col in setdiff(old_cols, c("target", "transform"))) {
    if (is.language(row[[col]][[1]])) {
      out[[col]] <- list(splice_args(row[[col]][[1]], args))
    } else {
      out[[col]] <- row[[col]]
    }
  }
  out
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
  lapply(as.character(na_omit(unique(plan[[name]]))), as.symbol)
}

lang <- function(...) UseMethod("lang")

lang.command <- lang.transform <- function(x) x[[1]]

char <- function(...) UseMethod("char")

char.transform <- function(x) safe_deparse(lang(x))

old_cols <- function(plan) {
  attr(plan, "old_cols")
}

`old_cols<-` <-  function(plan, value) {
  attr(plan, "old_cols") <- value
  plan
}

parse_transform <- function(transform, target) {
  if (safe_is_na(transform)) {
    return(NA)
  }
  transform <- structure(
    as.expression(transform),
    class = unique(c(deparse(transform[[1]]), "transform", class(transform)))
  )
  assert_good_transform(transform)
  transform <- structure(
    transform,
    id = dsl_id(transform),
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

interpret_transform <- function(...) UseMethod("interpret_transform")

interpret_transform.map <- function(transform) {
  structure(
    transform,
    new_groupings = new_groupings(transform)
  )
}

interpret_transform.cross <- interpret_transform.map

interpret_transform.combine <- function(transform, ...) {
  structure(
    transform,
    combine = dsl_combine(transform),
    by = dsl_by(transform)
  )
}

assert_good_transform <- function(...) UseMethod("assert_good_transform")

assert_good_transform.map <-
  assert_good_transform.cross <-
  assert_good_transform.combine <- function(...) NULL

assert_good_transform.default <- function(transform, target) {
  stop(
    "invalid transform: ", lang(transform),
    ". Expected: one of map(), cross(), or combine()",
    call. = FALSE
  )
}

dsl_deps <- function(transform) UseMethod("dsl_deps")

dsl_deps.map <- function(transform) {
  attr(transform, "deps") %|||% c(
    as.character(unnamed(as.list(transform[[1]][-1]))),
    unname(unlist(new_groupings(transform)))
  )
}

dsl_deps.cross <- dsl_deps.map

dsl_deps.combine <- function(transform) {
  attr(transform, "deps") %|||% c(
    dsl_combine(transform),
    dsl_by(transform)
  )
}

dsl_revdeps <- function(transform) UseMethod("dsl_revdeps")

dsl_revdeps.map <- function(transform) {
  attr(transform, "revdeps") %|||% c(
    names(new_groupings(transform)),
    tag_in(transform),
    tag_out(transform)
  )
}
  
dsl_revdeps.cross <- dsl_revdeps.map

dsl_revdeps.combine <- function(transform) {
  attr(transform, "revdeps") %|||% c(
    tag_in(transform),
    tag_out(transform)
  )
}

dsl_by <- function(...) UseMethod("dsl_by")

dsl_by.combine <- function(transform) {
  attr(transform, "by") %|||%
    all.vars(lang(transform)[[".by"]], functions = FALSE)
}

dsl_combine <- function(...) UseMethod("dsl_combine")

dsl_combine.combine <- function(transform) {
  attr(transform, "combine") %|||%
    as.character(unnamed(lang(transform))[-1])
}

new_groupings <- function(...) UseMethod("new_groupings")

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
  data_arg <- transform[[".data"]]
  if (is.null(data_arg)) {
    return(explicit)
  }
  data_arg <- lapply(data_arg, function(x){
    vapply(x, safe_deparse, FUN.VALUE = character(1))
  })
  c(explicit, data_arg)
}

new_groupings.cross <- function(transform) {
  attr(transform, "new_groupings") %|||%
    explicit_new_groupings(
      lang(transform),
      exclude = dsl_all_special
    )
}

explicit_new_groupings <- function(code, exclude = character(0)) {
  list <- named(as.list(code), exclude)
  lapply(list, function(x) {
    if (is.call(x)) {
      x <- x[-1]
    }
    as.character(lapply(as.list(x), deparse))
  })
}

old_groupings <- function(...) UseMethod("old_groupings")

old_groupings.map <- old_groupings.cross <- function(transform, plan = NULL) {
  attr(transform, "old_groupings") %|||%
    find_old_groupings(transform, plan)
}

find_old_groupings <- function(...) UseMethod("find_old_groupings")

find_old_groupings.map <- function(transform, plan) {
  group_names <- as.character(unnamed(lang(transform))[-1])
  group_names <- intersect(group_names, names(plan))
  subplan <- plan[, group_names, drop = FALSE]
  if (any(dim(subplan) < 1L)) {
    return(list())
  }
  out <- select_nonempty(lapply(subplan, na_omit))
  min_length <- min(vapply(out, length, FUN.VALUE = integer(1)))
  out <- as.data.frame(
    lapply(out, head, n = min_length),
    stringsAsFactors = FALSE
  )
  as.list(out[!duplicated(out),, drop = FALSE]) # nolint
}

find_old_groupings.cross <- function(transform, plan) {
  group_names <- as.character(unnamed(lang(transform))[-1])
  group_names <- intersect(group_names, names(plan))
  lapply(plan[, group_names, drop = FALSE], function(x) {
    unique(na_omit(x))
  })
}

find_old_groupings.combine <- function(transform, plan) NULL

set_old_groupings <- function(transform, plan) {
  attr(transform, "old_groupings") <- find_old_groupings(transform, plan)
  transform
}

groupings <- function(...) {
  UseMethod("groupings")
}

groupings.map <- groupings.cross <- function(transform) {
  c(new_groupings(transform), old_groupings(transform))
}

groupings.combine <- function(...) character(0)

group_names <- function(transform) {
  as.character(names(groupings(transform)))
}

dsl_id <- function(...) UseMethod("dsl_id")

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

tag_in <- function(...) UseMethod("tag_in")

tag_in.transform <- function(transform) {
  attr(transform, "tag_in") %||%
    all.vars(lang(transform)[[".tag_in"]], functions = FALSE)
}

tag_out <- function(...) UseMethod("tag_out")

tag_out.transform <- function(transform) {
  attr(transform, "tag_out") %||%
    all.vars(lang(transform)[[".tag_out"]], functions = FALSE)
}

dsl_syms <- function(x) {
  out <- lapply(as.character(x), dsl_sym)
}

dsl_sym <- function(x) {
  tryCatch(
    eval(parse(text = x), envir = emptyenv()),
    error = function(e) as.symbol(x)
  )
}

dsl_left_outer_join <- function(x, y) {
  by <- intersect(colnames(x), colnames(y))
  if (!length(by)) {
    return(x)
  }
  # The output must have the same number of rows as x.
  rows_keep <- complete_cases(y[, by, drop = FALSE])
  y <- y[rows_keep,, drop = FALSE] # nolint
  # Just a precaution. We should actually be okay by now.
  y <- y[!duplicated(y[, by, drop = FALSE]),, drop = FALSE] # nolint
  # Is merge() a performance bottleneck?
  # Need to profile.
  out <- merge(x = x, y = y, by = by, all.x = TRUE)
  out[, union(colnames(x), colnames(y)), drop = FALSE]
}

map_grid_error <- function(transform, groupings) {
  stop(
    "Failed to make a grid of grouping variables for map().\n",
    "Grouping variables in map() must have suitable lengths ",
    "for coercion to a data frame.\n",
    "Possibly uneven groupings detected in ", char(transform), ":\n",
    multiline_message(groupings),
    call. = FALSE
  )
}

check_group_names <- function(groups, protect) {
  groups <- intersect(groups, protect)
  if (length(groups)) {
    stop(
      "variables in `target(transform = ...)` ",
      "cannot also be custom column names in the plan:\n",
      multiline_message(groups),
      call. = FALSE
    )
  }
}

warn_empty_transform <- function(target) {
  warning(
    "A grouping or splitting variable for target ", shQuote(target),
    " is missing or undefined. Transformation skipped ",
    "and target deleted.",
    call. = FALSE
  )
}

dsl_all_special <- c(".id", ".tag_in", ".tag_out")
