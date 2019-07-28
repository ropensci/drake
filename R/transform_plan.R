#' @title Transform a plan
#' @description Evaluate the `map()`, `cross()`, `split()` and
#'   `combine()` operations in the `transform` column of a
#'   `drake` plan.
#' @details <https://ropenscilabs.github.io/drake-manual/plans.html#large-plans> # nolint
#' @export
#' @param plan A `drake` plan with a `transform` column
#' @param envir Environment for tidy evaluation.
#' @param trace Logical, whether to add columns to show
#'   what happens during target transformations.
#' @param max_expand Positive integer, optional upper bound on the lengths
#'   of grouping variables for `map()` and `cross()`. Comes in handy
#'   when you have a massive number of targets and you want to test
#'   on a miniature version of your workflow before you scale up
#'   to production.
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
  for (target in order) {
    index <- which(target == plan$target)
    rows <- transform_row(index, plan, graph, max_expand)
    plan <- sub_in_plan(plan, rows, index)
    old_cols(plan) <- old_cols
  }
  if (!trace) {
    keep <- as.character(intersect(colnames(plan), old_cols(plan)))
    plan <- plan[, intersect(colnames(plan), old_cols(plan)), drop = FALSE]
  }
  old_cols(plan) <- plan$transform <- NULL
  if (tidy_eval) {
    for (col in setdiff(colnames(plan), c("target", "transform"))) {
      plan[[col]] <- tidyeval_exprs(plan[[col]], envir = envir)
    }
  }
  if (sanitize) {
    plan <- sanitize_plan(plan, envir = envir)
  }
  plan
}

sub_in_plan <- function(plan, rows, index) {
  plan <- drake_bind_rows(
    plan[seq_len(index - 1), ],
    rows,
    plan[-seq_len(index), ]
  )
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
  slice <- match.call(drake_slice, slice)
  sub <- list(slice = slice)
  names(sub) <- safe_deparse(slice$data)
  command <- eval(call("substitute", command, sub), envir = baseenv())
  transform <- as.call(c(quote(map), slice$data))
  transform[[index]] <- as.numeric(seq_len(slice$slices))
  list(command = command, transform = transform)
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

dsl_transform <- function(...) {
  UseMethod("dsl_transform")
}

dsl_transform.map <- dsl_transform.cross <- function(
  transform,
  target,
  row,
  plan,
  graph,
  max_expand
) {
  groupings <- groupings(transform)
  groupings <- thin_groupings(groupings, max_expand)
  grid <- dsl_grid(transform, groupings)
  if (any(dim(grid) < 1L)) {
    warn_empty_transform(target)
    return()
  }
  cols <- upstream_trace_vars(target, plan, graph)
  grid <- dsl_left_outer_join(grid, plan[, cols, drop = FALSE])
  sub_cols <- intersect(colnames(grid), group_names(transform))
  new_targets <- new_targets(
    target, grid, cols = sub_cols, id = dsl_id(transform)
  )
  out <- data.frame(target = new_targets, stringsAsFactors = FALSE)
  grid$.id_chr <- sprintf("\"%s\"", new_targets)
  for (col in setdiff(old_cols(plan), c("target", "transform"))) {
    if (is.language(row[[col]][[1]])) {
      out[[col]] <- grid_subs(row[[col]][[1]], grid)
    } else {
      out[[col]] <- row[[col]][[1]]
    }
  }
  grid$.id_chr <- NULL
  cbind(out, grid)
}

group_names <- function(transform) {
  as.character(names(groupings(transform)))
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
  out <- merge(x = x, y = y, by = by, all.x = TRUE, sort = FALSE)
  out[, union(colnames(x), colnames(y)), drop = FALSE]
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

thin_groupings <- function(groupings, max_expand) {
  if (is.null(max_expand)) {
    return(groupings)
  }
  lapply(groupings, thin_grouping, max_expand = max_expand)
}

thin_grouping <- function(x, max_expand) {
  len <- min(length(x), max_expand)
  i <- seq(from = 1L, to = length(x), length.out = len)
  i <- floor(i)
  i <- unique(i)
  x[i]
}

dsl_transform.combine <- function(transform, target, row, plan, graph, ...) {
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
    target, out, cols = dsl_by(transform), id = dsl_id(transform)
  )
  out <- id_chr_sub(plan = out, cols = old_cols(plan), .id_chr = out$target)
  out
}

warn_empty_transform <- function(target) {
  warning(
    "A grouping or splitting variable for target ", shQuote(target),
    " is missing or undefined. Transformation skipped ",
    "and target deleted.",
    call. = FALSE
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

complete_cases <- function(x) {
  !as.logical(Reduce(`|`, lapply(x, is.na)))
}

map_by <- function(.x, .by, .f, ...) {
  splits <- split_by(.x, .by = .by)
  out <- lapply(
    X = splits,
    FUN = function(split){
      out <- .f(split, ...)
      if (nrow(out)) {
        out[, .by] <- split[replicate(nrow(out), 1), .by]
      }
      out
    }
  )
  do.call(what = rbind, args = out)
}

split_by <- function(.x, .by = character(0)) {
  if (!length(.by)) {
    return(list(.x))
  }
  fact <- lapply(.x[, .by, drop = FALSE], factor, exclude = c())
  splits <- split(x = .x, f = fact)
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

new_targets <- function(target, grid, cols, id) {
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
  lapply(as.character(na_omit(unique(plan[[name]]))), as.symbol)
}

splice_args <- function(x, replacements) {
  out <- splice_inner(x, replacements)
  # Avoid edge cases like #715
  out <- parse(text = safe_deparse(out)) # safe_deparse() is internal to drake.
  if (length(out)) {
    out <- out[[1]]
  }
  out
}

# From https://stackoverflow.com/a/54623901/3704549
splice_inner <- function(x, replacements) {
  if (is.call(x)) {
    as.call(
      do.call(
        "c",
        lapply(as.list(x), splice_inner, replacements),
        quote = TRUE
      )
    )
  } else if (is.name(x)) {
    nm <- deparse(x)
    if (nm %in% names(replacements)) {
      return(replacements[[nm]])
    } else {
      list(x)
    }
  } else {
    list(x)
  }
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

dsl_revdeps <- function(...) UseMethod("dsl_revdeps")

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

dsl_grid <- function(...) UseMethod("dsl_grid")

dsl_grid.map <- function(transform, groupings) {
  tryCatch(
    as.data.frame(groupings, stringsAsFactors = FALSE),
    error = function(e) {
      map_grid_error(transform, groupings)
    }
  )
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

dsl_grid.cross <- function(transform, groupings) {
  do.call(expand.grid, c(groupings, stringsAsFactors = FALSE))
}

groupings <- function(...) {
  UseMethod("groupings")
}

groupings.map <- groupings.cross <- function(transform) {
  c(new_groupings(transform), old_groupings(transform))
}

groupings.combine <- function(...) character(0)

old_groupings <- function(...) UseMethod("old_groupings")

old_groupings.map <- old_groupings.cross <- function(transform, plan = NULL) {
  attr(transform, "old_groupings") %|||%
    find_old_groupings(transform, plan)
}

set_old_groupings <- function(transform, plan) {
  attr(transform, "old_groupings") <- find_old_groupings(transform, plan)
  transform
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

na_omit <- function(x) {
  x[!is.na(x)]
}

find_old_groupings.combine <- function(transform, plan) NULL

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
  data_arg <- data_arg_groupings(transform[[".data"]])
  c(explicit, data_arg)
}

data_arg_groupings <- function(data_arg) {
  if (is.null(data_arg)) {
    return(list())
  }
  lapply(data_arg, function(x){
    x <- factor_to_character(x)
    vapply(x, safe_deparse, FUN.VALUE = character(1))
  })
}

dsl_all_special <- c(".id", ".tag_in", ".tag_out")

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
  paste(deparse(x), collapse = collapse)
}

dsl_combine <- function(...) UseMethod("dsl_combine")

dsl_combine.combine <- function(transform) {
  attr(transform, "combine") %|||%
    as.character(unnamed(lang(transform))[-1])
}

dsl_by <- function(...) UseMethod("dsl_by")

dsl_by.combine <- function(transform) {
  attr(transform, "by") %|||%
    all.vars(lang(transform)[[".by"]], functions = FALSE)
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

lang <- function(...) UseMethod("lang")

lang.command <- lang.transform <- function(x) x[[1]]

char <- function(...) UseMethod("char")

char.transform <- function(x) safe_deparse(lang(x))

named <- function(x, exclude = character(0)) {
  if (is.null(names(x))) return(NULL)
  x[!(names(x) %in% c("", exclude))]
}

unnamed <- function(x) {
  if (is.null(names(x))) return(x)
  x[!nzchar(names(x))]
}