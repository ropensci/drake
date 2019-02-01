transform_plan <- function(plan, envir, trace = FALSE) {
  if (!("transform" %in% names(plan))) {
    return(plan)
  }
  plan[["transform"]] <- tidyeval_exprs(plan[["transform"]], envir = envir)
  old_cols(plan) <- old_cols <- colnames(plan)
  plan[["transform"]] <- lapply(plan[["transform"]], parse_transform)
  while (any(index <- index_can_transform(plan))) {
    rows <- lapply(which(index), transform_row, plan = plan)
    plan <- sub_in_plan(plan, rows, at = which(index))
    old_cols(plan) <- old_cols
  }
  if (!trace) {
    keep <- as.character(intersect(colnames(plan), old_cols(plan)))
    plan <- plan[, intersect(colnames(plan), old_cols(plan)), drop = FALSE]
  }
  old_cols(plan) <- plan[["transform"]] <- NULL
  plan
}

transform_row <- function(plan, index) {
  row <- plan[index,, drop = FALSE]
  target <- row$target
  row$target <- NULL
  transform <- set_old_groupings(plan[["transform"]][[index]], plan)
  new_cols <- c(
    target,
    tag_in(transform),
    tag_out(transform),
    group_names(transform)
  )
  check_group_names(new_cols, old_cols(plan))
  out <- dsl_transform(transform, target, row, plan)
  out[[target]] <- out$target
  for (col in tag_in(transform)) {
    out[[col]] <- target
  }
  for (col in tag_out(transform)) {
    out[[col]] <- out$target
  }
  out
}

index_can_transform <- function(plan) {
  vapply(
    plan[["transform"]],
    can_transform,
    FUN.VALUE = logical(1),
    plan = plan
  )
}

can_transform <- function(transform, plan) {
  if (!inherits(transform, "transform")) {
    return(FALSE)
  }
  missing_groups <- setdiff(dsl_deps(transform), names(plan))
  length(missing_groups) < 1L
}

map_to_grid <- function(transform, target, row, plan) {
  groupings <- groupings(transform)
  if (!length(groupings)) NULL
  grid <- dsl_grid(transform, groupings)
  ncl <- c(names(new_groupings(transform)), old_cols(plan))
  plan <- plan[, setdiff(colnames(plan), ncl), drop = FALSE]
  grid <- dsl_left_outer_join(grid, plan)
  suffix_cols <- intersect(colnames(grid), group_names(transform))
  new_targets <- new_targets(target, grid[, suffix_cols, drop = FALSE])
  out <- data.frame(target = new_targets, stringsAsFactors = FALSE)
  for (col in setdiff(old_cols(plan), c("target", "transform"))) {
    out[[col]] <- grid_subs(row[[col]][[1]], grid)
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
  if (is.symbol(expr)) {
    expr <- as.call(c(quote(`{`), expr))
  }
  eval(call("substitute", expr, sub), envir = baseenv())
}

new_targets <- function(target, grid) {
  if (is.null(dim(grid)) || any(dim(grid) < 1L)) {
    return(target)
  }
  make.names(
    paste(target, apply(grid, 1, paste, collapse = "_"), sep = "_"),
    unique = TRUE
  )
}

dsl_transform <- function(...) {
  UseMethod("dsl_transform")
}

dsl_transform.cross <- dsl_transform.map <- map_to_grid

dsl_transform.combine <- function(transform, target, row, plan) {
  cols_keep <- union(dsl_by(transform), dsl_combine(transform))
  rows_keep <- complete_cases(plan[, cols_keep, drop = FALSE])
  if (!length(rows_keep) || !any(rows_keep)) {
    return(dsl_default_df(target, command))
  }
  out <- map_by(
    .x = plan[rows_keep,, drop = FALSE], # nolint
    .by = dsl_by(transform),
    .f = combine_step,
    row = row,
    transform = transform
  )
  out$target <- new_targets(target, out[, dsl_by(transform), drop = FALSE])
  out
}

combine_step <- function(plan, row, transform) {
  aggregates <- lapply(
    X = plan[, dsl_combine(transform), drop = FALSE],
    FUN = function(x) {
      unname(lapply(as.character(na_omit(unique(x))), as.symbol))
    }
  )
  out <- data.frame(command = NA, stringsAsFactors = FALSE)
  for (col in setdiff(old_cols(plan), c("target", "transform"))) {
    expr <- row[[col]][[1]]
    if (is.symbol(expr)) {
      expr <- as.call(c(quote(`{`), expr))
    }
    out[[col]] <- list(
      eval(call("substitute", expr, aggregates), envir = baseenv())
    )
  }
  out
}


grid_sub <- function(index, expr, grid) {
  sub <- lapply(grid, `[[`, index)
  if (is.symbol(expr)) {
    expr <- as.call(c(quote(`{`), expr))
  }
  eval(call("substitute", expr, sub), envir = baseenv())
}





lang <- function(...) UseMethod("lang")

lang.command <- lang.transform <- function(x) x[[1]]

char <- function(...) UseMethod("char")

char.transform <- function(x) safe_deparse(lang(x))

char.default <- function(x) safe_deparse(x)

old_cols <- function(plan) {
  attr(plan, "old_cols")
}

`old_cols<-` <-  function(plan, value) {
  attr(plan, "old_cols") <- value
  plan
}

parse_transform <- function(transform) {
  if (safe_is_na(transform)) {
    return(NA)
  }
  transform <- structure(
    as.expression(transform),
    class = unique(c(deparse(transform[[1]]), "transform", class(transform)))
  )
  assert_good_transform(transform)
  interpret_transform(transform)
}

interpret_transform <- function(transform) UseMethod("interpret_transform")

interpret_transform.map <- interpret_transform.cross <- function(transform) {
  structure(
    transform,
    deps = dsl_deps(transform),
    new_groupings = new_groupings(transform),
    tag_in = tag_in(transform),
    tag_out = tag_out(transform)
  )
}

interpret_transform.combine <- function(transform) {
  transform <- structure(
    transform,
    by = dsl_by(transform),
    combine = dsl_combine(transform),
    tag_in = tag_in(transform),
    tag_out = tag_out(transform)
  )
  structure(transform, dsl_deps = dsl_deps(transform))
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

dsl_deps.map <- dsl_deps.cross <- function(transform) {
  attr(transform, "deps") %|||%
    as.character(unnamed(as.list(transform[[1]][-1])))
}

dsl_deps.combine <- function(transform) {
  c(
    as.character(unnamed(transform[[1]][-1])),
    dsl_by(transform)
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

new_groupings <- function(transform) UseMethod("new_groupings")

new_groupings.map <- function(transform) {
  attr(transform, "new_groupings") %|||%
    find_new_groupings(
      lang(transform),
      exclude = c(".tag_in", ".tag_out")
    )
}

new_groupings.cross <- new_groupings.map

find_new_groupings <- function(code, exclude = character(0)) {
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

find_old_groupings <- function(transform, plan) {
  group_names <- as.character(unnamed(lang(transform))[-1])
  group_names <- intersect(group_names, names(plan))
  lapply(plan[, group_names, drop = FALSE], function(x) {
    unique(na_omit(x))
  })
}

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

dsl_default_df <- function(target, command) {
  out <- data.frame(target = target, stringsAsFactors = FALSE)
  out$command <- list(command)
  out
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
