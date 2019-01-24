#' @title Experimental: transform a plan.
#' @description This feature is experimental,
#'   and the `transform_plan()` function is not available to users.
#'   Read about the details at
#'   <https://ropenscilabs.github.io/drake-manual/plans.html#create-large-plans-the-easy-way> # nolint
#'   Please review your workflow with `vis_drake_graph()`
#'   before you run it.
#' @details The `transform_plan()` function
#'   take an existing `drake` plan and applies the transformations
#'   in the optional `"transform"` column, expanding and gathering
#'   targets to create a larger plan. Usually this is done
#'   inside `drake_plan(transform = TRUE)`, but
#'   `transform_plan()` on its own is useful
#'   if you generated multiple plans with `drake_plan(transform = FALSE)`
#'   and and want to combine and transform them later.
#' @keywords experimental internal
#' @seealso [drake_plan()]
#' @return A transformed workflow plan data frame
#' @param plan Workflow plan data frame with a column for targets,
#'   a column for commands, a column for transformations,
#'   and a column for optional grouping variables.
#' @param trace Logical, whether to add columns to show
#'   what happened during target transformations, e.g.
#'   `drake_plan(x = target(..., transform = ...), transform = TRUE)`.
transform_plan <- function(plan, trace = FALSE) {
  if (!("transform" %in% names(plan))) {
    return(plan)
  }
  old_cols(plan) <- old_cols <- colnames(plan)
  plan$transform <- lapply(plan$transform, parse_transform)
  while (any(index <- index_can_transform(plan))) {
    rows <- lapply(which(index), transform_row, plan = plan)
    plan <- sub_in_plan(plan, rows, at = which(index))
    old_cols(plan) <- old_cols
  }
  if (!trace) {
    keep <- as.character(intersect(colnames(plan), old_cols(plan)))
    plan <- plan[, intersect(colnames(plan), old_cols(plan)), drop = FALSE]
  }
  old_cols(plan) <- plan$transform <- plan$group <- NULL
  plan
}

index_can_transform <- function(plan) {
  vapply(plan$transform, can_transform, FUN.VALUE = logical(1), plan = plan)
}

can_transform <- function(transform, plan) {
  if (safe_is_na(transform)) {
    return(FALSE)
  }
  missing_groups <- setdiff(dsl_deps(transform), names(plan))
  length(missing_groups) < 1L
}

transform_row <- function(plan, row) {
  target <- plan$target[[row]]
  command <- parse_command(plan$command[[row]])
  transform <- set_old_groupings(plan$transform[[row]], plan)
  new_cols <- c(
    target,
    tag_in(transform),
    tag_out(transform),
    group_names(transform)
  )
  check_group_names(new_cols, old_cols(plan))
  out <- dsl_transform(transform, target, command, plan)
  out[[target]] <- out$target
  for (col in setdiff(old_cols(plan), c("target", "command", "transform"))) {
    out[[col]] <- rep(plan[[col]][row], nrow(out))
  }
  for (col in tag_in(transform)) {
    out[[col]] <- target
  }
  for (col in tag_out(transform)) {
    out[[col]] <- out$target
  }
  out
}

map_to_grid <- function(transform, target, command, plan) {
  groupings <- groupings(transform)
  if (!length(groupings)) {
    return(dsl_default_df(target, command))
  }
  grid <- dsl_grid(transform, groupings)
  ncl <- c(names(new_groupings(transform)), "target", "command", "transform")
  plan <- plan[, setdiff(colnames(plan), ncl), drop = FALSE]
  grid <- dsl_left_outer_join(grid, plan)
  suffix_cols <- intersect(colnames(grid), group_names(transform))
  new_targets <- new_targets(target, grid[, suffix_cols, drop = FALSE])
  new_commands <- grid_commands(command, grid)
  out <- data.frame(
    target = new_targets,
    command = new_commands,
    stringsAsFactors = FALSE
  )
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

grid_commands <- function(command, grid) {
  grid <- grid[, intersect(symbols(command), colnames(grid)), drop = FALSE]
  for (i in seq_along(grid)) {
    grid[[i]] <- dsl_syms(grid[[i]])
  }
  as.character(lapply(
    seq_len(nrow(grid)),
    grid_command,
    command = command,
    grid = grid
  ))
}

grid_command <- function(row, command, grid) {
  sub <- lapply(grid, `[[`, row)
  eval(call("substitute", lang(command), sub), envir = baseenv())
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

dsl_transform.combine <- function(transform, target, command, plan) {
  rows_keep <- complete_cases(plan[, dsl_by(transform), drop = FALSE])
  if (!length(rows_keep) || !any(rows_keep)) {
    return(dsl_default_df(target, command))
  }
  out <- map_by(
    .x = plan[rows_keep, ],
    .by = dsl_by(transform),
    .f = combine_step,
    command = command,
    transform = transform
  )
  out$target <- new_targets(target, out[, dsl_by(transform), drop = FALSE])
  out
}

combine_step <- function(plan, command, transform) {
  aggregates <- lapply(
    X = plan[, names(old_groupings(transform))],
    FUN = function(x) {
      unname(rlang::syms(as.character(na_omit(unique(x)))))
    }
  )
  command <- eval(
    call("substitute", lang(command), aggregates),
    envir = baseenv()
  )
  data.frame(command = safe_deparse(command), stringsAsFactors = FALSE)
}

lang <- function(...) UseMethod("lang")

lang.command <- lang.transform <- function(x) x[[1]]

char <- function(...) UseMethod("char")

char.command <- char.transform <- function(x) safe_deparse(lang(x))

old_cols <- function(plan) {
  attr(plan, "old_cols")
}

`old_cols<-` <-  function(plan, value) {
  attr(plan, "old_cols") <- value
  plan
}

parse_command <- function(command) UseMethod("parse_command")

parse_command.character <- function(command) {
  parse_command(parse(text = command))
}

parse_command.default <- function(command) {
  structure(
    as.expression(command),
    symbols = all.names(command),
    class = unique(c("command", class(command)))
  )
}

parse_transform <- function(transform) UseMethod("parse_transform")

parse_transform.character <- function(transform) {
  parse_transform(parse(text = transform)[[1]])
}

parse_transform.default <- function(transform) {
  if (safe_is_na(transform)) {
    return(NA)
  }
  if (is.character(transform)) {
    transform <- parse(text = transform)[[1]]
  }
  transform <- structure(
    as.expression(transform),
    class = unique(c(deparse(transform[[1]]), "transform", class(transform)))
  )
  assert_good_transform(transform)
  parse_transform(transform)
}

parse_transform.map <- parse_transform.cross <- function(transform) {
  structure(
    transform,
    deps = dsl_deps(transform),
    new_groupings = new_groupings(transform),
    tag_in = tag_in(transform),
    tag_out = tag_out(transform)
  )
}

parse_transform.combine <- function(transform) {
  transform <- structure(
    transform,
    new_groupings = new_groupings(transform),
    tag_in = tag_in(transform),
    tag_out = tag_out(transform),
    by = dsl_by(transform)
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

dsl_deps.default <- function(...) {
  character(0)
}

dsl_by <- function(...) UseMethod("dsl_by")

dsl_by.combine <- function(transform) {
  attr(transform, "by") %|||%
    all.vars(transform[[1]]$by, functions = FALSE)
}

dsl_by.default <- function(transform) {
  NULL
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

new_groupings.combine <- function(transform) {
  attr(transform, "new_groupings") %|||%
    find_new_groupings(
      lang(transform),
      exclude = c(".tag_in", ".tag_out", "by")
    )
}

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

old_groupings.transform <- function(transform, plan = NULL) {
  attr(transform, "old_groupings") %|||% find_old_groupings(transform, plan)
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

groupings.transform <- function(transform) {
  c(new_groupings(transform), old_groupings(transform))
}

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

symbols <- function(...) UseMethod("symbols")

symbols.command <- function(x) {
  attr(x, "symbols")
}

dsl_syms <- function(x) {
  out <- lapply(as.character(x), dsl_sym)
}

dsl_sym <- function(x) {
  tryCatch(
    eval(parse(text = x), envir = emptyenv()),
    error = function(e) {
      rlang::sym(x)
    }
  )
}

dsl_default_df <- function(target, command) {
  data.frame(
    target = target,
    command = char(command),
    stringsAsFactors = FALSE
  )
}

dsl_left_outer_join <- function(x, y) {
  by <- intersect(colnames(x), colnames(y))
  if (!length(by)) {
    return(x)
  }
  # The output must have the same number of rows as x.
  rows_keep <- complete_cases(y[, by])
  y <- y[rows_keep, ]
  dups <- duplicated(y[, by])
  if (any(dups)) {
    y <- y[!dups, ]
  }
  # Is merge() a performance bottleneck?
  # Need to profile.
  out <- merge(x = x, y = y, by = by, all.x = TRUE)
  out[, union(colnames(x), colnames(y))]
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
