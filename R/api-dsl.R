#' @title Experimental: transform a plan.
#' @description This feature has a lot of promise,
#'   but it is still experimental.
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
#' @export
#' @keywords experimental
#' @seealso [drake_plan()]
#' @return A transformed workflow plan data frame
#' @param plan Workflow plan data frame with a column for targets,
#'   a column for commands, and a column for transformations.
#' @param trace Logical, whether to add columns to show
#'   what happened during target transformations, e.g.
#'   `drake_plan(x = target(..., transform = ...), transform = TRUE)`.
#' @examples
#' plan1 <- drake_plan(
#'   analysis = target(
#'     analyze_data("source"),
#'     transform = cross(source = c(source1, source2))
#'   ),
#'   transform = FALSE
#' )
#' plan2 <- drake_plan(
#'   summaries = target(
#'     summarize_analyses(analysis),
#'     transform = reduce()
#'   ),
#'   transform = FALSE
#' )
#' plan <- bind_plans(plan1, plan2)
#' plan
#' transform_plan(plan)
#' transform_plan(plan, trace = TRUE)
transform_plan <- function(plan, trace = FALSE) {
  stopifnot("transform" %in% colnames(plan))
  row <- 1
  old_cols(plan) <- old_cols <- colnames(plan)
  while (row <= nrow(plan)) {
    if (is.na(plan$transform[[row]])) {
      row <- row + 1
      next
    }
    rows <- transform_row(plan, row)
    plan <- bind_plans(plan[seq_len(row - 1), ], rows, plan[-seq_len(row), ])
    old_cols(plan) <- old_cols
    row <- row + nrow(rows)
  }
  if (!trace) {
    plan <- plan[, intersect(colnames(plan), old_cols(plan)), drop = FALSE]
  }
  old_cols(plan) <- plan$transform <- plan$group <- NULL
  plan
}

transform_row <- function(plan, row) {
  target <- plan$target[[row]]
  command <- parse_command(plan$command[[row]])
  post_hoc_groups <- parse_group(plan[["group"]][[row]])
  transform <- parse_transform(plan$transform[[row]], plan)
  new_cols <- c(target, post_hoc_groups, group_names(transform))
  check_groupings(new_cols, old_cols(plan))
  out <- dsl_transform(transform, target, command, plan)
  out[[target]] <- out$target
  old_cols <- setdiff(
    old_cols(plan),
    c("target", "command", "transform", "group")
  )
  for (col in old_cols) {
    out[[col]] <- rep(plan[[col]][row], nrow(out))
  }
  for (col in post_hoc_groups) {
    out[[col]] <- out$target
  }
  out
}

dsl_transform.cross <- function(transform, target, command, plan) {
  groupings <- groupings(transform)
  if (!length(groupings)) {
    return(dsl_default_df(target, command))
  }
  grid <- do.call(expand.grid, c(groupings, stringsAsFactors = FALSE))
  ncl <- c(names(new_groupings(transform)), "target", "command", "transform")
  plan <- plan[, setdiff(colnames(plan), ncl), drop = FALSE]
  grid <- join_protect_x(grid, plan)
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
  make.names(paste(target, apply(grid, 1, paste, collapse = "_"), sep = "_"))
}

dsl_transform.reduce <- function(transform, target, command, plan) {
  command_symbols <- intersect(symbols(command), colnames(plan))
  keep <- complete_cases(plan[, command_symbols, drop = FALSE])
  if (!length(keep)) {
    return(dsl_default_df(target, command))
  }
  out <- map_by(
    .x = plan[keep, ],
    .by = group_names(transform),
    .f = reduction_step,
    command = command
  )
  grouping_symbols <- intersect(group_names(transform), colnames(plan))
  out$target <- new_targets(target, out[, grouping_symbols, drop = FALSE])
  out
}

reduction_step <- function(plan, command) {
  reductions <- lapply(plan, function(x) {
    names <- na_omit(unique(x))
    out <- rlang::syms(as.character(names))
    names(out) <- names
    out
  })
  command <- eval(
    call("substitute", lang(command), reductions),
    envir = baseenv()
  )
  data.frame(command = wide_deparse(command), stringsAsFactors = FALSE)
}

lang <- function(...) UseMethod("lang")

lang.command <- lang.transform <- function(x) x[[1]]

char <- function(...) UseMethod("char")

char.command <- char.transform <- function(x) wide_deparse(lang(x))

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

parse_transform <- function(...) {
  UseMethod("parse_transform")
}

parse_transform.character <- function(transform, plan) {
  parse_transform(parse(text = transform)[[1]], plan)
}

parse_transform.default <- function(transform, plan) {
  structure(
    as.expression(transform),
    class = unique(c(deparse(transform[[1]]), "transform", class(transform))),
    new_groupings = new_groupings(transform),
    old_groupings = old_groupings(transform, plan)
  )
}

new_groupings <- function(transform) UseMethod("new_groupings")

new_groupings.transform <- function(transform) {
  attr(transform, "new_groupings")
}

new_groupings.default <- function(code) {
  lapply(named(as.list(code)), function(x) {
    as.character(lapply(as.list(x)[-1], deparse))
  })
}

old_groupings <- function(...) UseMethod("old_groupings")

old_groupings.default <- function(code, plan) {
  group_names <- as.character(unnamed(code)[-1])
  group_names <- intersect(group_names, names(plan))
  lapply(plan[, group_names, drop = FALSE], function(x) {
    unique(na_omit(x))
  })
}

old_groupings.transform <- function(transform) {
  attr(transform, "old_groupings")
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

symbols <- function(...) UseMethod("symbols")

symbols.command <- function(x) {
  attr(x, "symbols")
}

symbols.group <- symbols.command

parse_group <- function(...) {
  UseMethod("parse_group")
}

parse_group.character <- function(group) {
  parse_group.default(parse(text = group))
}

parse_group.default <- function(group) {
  all.vars(group, functions = FALSE)
}

dsl_transform <- function(...) {
  UseMethod("dsl_transform")
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

check_groupings <- function(groups, protect) {
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
