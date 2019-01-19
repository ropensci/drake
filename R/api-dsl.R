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
#'   summarize = target(
#'     summarize_analyses(analysis),
#'     transform = summarize()
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
    plan <- plan[, !protect]
  }
  old_cols(plan) <- plan$transform <- plan$group <- NULL
  plan
}

transform_row <- function(plan, row) {
  target <- plan$target[[row]]
  command <- dsl_parse_command(plan$command[[row]])
  transform <- dsl_parse_transform(plan$transform[[row]], plan)
  check_groupings(names(groupings(transform)), c(target, old_cols(plan)))
  out <- dsl_transform(transform, target, command, plan)
  out[[target]] <- out$target
  old_cols <- setdiff(
    old_cols(plan),
    c("target", "command", "transform", "group")
  )
  for (col in old_cols) {
    out[[col]] <- rep(plan[[col]][row], nrow(out))
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

dsl_parse_command <- function(command) UseMethod("dsl_parse_command")

dsl_parse_command.character <- function(command) {
  dsl_parse_command(parse(text = command)[[1]])
}

dsl_parse_command.language <- function(command) {
  structure(
    command,
    symbols = all.names(command),
    class = unique(c("command", class(command)))
  )
}

dsl_parse_command.call <-
  dsl_parse_command.expression <-
  dsl_parse_command.language

symbols <- function(command) UseMethod("symbols")

symbols.command <- function(command) {
  attr(command, "symbols")
}

dsl_parse_transform <- function(...) {
  UseMethod("dsl_parse_transform")
}

dsl_parse_transform.character <- function(transform, plan) {
  dsl_parse_transform(parse(text = transform)[[1]], plan)
}

dsl_parse_transform.call <- function(transform, plan) {
  structure(
    transform,
    class = unique(c(deparse(transform[[1]]), "transform", class(transform))),
    new_groupings = new_groupings(transform),
    old_groupings = old_groupings(transform, plan)
  )
}

new_groupings <- function(transform) UseMethod("new_groupings")

new_groupings.call <- function(transform) {
  lapply(named(as.list(transform)), function(x) {
    as.character(lapply(as.list(x)[-1], deparse))
  })
}

new_groupings.transform <- function(transform) {
  attr(transform, "new_groupings")
}

old_groupings <- function(...) UseMethod("old_groupings")

old_groupings.call <- function(transform, plan) {
  group_names <- as.character(unnamed(transform)[-1])
  group_names <- intersect(group_names, names(plan))
  lapply(plan[, group_names], function(x) {
    unique(na_omit(x))
  })
}

old_groupings.transform <- function(transform) {
  attr(transform, "old_groupings")
}

groupings <- function(...) {
  UseMethod("groupings")
}

groupings.call <- function(transform, plan) {
  c(new_groupings(transform), old_groupings(transform, plan))
}

groupings.transform <- function(transform) {
  c(new_groupings(transform), old_groupings(transform))
}

dsl_transform <- function(...) {
  UseMethod("dsl_transform")
}

dsl_transform.cross <- function(transform, target, command, plan) {
  groupings <- groupings(transform)
  grid <- do.call(
    what = expand.grid,
    args = c(groupings, stringsAsFactors = FALSE)
  )
  plan <- plan[, intersect(symbols(command), colnames(plan))]
    
  browser()
  
  # TODO: use the correct join.
  grid <- merge(grid, plan)

  
  new_targets <- dsl_new_targets(target, grid)
  new_commands <- dsl_new_commands(command, grid)
  out <- data.frame(
    target = new_targets,
    command = new_commands,
    stringsAsFactors = FALSE
  )
  cbind(out, grid)
}

dsl_transform.reduce <- function(transform, target, command, plan) {
  browser()
  groupings <- groupings(transform)
  groupings <- groupings[intersect(names(groupings), symbols(command))]
  
  factors <- all.vars(transform)
  groups <- dsl_cols(plan)
  groups <- groups[grepl_vector(dsl_cols(plan), command)]
  
  
  cols_keep <- c("target", "command", names(groupings), groups)
  
  keep <- complete_cases(plan[, c("target", "command", factors, groups)])
  plan <- plan[keep, ]
  out <- map_by(
    .x = plan,
    .by = names(groupings),
    .f = dsl_aggregate,
    command = command,
    groups = groups
  )
  suffixes <- cbind(target, out[, intersect(factors, colnames(out))])
  out$target <- apply(suffixes, 1, paste, collapse = "_")
  out
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

dsl_new_targets <- function(target, grid) {
  make.names(paste(apply(grid, 1, paste, collapse = "_"), sep = "_"))
}

dsl_new_commands <- function(command, grid) {
  grid <- grid[, intersect(symbols(command), colnames(grid))]
  if (any(dim(grid) < 1L)) {
    replicate(nrow(grid), command)
  }
  for (i in seq_along(grid)) {
    grid[[i]] <- rlang::syms(grid[[i]])
  }
  as.character(lapply(
    seq_len(nrow(grid)),
    dsl_new_command,
    command = command,
    grid = grid
  ))
}

dsl_new_command <- function(row, command, grid) {
  eval(call("substitute", command, unlist(grid[row, ])), envir = baseenv())
}
