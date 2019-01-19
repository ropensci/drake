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
  attr(plan, "protect") <- protect <- colnames(plan)
  while (row <= nrow(plan)) {
    if (is.na(plan$transform[[row]])) {
      row <- row + 1
      next
    }
    transformed <- dsl_row(plan, row)
    plan <- bind_plans(
      plan[seq_len(row - 1), ],
      transformed,
      plan[-seq_len(row), ]
    )
    attr(plan, "protect") <- protect
    row <- row + nrow(transformed)
  }
  if (!trace) {
    plan <- plan[, intersect(attr(plan, "protect"), colnames(plan))]
  }
  attr(plan, "protect") <- plan$transform <- plan$group <- NULL
  plan
}

dsl_row <- function(plan, row) {
  target <- plan$target[[row]]
  command <- dsl_parse_command(plan$command[[row]])
  transform <- dsl_parse_transform(plan$transform[[row]])
  transform(plan, target, command, dsl_groupings(transform))

  
  
  out <- transformer(plan, plan$target[[row]], command, transform)
  for (col in setdiff(attr(plan, "protect"), c("target", "command"))) {
    out[[col]] <- rep(plan[[col]][row], nrow(out))
  }
  out[[plan$target[[row]]]] <- out$target
  for (group in dsl_parse_custom_groups(plan, row)) {
    out[[group]] <- out$target
  }
  out
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

dsl_parse_command.call <- dsl_parse_command.language

dsl_parse_transform <- function(transform) UseMethod("dsl_parse_transform")

dsl_parse_transform.character <- function(transform) {
  dsl_parse_transform(parse(text = transform)[[1]])
}

dsl_parse_transform.language <- function(transform) {
  structure(
    transform,
    class = unique(c(as.character(transform[1]), class(transform))),
    resolved_groupings = dsl_get_resolved_groupings(transform),
    unresolved_groupings = dsl_get_unresolved_groupings(transform)
  )
}

dsl_parse_transform.call <- dsl_parse_transform.language

dsl_get_resolved_groupings <- function(transform) {
  lapply(named(as.list(transform)), function(x) {
    lapply(as.list(x)[-1], deparse)
  })
}

dsl_get_unresolved_groupings <- function(transform) {
  as.character(unnamed(transform)[1])
}




dsl_cross <- function(plan, target, command, transform) {
  levels <- dsl_levels(plan, transform)
  dsl_check_conflicts(plan, c(target, names(levels)))
  grid <- dsl_grid(plan, levels)
  suffixes <- grid[, names(levels)]
  targets <- apply(cbind(target, suffixes), 1, paste, collapse = "_")
  relevant <- grepl_vector(names(grid), command)
  grid <- grid[, relevant, drop = FALSE]
  command <- gsub_grid(text = command, grid = grid)
  out <- weak_tibble(target = targets, command = command)
  cbind(out, grid)
}

dsl_summarize <- function(plan, target, command, transform) {
  dsl_check_conflicts(plan, target)
  factors <- all.vars(transform)
  groups <- dsl_cols(plan)
  groups <- groups[grepl_vector(dsl_cols(plan), command)]
  keep <- complete_cases(plan[, c("target", "command", factors, groups)])
  plan <- plan[keep, ]
  out <- map_by(
    .x = plan,
    .by = factors,
    .f = dsl_aggregate,
    command = command,
    groups = groups
  )
  suffixes <- cbind(target, out[, intersect(factors, colnames(out))])
  out$target <- apply(suffixes, 1, paste, collapse = "_")
  out
}
