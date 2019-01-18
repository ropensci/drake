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
    transformed <- trf_row(plan, row)
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

trf_row <- function(plan, row) {
  command <- plan$command[[row]]
  if (is.language(command)) {
    command <- wide_deparse(command)
  }
  transform <- plan$transform[[row]]
  if (is.character(transform)) {
    transform <- parse(text = transform)[[1]]
  }
  transformer <- get(
    paste0("trf_", as.character(transform[[1]])),
    envir = getNamespace("drake")
  )
  out <- transformer(plan, plan$target[[row]], command, transform)
  for (col in setdiff(attr(plan, "protect"), c("target", "command"))) {
    out[[col]] <- rep(plan[[col]][row], nrow(out))
  }
  out[[plan$target[[row]]]] <- out$target
  for (group in trf_parse_custom_groups(plan, row)) {
    out[[group]] <- out$target
  }
  out
}

trf_cross <- function(plan, target, command, transform) {
  levels <- trf_levels(plan, transform)
  trf_check_conflicts(plan, c(target, names(levels)))
  grid <- trf_grid(plan, levels)
  suffixes <- grid[, names(levels)]
  targets <- apply(cbind(target, suffixes), 1, paste, collapse = "_")
  relevant <- grepl_vector(names(grid), command)
  grid <- grid[, relevant, drop = FALSE]
  command <- gsub_grid(text = command, grid = grid)
  out <- weak_tibble(target = targets, command = command)
  cbind(out, grid)
}

trf_summarize <- function(plan, target, command, transform) {
  trf_check_conflicts(plan, target)
  factors <- all.vars(transform)
  groups <- trf_cols(plan)
  groups <- groups[grepl_vector(trf_cols(plan), command)]
  keep <- complete_cases(plan[, c("target", "command", factors, groups)])
  plan <- plan[keep, ]
  out <- map_by(
    .x = plan,
    .by = factors,
    .f = trf_aggregate,
    command = command,
    groups = groups
  )
  suffixes <- cbind(target, out[, intersect(factors, colnames(out))])
  out$target <- apply(suffixes, 1, paste, collapse = "_")
  out
}
