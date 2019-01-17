#' @title Transform a plan
#' @description Take an existing `drake` plan with a `transform` column,
#'   and apply transformations that expand and gather targets.
#'   Use to generate large plans. `transform_plan()` is useful
#'   if you generated multiple plans with `drake_plan(transform = FALSE)`
#'   and and want to combine and transform them later.
#' @export
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
  transform <- parse(text = plan$transform[[row]])[[1]]
  transformer <- get(
    paste0("trf_", as.character(transform[[1]])),
    envir = getNamespace("drake")
  )
  out <- transformer(plan, plan$target[[row]], plan$command[[row]], transform)
  reappend <- setdiff(attr(plan, "protect"), c("target", "command"))
  for (col in reappend) {
    out[[col]] <- rep(unlist(plan[row, col], use.names = FALSE), nrow(out))
  }
  out[[plan$target[[row]]]] <- out$target
  if ("group" %in% colnames(plan) && !is.na(plan$group[[row]])) {
    out[[plan$group[[row]]]] <- out$target
  }
  out
}

# Supported transformations

trf_cross <- function(plan, target, command, transform) {
  levels <- trf_levels(plan, transform)
  trf_check_conflict(plan, c(target, names(levels)))
  grid <- trf_grid(plan, levels)
  suffixes <- grid[, names(levels)]
  targets <- apply(cbind(target, suffixes), 1, paste, collapse = "_")
  command <- gsub_grid(text = command, grid = grid)
  out <- weak_tibble(target = targets, command = command)
  cbind(out, grid)
}

trf_summarize <- function(plan, target, command, transform) {
  trf_check_conflict(plan, target)
  factors <- all.vars(transform)
  groups <- intersect(trf_cols(plan), all.vars(parse(text = command)))
  keep <- complete_cases(plan[, c("target", "command", factors, groups)])
  plan <- plan[keep, ]
  out <- map_by(
    .x = plan,
    .by = factors,
    .f = trf_group,
    command = command,
    groups = groups
  )
  suffixes <- cbind(target, out[, intersect(factors, colnames(out))])
  out$target <- apply(suffixes, 1, paste, collapse = "_")
  out
}

# Utils

trf_check_conflict <- function(plan, cols) {
  x <- intersect(attr(plan, "protect"), cols)
  if (length(x)) {
    stop(
      "variables in `target(transform = ...)` ",
      "cannot also be custom column names in the plan:\n",
      multiline_message(x),
      call. = FALSE
    )
  }
}

trf_cols <- function(plan) {
  setdiff(colnames(plan), attr(plan, "protect"))
}

trf_grid <- function(plan, levels) {
  args <- c(levels, stringsAsFactors = FALSE)
  grid <- do.call(what = expand.grid, args = args)
  if (length(trf_cols(plan))) {
    grid <- merge(grid, plan[, trf_cols(plan)])
  }
  grid
}

trf_group <- function(plan, command, groups) {
  for (group in groups) {
    levels <- unique(plan[[group]])
    levels <- paste(levels, levels, sep = " = ")
    levels <- paste(levels, collapse = ", ")
    command <- gsub(group, levels, command, fixed = TRUE)
  }
  data.frame(command = command, stringsAsFactors = FALSE)
}

trf_levels <- function(plan, transform) {
  transform <- transform[-1]
  names <- names(transform) %||% rep("", length(transform))
  out <- lapply(transform[nzchar(names)], function(x) {
    as.character(x)[-1]
  })
  planned <- vapply(
    transform[!nzchar(names)],
    as.character,
    FUN.VALUE = character(1)
  )
  for (factor in planned) {
    out[[factor]] <- as.character(na_omit(plan[[factor]]))
  }
  out
}
