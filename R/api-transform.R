# Iteration down the rows of the plan

trf_plan <- function(plan) {
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
  out <- plan[, intersect(attr(plan, "protect"), colnames(plan))]
  attr(out, "protect") <- out$transform <- NULL
  out
}

trf_row <- function(plan, row) {
  transform <- parse(text = plan$transform[[row]])[[1]]
  transformer <- get(
    paste0("trf_", as.character(transform[[1]])),
    envir = getNamespace("drake")
  )
  transformer(plan, plan$target[[row]], plan$command[[row]], transform)
}

# Supported transformations

trf_cross <- function(plan, target, command, transform) {
  levels <- trf_levels(plan, transform)
  grid <- trf_grid(plan, levels)
  suffixes <- grid[, names(levels)]
  targets <- apply(cbind(target, suffixes), 1, paste, collapse = "_")
  command <- gsub_grid(text = command, grid = grid)
  out <- weak_tibble(target = targets, command = command)
  out[[target]] <- targets
  cbind(out, grid)
}

trf_summarize <- function(plan, target, command, transform) {
  factors <- all.vars(transform)
  groups <- intersect(trf_cols(plan), all.vars(parse(text = command)))
  keep <- complete.cases(plan[, c("target", "command", factors, groups)])
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
  out[[target]] <- out$target
  out
}

# Utils

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
    if (any(is.na(plan[[group]]))) {
      return(data.frame(stringsAsFactors = FALSE))
    }
    levels <- unique(plan[[group]])
    levels <- paste(levels, levels, sep = " = ")
    levels <- paste(levels, collapse = ", ")
    command <- gsub(group, levels, command)
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
