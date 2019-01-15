# Iteration down the rows of the plan

tf_plan <- function(plan) {
  row <- 1
  attr(plan, "protect") <- colnames(plan)
  while (row <= nrow(plan)) {
    if (is.na(plan$transform[[row]])) {
      row <- row + 1
      next
    }
    transformed <- tf_row(plan, row)
    plan <- bind_plans(
      plan[seq_len(row - 1), ],
      transformed,
      plan[-seq_len(row), ]
    )
    row <- row + nrow(transformed)
  }
  out <- plan[, attr(plan, "protect")]
  attr(out, "protect") <- out$transform <- NULL
  out
}

tf_row <- function(plan, row) {
  transform <- parse(text = plan$transform[[row]])[[1]]
  transformer <- get(
    paste0("tf_", as.character(transform[[1]])),
    envir = getNamespace("drake")
  )
  transformer(plan, plan$target[[row]], plan$command[[row]], transform)
}

# Supported transformations

tf_cross <- function(plan, target, command, transform) {
  levels <- tf_levels(plan, transform)
  factors <- tf_factors(plan, levels)
  suffixes <- factors[, names(levels)]
  targets <- apply(cbind(target, suffixes), 1, paste, collapse = "_")
  command <- gsub_grid(text = command, factors = factors)
  out <- weak_tibble(target = targets, command = command)
  out[[target]] <- targets
  cbind(out, factors)
}

tf_summarize <- function(plan, target, command, transform) {
  if (is.character(command)) {
    command <- parse(text = command)[[1]]
  }
  fun <- as.character(command[1])
  factors <- as.character(transform[-1])
  groups <- as.character(command[-1])
  targets <- na_omit(unlist(as.list(plan[, groups]), use.names = FALSE))
  plan <- plan[plan$target %in% targets, ]
  out <- map_by(
    .x = plan,
    .by = factors,
    .f = gather_plan,
    target = target,
    gather = fun,
    append = FALSE
  )
  suffixes <- out[, c("target", intersect(factors, colnames(out)))]
  out$target <- apply(suffixes, 1, paste, collapse = "_")
  out
}

# Utils

tf_cols <- function(plan) {
  setdiff(colnames(plan), attr(plan, "protect"))
}

tf_levels <- function(plan, transform) {
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
    out[[factor]] <- as.character(stats::na_omit(plan[[factor]]))
  }
  out
}

tf_factors <- function(plan, levels) {
  args <- c(levels, stringsAsFactors = FALSE)
  factors <- do.call(what = expand.grid, args = args)
  if (length(tf_cols(plan))) {
    factors <- merge(factors, plan[, tf_cols(plan)])
  }
  factors
}
