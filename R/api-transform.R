if (F) {

ld()
plan <- drake_plan(
  small = simulate(48),
  large = simulate(64),
  reg = target(
    reg_fun(data),
    transform = cross(reg_fun = c(reg1, reg2), data = c(small, large))
  ),
  summary = target(
    sum_fun(data, reg),
    transform = cross(sum_fun = c(coefficients, residuals), reg)
  ),
  winners = target(
    min(summary),
    transform = summarize(data, sum_fun)
  )
)
p2 <- tf_plan(plan)

config <- drake_config(p2[1:15, ])
vis_drake_graph(config)


}

# Iteration over the plan

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
      plan[seq(row + 1, nrow(plan)), ]
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
  browser()
  if (is.character(command)) {
    command <- parse(text = command)[[1]]
  }
  fun <- as.character(command[1])
  
  
  plan
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
    out[[factor]] <- as.character(stats::na.omit(plan[[factor]]))
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
