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
row <- 3
tf_plan(plan)

}

# Iteration over the plan

tf_plan <- function(plan) {
  row <- 1
  attr(plan, "protect") <- colnames(plan)
  while(row < nrow(plan)) {
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
}

tf_row <- function(plan, row) {
  call <- parse(text = plan$transform[[row]])[[1]]
  transform <- get(
    paste0("tf_", as.character(call[[1]])),
    envir = getNamespace("drake")
  )
  transform(
    plan,
    plan$target[[row]],
    plan$command[[row]],
    tf_levels(plan, call)
  )
}

# Supported transformations

tf_cross <- function(plan, target, command, levels) {
  levels$stringsAsFactors <- FALSE
  factors <- tf_factors(plan, levels)
  targets <- apply(cbind(target, factors), 1, paste, collapse = "_")
  command <- gsub_grid(text = command, factors = factors)
  out <- weak_tibble(target = targets, command = command)
  out[[target]] <- targets
  cbind(out, factors)
}

# Utils

tf_levels <- function(plan, call) {
  out <- lapply(call[-1], as.character)
  factors <- names(out)
  if (!length(factors)) {
    factors <- rep("", length(out))
  }
  factors[!nzchar(factors)] <- unlist(lapply(out[!nzchar(factors)], `[[`, 1))
  names(out) <- factors
  out <- lapply(out, `[`, -1)
  for (factor in factors) {
    if (!length(out[[factor]])) {
      out$factor <- unique(na.omit(plan[[factor]]))
    }
  }
}

tf_cols <- function(plan) {
  setdiff(colnames(plan), attr(plan, "protect"))
}

tf_factors <- function(plan, levels) {
  factors <- do.call(what = expand.grid, args = levels)
  factors[, tf_cols(plan)] <- plan[, tf_cols(plan)]
  factors
}
