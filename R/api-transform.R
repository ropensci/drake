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

}

tf_plan <- function(plan) {
  row <- 1
  attr(plan, "protect") <- colnames(plan)
  while(row < nrow(plan)) {
    if (is.na(plan$transform[[row]])) {
      row <- row + 1
      next 
    }
    plan <- tf_row(plan, row)
  }
}

tf_row <- function(plan, row) {
  call <- parse(text = plan$transform[[row]])[[1]]
  transform <- get(
    paste0("tf_", as.character(call[[1]])),
    envir = getNamespace("drake")
  )
  transform(plan, plan$target[[row]], plan$command[[row]], tf_levels(call))
}

tf_levels <- function(call) {
  lapply(as.list(call[nzchar(names(call))]), function(x) as.character(x)[-1])
}

tf_cols <- function(plan) {
  setdiff(colnames(plan), attr(plan, "protect"))
}

tf_cross <- function(plan, target, command, levels) {
  levels$stringsAsFactors <- FALSE
  rules <- do.call(what = expand.grid, args = levels)
  rules[, tf_cols(plan)] <- plan[, tf_cols(plan)]
  gsub_grid(text = command, rules = rules)
}

gsub_grid <- function(text, rules) {
  browser()
  
  
}

