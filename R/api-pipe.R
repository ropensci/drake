split_dp_chain <- function (expr) {
  links <- list()
  i <- 1L
  while (is.call(expr) && identical(expr[[1L]], quote(`%dp%`))) {
    link <- expr[[3L]]
    if (is.symbol(link) || is.function(link)) {
      link <- as.call(list(link, quote(.)))
    } else if (!any(vapply(link[-1], identical, logical(1), quote(.)))) {
      link <- as.call(c(link[[1]], quote(.), as.list(link[-1])))
    }
    links[[i]] <- link
    expr <- expr[[2L]]
    i <- i + 1L
  }
  links[[i]] <- expr
  rev(links)
}

resolve_drake_pipe_row <- function(plan, row) {
  plan <- plan[row,, drop = FALSE] # nolint
  commands <- split_dp_chain(plan$command[[1]])
  if (length(commands) < 2L) {
    return(plan)
  }
  targets <- rev(make.unique(rep(plan$target[1], length(commands))))
  for (i in seq(2, length(targets))) {
    sub <- list(. = as.name(targets[i - 1]))
    commands[[i]] <- eval(
      call("substitute", commands[[i]], sub),
      envir = baseenv()
    )
  }
  out <- lapply(seq_along(targets), function(i) {
    subplan <- plan
    subplan$target <- targets[i]
    subplan$command[[1]] <- commands[[i]]
    subplan
  })
  do.call(rbind, out)
}

resolve_drake_pipe <- function(plan) {
  if (!nrow(plan)) {
    return(plan)
  }
  out <- lapply(seq_len(nrow(plan)), resolve_drake_pipe_row, plan = plan)
  drake_bind_rows(out)
}
